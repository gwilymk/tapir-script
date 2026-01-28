use std::{
    collections::{BTreeSet, HashMap},
    mem,
};

use petgraph::{Direction, prelude::DiGraphMap};

use crate::{
    ast::{FunctionId, InternalOrExternalFunctionId, SymbolId},
    compile::{
        ir::{
            BlockExitInstr, BlockId, Phi, TapIr, TapIrBlock, TapIrFunction, TapIrFunctionBlockIter,
            optimisations::OptimisationResult,
        },
        symtab_visitor::SymTab,
    },
};

const MAX_BLOCKS_FOR_INLINE: usize = 5;

pub fn inline_small_functions(
    functions: &mut [TapIrFunction],
    symtab: &mut SymTab,
) -> OptimisationResult {
    let function_indexes = functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.id(), idx))
        .collect::<HashMap<_, _>>();

    // step 1. find inlinable functions.
    //
    // Recursive functions can't be inlined, so discount those. Otherwise, we can inline any
    // function that is relatively 'simple'. The heuristic for 'simple' is just the number of
    // blocks in that function. If it is less than MAX_BLOCKS_FOR_INLINE, it is probably worth inlining.
    //
    // Any function named 'black_box' is never allowed to be inlined (to allow for easier testing)
    let mut inlinable_functions = BTreeSet::new();
    for f in functions.iter() {
        if f.id().is_toplevel() {
            continue; // can't ever inline the toplevel (not that anything can call it)
        }

        if is_recursive(f) {
            continue;
        }

        if symtab.name_for_function(InternalOrExternalFunctionId::Internal(f.id())) == "black_box" {
            continue;
        }

        if f.blocks().count() <= MAX_BLOCKS_FOR_INLINE {
            inlinable_functions.insert(f.id());
        }
    }

    // step 2. Collect 'inline' points
    //
    // Collect every FunctionId where we can actually do an inline, and the function it intends to inline.
    // When we actually inline, we should only look at the leaves of this graph, to ensure that by inlining into
    // something, we don't then inline _that_ and create something that's way too big.
    let mut inlining_graph = DiGraphMap::new();
    for f in functions.iter() {
        for callee in f.direct_callees() {
            if inlinable_functions.contains(&callee) {
                inlining_graph.add_edge(f.id(), callee, ());
            }
        }
    }

    // These are all the inlinable functions (because to be a target you have to be inlinable) which themselves
    // don't contain any inlinable functions.
    let leaf_nodes = inlinable_functions
        .iter()
        .copied()
        .filter(|f| {
            inlining_graph
                .edges_directed(*f, petgraph::Direction::Outgoing)
                .count()
                == 0
        })
        .collect::<BTreeSet<_>>();

    struct InlineTarget {
        function_to_inline_into: FunctionId,
        function_to_be_inlined: FunctionId,
    }

    let inline_targets = leaf_nodes
        .iter()
        .copied()
        .flat_map(|function_to_be_inlined| {
            inlining_graph
                .neighbors_directed(function_to_be_inlined, Direction::Incoming)
                .map(move |function_to_inline_into| InlineTarget {
                    function_to_be_inlined,
                    function_to_inline_into,
                })
        });

    let mut did_something = OptimisationResult::DidNothing;

    // Step 3: actually inline functions
    for InlineTarget {
        function_to_be_inlined: function_id_to_be_inlined,
        function_to_inline_into: function_id_to_inline_into,
    } in inline_targets
    {
        let function_to_be_inlined_idx = *function_indexes.get(&function_id_to_be_inlined).unwrap();
        let function_to_inline_into_idx =
            *function_indexes.get(&function_id_to_inline_into).unwrap();

        let [function_to_be_inlined, function_to_inline_into] = functions
            .get_disjoint_mut([function_to_be_inlined_idx, function_to_inline_into_idx])
            .unwrap();

        loop {
            // Step 3a. Find a call to this function
            let Some((block_id, call_idx, args, rets)) =
                function_to_inline_into.blocks().find_map(|block| {
                    block.instrs().iter().enumerate().find_map(|(idx, instr)| {
                        if let TapIr::Call { f, target, args } = instr
                            && *f == function_id_to_be_inlined
                        {
                            Some((block.id(), idx, args.clone(), target.clone()))
                        } else {
                            None
                        }
                    })
                })
            else {
                // we could fail on the first attempt because it's actually spawned, not called.
                break;
            };

            did_something = OptimisationResult::DidSomething;

            // Step 3b. We need to introduce a jump to the inlined function so to do that we need to split this block
            // at the call instruction so we can add the required unconditional jump to the inlined function.
            let new_block_id = function_to_inline_into.next_block_id();
            let inlined_function_entrypoint = new_block_id + 1;
            let block_to_split = function_to_inline_into.block_mut(block_id).unwrap();

            // Put a jump to the new inlined function as the current split block's exit and take it's current
            // exit to put in the after_call block.
            let old_block_exit = mem::replace(
                &mut block_to_split.block_exit,
                BlockExitInstr::JumpToBlock(inlined_function_entrypoint),
            );

            let after_call = TapIrBlock {
                id: new_block_id,
                instrs: block_to_split.instrs.split_off(call_idx + 1),
                block_entry: vec![],
                block_exit: old_block_exit,
            };
            assert!(matches!(
                block_to_split.instrs.pop(),
                Some(TapIr::Call { .. })
            )); // remove the actual call instruction

            function_to_inline_into.insert_block(after_call);

            // Step 3c. Make a copy of the inlined function and insert it into the target function
            let inline_copy = create_copy(
                function_to_be_inlined,
                symtab,
                inlined_function_entrypoint,
                new_block_id,
                &rets,
                &args,
            );

            for block in inline_copy {
                function_to_inline_into.insert_block(block);
            }

            // Step 3d. Update phis referencing the old block
            // any references to the old block in phis should be updated to the new block, since this one now unconditionally jumps
            // to the inlined function
            let mut dfs = TapIrFunctionBlockIter::new_dfs(function_to_inline_into);
            while let Some(block) = dfs.next_mut(function_to_inline_into) {
                for phi in block.block_entry_mut() {
                    for (source_block, _) in &mut phi.sources {
                        if *source_block == block_id {
                            *source_block = new_block_id
                        }
                    }
                }
            }
        }
    }

    did_something
}

// Checks if a function directly calls itself. If it does that, then it
// is never worth inlining
fn is_recursive(f: &TapIrFunction) -> bool {
    f.callees().any(|callee| callee == f.id())
}

fn create_copy(
    f: &TapIrFunction,
    symtab: &mut SymTab,
    inlined_function_entrypoint: BlockId,
    block_to_continue: BlockId,
    targets: &[SymbolId],
    args: &[SymbolId],
) -> Box<[TapIrBlock]> {
    // We need to rename every single symbol to have new values inside the new function to maintain SSA
    let mut new_variables = HashMap::new();
    for (idx, &arg) in args.iter().enumerate() {
        new_variables.insert(f.arguments[idx], arg);
    }

    // There could be multiple return points in this inlined function, so we should create a final block
    // with phi functions here to collect them all up at the end.
    let mut return_phi = targets
        .iter()
        .map(|_| Phi {
            target: symtab.new_temporary(),
            sources: vec![],
        })
        .collect::<Vec<_>>();

    let new_block_ids = f
        .blocks()
        .enumerate()
        .map(|(idx, b)| (b.id(), inlined_function_entrypoint + idx))
        .collect::<HashMap<_, _>>();

    let return_entrypoint = inlined_function_entrypoint + f.blocks().count() + 1;

    let mut new_blocks = vec![];
    for block in f.blocks() {
        let mut new_block = block.clone();
        let new_block_id = *new_block_ids.get(&new_block.id).unwrap();
        new_block.id = new_block_id;

        for symbol in new_block.sources_mut() {
            *symbol = *new_variables
                .entry(*symbol)
                .or_insert_with_key(|id| symtab.new_rename(*id));
        }
        for symbol in new_block.targets_mut() {
            *symbol = *new_variables
                .entry(*symbol)
                .or_insert_with_key(|id| symtab.new_rename(*id));
        }

        macro_rules! replace_block_id {
            ($location:expr) => {
                *$location = *new_block_ids.get($location).unwrap()
            };
        }

        match new_block.block_exit_mut() {
            BlockExitInstr::JumpToBlock(block_id) => {
                replace_block_id!(block_id);
            }
            BlockExitInstr::ConditionalJump {
                if_true, if_false, ..
            } => {
                replace_block_id!(if_true);
                replace_block_id!(if_false);
            }
            BlockExitInstr::Return(targets) => {
                for (&target, phi) in targets.iter().zip(return_phi.iter_mut()) {
                    phi.sources.push((new_block_id, target));
                }

                new_block.block_exit = BlockExitInstr::JumpToBlock(return_entrypoint);
            }
        }

        for phi in new_block.block_entry_mut() {
            phi.sources.retain_mut(|(block_id, _)| {
                if let Some(new_block_id) = new_block_ids.get(block_id) {
                    *block_id = *new_block_id;
                    true
                } else {
                    false
                }
            });
        }

        new_blocks.push(new_block);
    }

    let instrs = targets
        .iter()
        .enumerate()
        .map(|(index, &target)| TapIr::Move {
            target,
            source: return_phi[index].target,
        })
        .collect();

    let return_block = TapIrBlock {
        block_entry: return_phi,
        id: return_entrypoint,
        instrs,
        block_exit: BlockExitInstr::JumpToBlock(block_to_continue),
    };

    new_blocks.push(return_block);

    new_blocks.into_boxed_slice()
}
