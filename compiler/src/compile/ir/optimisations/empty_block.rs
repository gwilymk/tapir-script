use std::collections::{HashMap, HashSet};

use crate::compile::ir::{
    BlockExitInstr, BlockId, TapIrFunction, TapIrFunctionBlockIter,
    optimisations::OptimisationResult,
};

/// It is quite common for blocks to be generated which contain no instructions and only contain an
/// unconditional jump to another block. This optimisation removes those blocks and redirects any
/// blocks which jump to this one to instead jump to this one's target.
///
/// Note that this doesn't actually remove the blocks themselves from the function. Removing unreferenced
/// blocks is handled by a separate optimisation step.
pub fn remove_empty_blocks(f: &mut TapIrFunction) -> OptimisationResult {
    // We have to do 3 passes here. Because we don't know if a target of a jump will be empty.
    // This would be possible with a reverse post order iteration if it wasn't a graph that we're working with.
    // Because it's a graph, we don't necessarily see every single child before the parent.

    // Maps the removed block to what the new exit instruction should be
    let mut empty_blocks = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next(f) {
        if block.id() == f.root {
            continue; // we should never try to remove the root node
        }

        if !block.instrs().is_empty() {
            continue; // there is actually something here
        }

        if !block.block_entry().is_empty() {
            // TODO: What should we do about non-empty phis? Copying them only works if
            // there is only 1 predecessor?
            continue;
        }

        empty_blocks.insert(block.id(), block.block_exit().clone());
    }

    if empty_blocks.is_empty() {
        return OptimisationResult::DidNothing;
    }

    // If we do a replacement, then we need to add a new phi variable to the targets if there was a way in
    // because we would add additional moves at register allocation time.

    // This maps blocks -> list of new blocks that are now a parent for this block
    let mut replacements: HashMap<BlockId, HashSet<BlockId>> = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        let this_block_id = block.id();
        let exit = block.block_exit_mut();

        macro_rules! add_replacement {
            ($replacement:expr) => {
                replacements
                    .entry($replacement)
                    .or_default()
                    .insert(this_block_id);
            };
        }

        match exit {
            BlockExitInstr::JumpToBlock(block_id) => {
                let block_id = *block_id;
                if let Some(new_exit) = empty_blocks.get(&block_id) {
                    // Only redirect if it actually changes the exit instruction
                    if *exit != *new_exit {
                        add_replacement!(block_id);

                        // Unconditional jumps can be replaced unconditionally with what the empty block was doing
                        *exit = new_exit.clone();
                    }
                };
            }
            BlockExitInstr::ConditionalJump {
                if_true, if_false, ..
            } => {
                // Conditional jumps can only be replaced if the target wasn't doing its own conditional jump
                // or its own return.
                let new_true_target = if let Some(BlockExitInstr::JumpToBlock(new_target)) =
                    empty_blocks.get(if_true)
                {
                    Some(*new_target)
                } else {
                    None
                };

                let new_false_target = if let Some(BlockExitInstr::JumpToBlock(new_target)) =
                    empty_blocks.get(if_false)
                {
                    Some(*new_target)
                } else {
                    None
                };

                // Don't collapse if both branches would end up at the same destination.
                // This would corrupt phi nodes that distinguish between the two branches,
                // since both phi sources would then appear to come from the same block.
                let would_merge_to_same = match (new_true_target, new_false_target) {
                    (Some(t), Some(f)) => t == f,
                    (Some(t), None) => t == *if_false,
                    (None, Some(f)) => *if_true == f,
                    (None, None) => false,
                };

                if would_merge_to_same {
                    // Skip this optimization to preserve phi semantics
                    continue;
                }

                if let Some(new_target) = new_true_target
                    && if_true != &new_target
                {
                    add_replacement!(*if_true);
                    *if_true = new_target;
                }

                if let Some(new_target) = new_false_target
                    && if_false != &new_target
                {
                    add_replacement!(*if_false);
                    *if_false = new_target;
                }
            }
            BlockExitInstr::Return(_) => {}
        };
    }

    if replacements.is_empty() {
        return OptimisationResult::DidNothing;
    }

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        for phi in block.block_entry_mut() {
            let mut new_phi_variables = vec![];

            for &(source_block, symbol) in &phi.sources {
                let Some(replacement) = replacements.get(&source_block) else {
                    continue;
                };

                new_phi_variables
                    .extend(replacement.iter().map(|replacement| (*replacement, symbol)));
            }

            phi.sources.extend(new_phi_variables);
        }
    }

    OptimisationResult::DidSomething
}
