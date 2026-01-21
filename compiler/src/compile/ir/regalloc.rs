use std::collections::{BTreeSet, HashMap, HashSet};

use petgraph::algo::dominators::{self, Dominators};

use crate::{
    ast::SymbolId,
    compile::ir::{
        BlockExitInstr, BlockId, TapIr, TapIrBlock, TapIrFunction, TapIrFunctionBlockIter,
    },
};

struct RegAllocator {
    register_allocations: HashMap<SymbolId, RegisterId>,
    available_registers: BTreeSet<RegisterId>,

    // Sometimes a write happens in a block which is part of a loop. In that case, we need to ensure that
    // the write is kept until we're outside of that loop.
    dominators: Dominators<BlockId>,
    phony_writes: HashMap<BlockId, Vec<SymbolId>>,

    // Blocks that are inside a loop (dominated by a loop header).
    // Used to determine when we need to propagate writes vs when we can free registers.
    blocks_in_loops: HashSet<BlockId>,
}

impl RegAllocator {
    fn new(function: &TapIrFunction) -> Self {
        let register_allocations = function
            .arguments()
            .iter()
            .enumerate()
            .map(|(i, arg)| (*arg, RegisterId(i as u8 + 1)))
            .collect();

        let dominators = dominators::simple_fast(function, function.root);
        let blocks_in_loops = find_blocks_in_loops(function, &dominators);

        Self {
            register_allocations,
            available_registers: ((function.arguments().len() as u8 + 1)..255)
                .map(RegisterId)
                .collect::<BTreeSet<_>>(),

            dominators,
            phony_writes: HashMap::new(),
            blocks_in_loops,
        }
    }

    fn start_block(&mut self, block_id: BlockId) {
        if let Some(this_block_writes) = self.phony_writes.remove(&block_id) {
            for symbol in this_block_writes {
                self.write_symbol(symbol, block_id);
            }
        }
    }

    fn read_symbol(&mut self, symbol: SymbolId) {
        self.register_allocations.entry(symbol).or_insert_with(|| {
            // this is the last read
            self.available_registers
                .pop_first()
                .expect("Ran out of registers")
        });
    }

    fn write_symbol(&mut self, symbol: SymbolId, block_id: BlockId) {
        if self.blocks_in_loops.contains(&block_id) {
            // In a loop - propagate write to dominator to keep register alive
            let idom = self
                .dominators
                .immediate_dominator(block_id)
                .expect("Block in loop must have a dominator");
            self.phony_writes.entry(idom).or_default().push(symbol);
            self.read_symbol(symbol);
        } else if let Some(register) = self.register_allocations.get(&symbol) {
            // Not in a loop, already allocated - safe to free the register
            self.available_registers.insert(*register);
        } else {
            // Not in a loop, not allocated - just allocate
            self.read_symbol(symbol);
        }
    }

    fn allocations(self) -> RegisterAllocations {
        let last_used_register = self
            .register_allocations
            .values()
            .max()
            .copied()
            .unwrap_or(RegisterId(0));

        let first_free_register = RegisterId(last_used_register.0 + 1);

        RegisterAllocations {
            allocations: self.register_allocations,
            first_free_register,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegisterId(pub u8);

pub struct RegisterAllocations {
    allocations: HashMap<SymbolId, RegisterId>,
    first_free_register: RegisterId,
}

impl RegisterAllocations {
    pub fn register_for_symbol(&self, symbol: SymbolId) -> RegisterId {
        *self
            .allocations
            .get(&symbol)
            .expect("Should've done all the register allocation")
    }

    pub fn first_free_register(&self) -> RegisterId {
        self.first_free_register
    }
}

/// The register allocator will do the wrong thing if the function you pass here isn't in
/// SSA form.
pub fn allocate_registers(function: &mut TapIrFunction) -> RegisterAllocations {
    let mut register_allocator = RegAllocator::new(function);

    let phony_reads = get_phony_reads(function);

    let mut post_order = TapIrFunctionBlockIter::new_post_order(function);
    while let Some(block) = post_order.next_mut(function) {
        let block_exit = block.block_exit();

        match block_exit {
            crate::compile::ir::BlockExitInstr::JumpToBlock(_) => {}
            crate::compile::ir::BlockExitInstr::ConditionalJump { test, .. } => {
                register_allocator.read_symbol(*test);
            }
            crate::compile::ir::BlockExitInstr::Return(symbol_ids) => {
                for symbol_id in symbol_ids {
                    register_allocator.read_symbol(*symbol_id);
                }
            }
        }

        for instr in block.instrs().iter().rev() {
            for source in instr.sources() {
                register_allocator.read_symbol(source);
            }

            for target in instr.targets() {
                register_allocator.write_symbol(target, block.id());
            }
        }

        for phi in block.block_entry().iter().rev() {
            let target = phi.target;

            for &(_, source) in &phi.sources {
                register_allocator.read_symbol(source);
            }

            register_allocator.write_symbol(target, block.id());
        }

        register_allocator.start_block(block.id());
        block.remove_block_entries();
    }

    for (target_block, moves) in phony_reads {
        let next_block_id = function.next_block_id();

        let block = function
            .block_mut(target_block)
            .expect("Found a block, so should have it");

        // ideally these phony reads should happen _after_ the jump, but we can't
        // put instructions there.
        //
        // If it's an unconditional jump, then we can safely insert these reads
        // into it. However, if it is a conditional jump then we need to put
        // only do these additional moves after the conditional jump. So we do
        // this by putting in new blocks.

        if let BlockExitInstr::ConditionalJump {
            if_true, if_false, ..
        } = block.block_exit_mut()
        {
            let (true_phony_reads, false_phony_reads): (Vec<_>, Vec<_>) = moves
                .into_iter()
                .partition(|read| read.target_block == *if_true);

            let true_block_id = next_block_id;
            let false_block_id = true_block_id.next();

            let true_block = TapIrBlock {
                id: true_block_id,
                instrs: true_phony_reads
                    .iter()
                    .map(|read| read.as_tapir())
                    .collect(),
                block_entry: vec![],
                block_exit: BlockExitInstr::JumpToBlock(*if_true),
            };

            let false_block = TapIrBlock {
                id: false_block_id,
                instrs: false_phony_reads
                    .iter()
                    .map(|read| read.as_tapir())
                    .collect(),
                block_entry: vec![],
                block_exit: BlockExitInstr::JumpToBlock(*if_false),
            };

            *if_true = true_block_id;
            *if_false = false_block_id;

            function.insert_block(true_block);
            function.insert_block(false_block);
        } else {
            for phony in moves {
                block.instrs.push(phony.as_tapir());
            }
        }
    }

    register_allocator.allocations()
}

struct PhonyRead {
    source: SymbolId,
    target: SymbolId,
    target_block: BlockId,
}

/// Collect a list of moves which will be needed to make the phi functions line up correctly
fn get_phony_reads(function: &mut TapIrFunction) -> HashMap<BlockId, Vec<PhonyRead>> {
    let mut phony_reads: HashMap<BlockId, Vec<PhonyRead>> = HashMap::new();
    for block in function.blocks() {
        for phi in block.block_entry() {
            for &(source_block, source) in &phi.sources {
                phony_reads
                    .entry(source_block)
                    .or_default()
                    .push(PhonyRead {
                        source,
                        target: phi.target,
                        target_block: block.id(),
                    });
            }
        }
    }

    phony_reads
}

impl PhonyRead {
    fn as_tapir(&self) -> TapIr {
        TapIr::Move {
            target: self.target,
            source: self.source,
        }
    }
}

fn find_blocks_in_loops(
    function: &TapIrFunction,
    dominators: &Dominators<BlockId>,
) -> HashSet<BlockId> {
    use petgraph::visit::IntoNeighbors;

    // Build predecessor map (reverse of the successor/neighbor relationship)
    let mut predecessors: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block in function.blocks() {
        for successor in function.neighbors(block.id()) {
            predecessors.entry(successor).or_default().push(block.id());
        }
    }

    // Find all blocks that are inside loops.
    // A block is in a loop if it's dominated by a loop header.
    // A loop header is a block that has a predecessor it dominates (back edge).
    let mut loop_headers: HashSet<BlockId> = HashSet::new();
    for block in function.blocks() {
        let block_id = block.id();
        for pred in predecessors.get(&block_id).into_iter().flatten() {
            // If this block dominates its predecessor, there's a back edge
            // and this block is a loop header
            if dominators
                .dominators(*pred)
                .is_some_and(|mut doms| doms.any(|d| d == block_id))
            {
                loop_headers.insert(block_id);
                break;
            }
        }
    }

    // A block is "in a loop" if any of its dominators is a loop header
    let mut blocks_in_loops: HashSet<BlockId> = HashSet::new();
    for block in function.blocks() {
        if let Some(doms) = dominators.dominators(block.id()) {
            for dom in doms {
                if loop_headers.contains(&dom) {
                    blocks_in_loops.insert(block.id());
                    break;
                }
            }
        }
    }
    blocks_in_loops
}
