use std::collections::{BTreeSet, HashMap};

use crate::{
    ast::SymbolId,
    compile::ir::{BlockId, TapIr, TapIrFunction, TapIrFunctionBlockIter, TapIrInstr},
};

struct RegAllocator {
    register_allocations: HashMap<SymbolId, RegisterId>,
    available_registers: BTreeSet<RegisterId>,
}

impl RegAllocator {
    fn new(function: &TapIrFunction) -> Self {
        let register_allocations = function
            .arguments()
            .iter()
            .enumerate()
            .map(|(i, arg)| (*arg, RegisterId(i as u8 + 1)))
            .collect();

        Self {
            register_allocations,
            available_registers: ((function.arguments().len() as u8 + 1)..255)
                .map(RegisterId)
                .collect::<BTreeSet<_>>(),
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

    fn write_symbol(&mut self, symbol: SymbolId) {
        if let Some(register) = self.register_allocations.get(&symbol) {
            self.available_registers.insert(*register);
        } else {
            self.register_allocations.insert(
                symbol,
                *self
                    .available_registers
                    .first()
                    .expect("Ran out of registers"),
            );
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

        // we need to insert these phony reads in because in the final phase we'll add fake
        // move instructions here (breaking SSA form) which will read from this variable. So
        // we need to ensure that it stays alive until that point, otherwise the register
        // could get reused.
        if let Some(phony_reads) = phony_reads.get(&block.id()) {
            for phony_read in phony_reads {
                register_allocator.read_symbol(phony_read.source);
            }
        }

        for instr in block.instrs().iter().rev() {
            for source in instr.sources() {
                register_allocator.read_symbol(source);
            }

            for target in instr.targets() {
                register_allocator.write_symbol(target);
            }
        }

        for phi in block.block_entry().iter().rev() {
            let target = phi.target;

            register_allocator.write_symbol(target);

            for &(_, source) in &phi.sources {
                register_allocator.read_symbol(source);
            }
        }

        block.remove_block_entries();
    }

    for (target_block, moves) in phony_reads {
        let block = function
            .block_mut(target_block)
            .expect("Found a block, so should have it");

        for PhonyRead { target, source } in moves {
            block.instrs.push(TapIr {
                instr: TapIrInstr::Move { source, target },
            });
        }
    }

    register_allocator.allocations()
}

struct PhonyRead {
    source: SymbolId,
    target: SymbolId,
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
                    });
            }
        }
    }

    phony_reads
}
