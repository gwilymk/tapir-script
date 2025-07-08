use std::collections::{BTreeSet, HashMap, HashSet};

use petgraph::visit::DfsPostOrder;

use crate::{ast::SymbolId, compile::ir::TapIrFunction};

struct RegAllocator {}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct RegisterId(u8);

// https://graal.ens-lyon.fr/~pkchouha/presentation/ssa/ssaf.pdf

impl RegAllocator {
    /// The register allocator will do the wrong thing if the function you pass here isn't in
    /// SSA form.
    pub fn new(function: &TapIrFunction) -> Self {
        let mut post_order = DfsPostOrder::new(function, function.root);

        let mut register_allocations: HashMap<SymbolId, RegisterId> = HashMap::new();

        let mut available_registers = (1..255).map(RegisterId).collect::<BTreeSet<_>>();

        while let Some(block_id) = post_order.next(function) {
            let block = function
                .block(block_id)
                .expect("Should have a block if we found it");

            let block_exit = block.block_exit();

            match block_exit {
                crate::compile::ir::BlockExitInstr::JumpToBlock(_) => {}
                crate::compile::ir::BlockExitInstr::ConditionalJump { test, .. } => {
                    if !register_allocations.contains_key(test) {
                        // this is the last read
                        let reg = available_registers
                            .pop_first()
                            .expect("Ran out of registers");

                        register_allocations.insert(*test, reg);
                    }
                }
                crate::compile::ir::BlockExitInstr::Return(symbol_ids) => todo!(),
            }

            for instr in block.instrs().iter().rev() {
                instr.sources();
                instr.targets();
            }

            for phi in block.block_entry() {}
        }

        todo!()
    }
}

/// y = 0
/// x = 0
/// loop {
///     y = x + y
///     x = read
///     y = x + y
///     x = 6
///     y = x + y
/// }
///
/// SSA
///
/// y0 = 0
/// x0 = 0
/// loop {
///     y4 = phi(y0, y3)
///     x3 = phi(x0, x2)
///     y1 = y4 + x3
///     x1 = read
///     y2 = x1 + y1
///     x2 = 6
///     y3 = x2 + y2
/// }
///
/// Optimise
///
/// y0 = 0
/// x0 = 0
/// x2 = 6
/// loop {
///     y4 = phi(y0, y3)
///     x3 = phi(x0 if came from not loop, x2 if came from loop)
///     y1 = y4 + x3
///     x1 = read
///     y2 = x1 + y1
///     y3 = x2 + y2
/// }
fn _a() {}
