use std::collections::HashSet;

use crate::compile::ir::{
    TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
};

/// Removes every instruction which stores but doesn't ultimately use the result
pub fn remove_dead_stores(f: &mut TapIrFunction) -> OptimisationResult {
    let used_symbols = f.blocks().flat_map(|b| b.sources()).collect::<HashSet<_>>();

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        block.block_entry_retain_mut(|phi| {
            if !used_symbols.contains(&phi.target) {
                did_something = OptimisationResult::DidSomething;
                return false;
            }

            true
        });

        block.instrs_retain(|instr| {
            if instr.could_have_side_effects() {
                return true;
            }

            if instr.targets().any(|target| used_symbols.contains(&target)) {
                return true;
            }

            did_something = OptimisationResult::DidSomething;
            false
        });
    }

    did_something
}
