use std::collections::HashMap;

use crate::compile::ir::{
    BlockExitInstr, TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
};

/// It is quite common for blocks to be generated which contain no instructions and only contain an
/// unconditional jump to another block. This optimisation removes those blocks and redirects any
/// blocks which jump to this one to instead jump to this one's target.
///
/// Note that this doesn't actually remove the blocks themselves from the function. Removing unreferenced
/// blocks is handled by a separate optimisation step.
pub fn remove_empty_blocks(f: &mut TapIrFunction) -> OptimisationResult {
    // We have to do 2 passes here. Because we don't know if a target of a jump will be empty.
    // This would be possible with a reverse post order iteration if it wasn't a graph that we're working with.
    // Because it's a graph, we don't necessarily see every single child before the parent.

    // Maps the removed block to what the new exit instruction should be
    let mut empty_blocks = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);

    while let Some(block) = dfs.next(f) {
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

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);

    let mut did_something = OptimisationResult::DidNothing;
    while let Some(block) = dfs.next_mut(f) {
        let exit = block.block_exit_mut();

        match exit {
            BlockExitInstr::JumpToBlock(block_id) => {
                if let Some(new_exit) = empty_blocks.get(block_id) {
                    // Unconditional jumps can be replaced unconditionally with what the empty block was doing
                    *exit = new_exit.clone();

                    did_something = OptimisationResult::DidSomething;
                };
            }
            BlockExitInstr::ConditionalJump {
                if_true, if_false, ..
            } => {
                // Conditional jumps can only be replaced if the target wasn't doing its own conditional jump
                // or its own return.

                if let Some(BlockExitInstr::JumpToBlock(new_target)) = empty_blocks.get(if_true) {
                    *if_true = *new_target;
                    did_something = OptimisationResult::DidSomething;
                }

                if let Some(BlockExitInstr::JumpToBlock(new_target)) = empty_blocks.get(if_false) {
                    *if_false = *new_target;
                    did_something = OptimisationResult::DidSomething;
                }
            }
            BlockExitInstr::Return(_) => {}
        };
    }

    did_something
}
