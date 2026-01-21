use std::collections::HashMap;

use crate::{
    ast::SymbolId,
    compile::ir::{
        BlockExitInstr, Constant, TapIr, TapIrFunction, TapIrFunctionBlockIter,
        optimisations::OptimisationResult,
    },
};

pub fn remove_constant_conditionals(f: &mut TapIrFunction) -> OptimisationResult {
    let mut did_something = OptimisationResult::DidNothing;

    let mut constants: HashMap<SymbolId, bool> = HashMap::new();

    let mut reverse_post_order = TapIrFunctionBlockIter::new_reverse_post_order(f);
    while let Some(block) = reverse_post_order.next_mut(f) {
        for instr in block.instrs() {
            if let TapIr::Constant(target, Constant::Bool(value)) = *instr {
                constants.insert(target, value);
            }
        }

        if let BlockExitInstr::ConditionalJump {
            test,
            if_true,
            if_false,
        } = block.block_exit()
            && let Some(value) = constants.get(test)
        {
            let target_block = if *value { *if_true } else { *if_false };

            did_something = OptimisationResult::DidSomething;
            *block.block_exit_mut() = BlockExitInstr::JumpToBlock(target_block);
        }
    }

    did_something
}
