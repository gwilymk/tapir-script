use std::collections::HashMap;

use crate::{
    ast::SymbolId,
    compile::ir::{
        TapIrFunction, TapIrFunctionBlockIter, TapIrInstr, optimisations::OptimisationResult,
    },
};

/// Finds all the moves copies the target of the move wherever possible.
///
/// A subsequent optimisation pass removes the dead stores that result from this pass.
pub fn copy_propagation(f: &mut TapIrFunction) -> OptimisationResult {
    let mut renames: HashMap<SymbolId, SymbolId> = HashMap::new();

    let mut did_something = OptimisationResult::DidNothing;

    macro_rules! replace_source {
        ($source:ident) => {
            if let Some(new_source) = renames.get($source) {
                *$source = *new_source;
                did_something = OptimisationResult::DidSomething;
            }
        };
    }

    // reverse post order is the best order to do this, but this will require multiple passes anyway
    let mut dfs = TapIrFunctionBlockIter::new_reverse_post_order(f);
    while let Some(block) = dfs.next_mut(f) {
        for phi in block.block_entry_mut() {
            for (_, source) in &mut phi.sources {
                replace_source!(source);
            }
        }

        for instr in block.instrs_mut() {
            for source in instr.sources_mut() {
                replace_source!(source);
            }

            if let TapIrInstr::Move { source, target } = instr.instr {
                if source != target && renames.insert(target, source).is_some() {
                    panic!(
                        "Should be in SSA so there should only ever be one definition of source"
                    );
                }
            }
        }

        for source in block.block_exit_mut().sources_mut() {
            replace_source!(source);
        }
    }

    did_something
}
