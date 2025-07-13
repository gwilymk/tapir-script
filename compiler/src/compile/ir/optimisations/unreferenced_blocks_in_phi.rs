use std::collections::HashSet;

use crate::compile::ir::{
    TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
};

/// Some optimisations will remove blocks. Once this is done, we should also remove all the references
/// to those blocks in phis so later optimisations can remove those phis.
///
/// Also removes duplicate phis because we're iterating through them at this point
pub fn remove_unreferenced_blocks_in_phi(f: &mut TapIrFunction) -> OptimisationResult {
    let referenced_blocks = f.blocks().map(|block| block.id()).collect::<HashSet<_>>();

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        for phi in block.block_entry_mut() {
            let mut seen_sources = HashSet::new();

            phi.sources.retain(|(block, symbol)| {
                if !seen_sources.insert((*block, *symbol)) || !referenced_blocks.contains(block) {
                    did_something = OptimisationResult::DidSomething;
                    return false;
                }

                true
            });
        }
    }

    did_something
}
