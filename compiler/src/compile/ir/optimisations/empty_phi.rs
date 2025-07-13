use std::collections::HashMap;

use crate::{
    ast::SymbolId,
    compile::ir::{
        self, Phi, TapIrFunctionBlockIter,
        optimisations::{OptimisationResult, rename_all_variables},
    },
};

// A PHI is considered empty if it references itself and something else. For example,
// x = phi(y, y, y, y) will cause us to rename all occurrences of x with y. Similarly,
// x = phi(x, x, y, y) will do the same.
//
// However, x = phi(x, y, y, z) is _not_ considered empty, and therefore won't get renamed
// or removed.
//
// It is important to remove these as they result in extra moves which aren't actually required
// and can make other optimisations harder to write because they have consider the empty phis
// when doing the joins at blocks.
fn remove_empty_phis(function: &mut ir::TapIrFunction) -> OptimisationResult {
    // We do 2 passes, the first to collect all the useless phis (and remove them), and the second to
    // rename all the variables that need to be renamed after removing the useless phis
    let mut renames = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(function);
    while let Some(block) = dfs.next_mut(function) {
        block.block_entry_retain_mut(|phi| {
            if let Some(rename) = is_empty(phi) {
                if renames.insert(phi.target, rename).is_some() {
                    panic!(
                        "Should be in SSA form and therefore target should be assigned exactly once"
                    );
                }

                return false;
            }

            true
        });
    }

    rename_all_variables(function, &renames)
}

/// Returns whether this phi should be considered empty. If the result is `None`, then
/// it is _not_ empty. If the result is `Some(symbol_id)` then it _is_ empty and you can
/// safely rename any occurrence of `phi.target` with the result.
fn is_empty(phi: &Phi) -> Option<SymbolId> {
    if phi.sources.is_empty() {
        return None;
    }

    let phi_target = phi.target;
    let mut phi_source = None;

    for &(_, source) in &phi.sources {
        if source == phi_target || phi_source == Some(source) {
            continue;
        }

        if phi_source.is_some_and(|phi_source| phi_source != source) {
            return None; // this is non-empty
        }

        phi_source = Some(source);
    }

    phi_source
}

#[cfg(test)]
mod test {
    use crate::compile::ir::BlockId;

    use super::*;

    #[test]
    fn all_same_variable_is_empty() {
        let phi = Phi {
            target: SymbolId(3),
            sources: vec![
                (BlockId(5), SymbolId(5)),
                (BlockId(4), SymbolId(5)),
                (BlockId(0), SymbolId(5)),
            ],
        };

        assert_eq!(is_empty(&phi), Some(SymbolId(5)));
    }

    #[test]
    fn all_same_including_target_is_empty() {
        let phi = Phi {
            target: SymbolId(3),
            sources: vec![
                (BlockId(5), SymbolId(5)),
                (BlockId(4), SymbolId(3)),
                (BlockId(0), SymbolId(5)),
            ],
        };

        assert_eq!(is_empty(&phi), Some(SymbolId(5)));
    }

    #[test]
    fn different_is_not_empty() {
        let phi = Phi {
            target: SymbolId(3),
            sources: vec![
                (BlockId(5), SymbolId(5)),
                (BlockId(4), SymbolId(3)),
                (BlockId(0), SymbolId(8)),
            ],
        };

        assert_eq!(is_empty(&phi), None);
    }
}
