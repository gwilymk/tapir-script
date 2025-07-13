use std::{
    collections::HashMap,
    ops::{BitOr, BitOrAssign},
};

use crate::{
    ast::SymbolId,
    compile::ir::{TapIrFunction, TapIrFunctionBlockIter},
};

mod empty_phi;

#[derive(Clone, Copy, PartialEq, Eq)]
enum OptimisationResult {
    DidSomething,
    DidNothing,
}

impl BitOrAssign for OptimisationResult {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitOr for OptimisationResult {
    type Output = OptimisationResult;

    fn bitor(self, rhs: Self) -> Self::Output {
        if self == OptimisationResult::DidSomething || rhs == OptimisationResult::DidSomething {
            OptimisationResult::DidSomething
        } else {
            OptimisationResult::DidNothing
        }
    }
}

fn rename_all_variables(
    function: &mut TapIrFunction,
    renames: &HashMap<SymbolId, SymbolId>,
) -> OptimisationResult {
    if renames.is_empty() {
        return OptimisationResult::DidNothing;
    }

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(function);

    while let Some(block) = dfs.next_mut(function) {
        for symbol in block.sources_mut() {
            if let Some(renamed_symbol) = renames.get(symbol) {
                *symbol = *renamed_symbol;
                did_something = OptimisationResult::DidSomething;
            }
        }

        for symbol in block.targets_mut() {
            if let Some(renamed_symbol) = renames.get(symbol) {
                *symbol = *renamed_symbol;
                did_something = OptimisationResult::DidSomething;
            }
        }
    }

    did_something
}
