mod constant_folding_visitor;
mod constant_propagation_visitor;

use std::ops::{BitOr, BitOrAssign};

pub use constant_propagation_visitor::constant_propagation;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ConstantOptimisationResult {
    DidSomething,
    DidNothing,
}

impl BitOrAssign for ConstantOptimisationResult {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitOr for ConstantOptimisationResult {
    type Output = ConstantOptimisationResult;

    fn bitor(self, rhs: Self) -> Self::Output {
        if self == ConstantOptimisationResult::DidSomething
            || rhs == ConstantOptimisationResult::DidSomething
        {
            ConstantOptimisationResult::DidSomething
        } else {
            ConstantOptimisationResult::DidNothing
        }
    }
}
