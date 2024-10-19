mod constant_propagation_visitor;

use std::ops::{BitOr, BitOrAssign};

pub use constant_propagation_visitor::constant_propagation;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ConstantPropagationResult {
    DidSomething,
    DidNothing,
}

impl BitOrAssign for ConstantPropagationResult {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitOr for ConstantPropagationResult {
    type Output = ConstantPropagationResult;

    fn bitor(self, rhs: Self) -> Self::Output {
        if self == ConstantPropagationResult::DidSomething
            || rhs == ConstantPropagationResult::DidSomething
        {
            ConstantPropagationResult::DidSomething
        } else {
            ConstantPropagationResult::DidNothing
        }
    }
}
