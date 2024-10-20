mod constant_folding_visitor;
mod constant_propagation_visitor;
mod dead_code_elimination_visitor;

use std::ops::{BitOr, BitOrAssign};

use constant_folding_visitor::constant_fold;
use constant_propagation_visitor::constant_propagation;
use dead_code_elimination_visitor::dead_code_eliminate;

use crate::{
    ast::{ExpressionKind, Function},
    reporting::Diagnostics,
};

use super::CompileSettings;

pub fn optimise(
    function: &mut Function,
    compile_settings: &CompileSettings,
    diagnostics: &mut Diagnostics,
) {
    while constant_propagation(function, compile_settings) | constant_fold(function, diagnostics)
        == ConstantOptimisationResult::DidSomething
    {}
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ConstantOptimisationResult {
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

#[derive(Clone, Copy, Debug)]
enum Constant {
    Int(i32),
    Fix(agb_fixnum::Num<i32, 8>),
    Bool(bool),
}

impl From<Constant> for ExpressionKind<'_> {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Int(i) => ExpressionKind::Integer(i),
            Constant::Fix(num) => ExpressionKind::Fix(num),
            Constant::Bool(b) => ExpressionKind::Bool(b),
        }
    }
}

impl TryFrom<&ExpressionKind<'_>> for Constant {
    type Error = ();

    fn try_from(value: &ExpressionKind<'_>) -> Result<Self, Self::Error> {
        Ok(match value {
            ExpressionKind::Integer(i) => Constant::Int(*i),
            ExpressionKind::Fix(num) => Constant::Fix(*num),
            ExpressionKind::Bool(b) => Constant::Bool(*b),
            _ => return Err(()),
        })
    }
}
