#![deny(clippy::all)]
use lalrpop_util::lalrpop_mod;

mod ast;
mod lexer;
mod reporting;
mod tokens;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use reporting::{format::DiagnosticCache, Message};
