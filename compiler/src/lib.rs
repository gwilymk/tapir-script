#![deny(clippy::all)]
use lalrpop_util::lalrpop_mod;

mod ast;
mod compile;
mod lexer;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use reporting::{format::DiagnosticCache, Message};
