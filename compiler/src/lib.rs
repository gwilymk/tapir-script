#![deny(clippy::all)]
use std::path::Path;

use lalrpop_util::lalrpop_mod;

use reporting::Diagnostics;

mod ast;
mod compile;
mod lexer;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use compile::{CompileSettings, Property};
pub use reporting::{format::DiagnosticCache, Message};
pub use types::Type;

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    compile_settings: CompileSettings,
) -> Result<Vec<u16>, Diagnostics> {
    let bytecode = compile::compile(filename, input, &compile_settings)?;

    Ok(bytecode.compile())
}

pub struct EventHandler {
    pub name: String,
    pub bytecode_offset: usize,
    pub arguments: Vec<Type>,
}
