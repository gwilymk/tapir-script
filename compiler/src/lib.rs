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
) -> Result<CompileResult, Diagnostics> {
    let bytecode = compile::compile(filename, input, &compile_settings)?;

    let compiled = bytecode.compile();
    Ok(CompileResult {
        bytecode: compiled,
        event_handlers: bytecode.event_handlers,
        triggers: bytecode.triggers,
    })
}

pub struct CompileResult {
    pub bytecode: Vec<u16>,
    pub event_handlers: Vec<EventHandler>,
    pub triggers: Vec<Trigger>,
}

pub struct EventHandler {
    pub name: String,
    pub bytecode_offset: usize,
    pub arguments: Vec<EventHandlerArgument>,
}

pub struct Trigger {
    pub name: String,
    pub arguments: Vec<Type>,
}

pub struct EventHandlerArgument {
    pub name: String,
    pub ty: Type,
}
