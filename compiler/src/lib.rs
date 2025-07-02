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

    let compiled = bytecode.data.into_boxed_slice();
    Ok(CompileResult {
        bytecode: compiled,
        event_handlers: bytecode.event_handlers.into_boxed_slice(),
        triggers: bytecode.triggers,
    })
}

pub struct CompileResult {
    pub bytecode: Box<[u32]>,
    pub event_handlers: Box<[EventHandler]>,
    pub triggers: Box<[Trigger]>,
}

pub struct EventHandler {
    pub name: String,
    pub bytecode_offset: usize,
    pub arguments: Box<[EventHandlerArgument]>,
}

pub struct Trigger {
    pub name: String,
    pub arguments: Box<[Type]>,
}

#[derive(Clone)]
pub struct EventHandlerArgument {
    pub name: String,
    pub ty: Type,
}
