#![deny(clippy::all)]
use std::path::Path;

use lalrpop_util::lalrpop_mod;

use reporting::Diagnostics;

mod ast;
mod builtins;
mod compile;
mod lexer;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use compile::{CompileSettings, Property};
pub use reporting::format::DiagnosticCache;
pub use types::Type;

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    compile_settings: CompileSettings,
) -> Result<CompileResult, Diagnostics> {
    let bytecode = compile::compile(filename, input, &compile_settings)?;
    let parts = bytecode.into_parts();

    Ok(CompileResult {
        bytecode: parts.bytecode,
        globals: parts.globals,
        event_handlers: parts.event_handlers,
        triggers: parts.triggers,
        extern_functions: parts.extern_functions,
    })
}

pub struct CompileResult {
    pub bytecode: Box<[u32]>,
    pub globals: Box<[i32]>,
    pub event_handlers: Box<[EventHandler]>,
    pub triggers: Box<[Trigger]>,
    pub extern_functions: Box<[ExternFunction]>,
}

pub struct ExternFunction {
    pub name: String,
    pub arguments: Box<[Type]>,
    pub returns: Box<[Type]>,
}

pub struct EventHandler {
    pub name: String,
    pub bytecode_offset: usize,
    pub arguments: Box<[FunctionArgument]>,
}

pub struct Trigger {
    pub name: String,
    pub arguments: Box<[Type]>,
}

#[derive(Clone)]
pub struct FunctionArgument {
    pub name: String,
    pub ty: Type,
}
