#![deny(clippy::all)]
use std::path::Path;

use lalrpop_util::lalrpop_mod;

mod ast;
mod builtins;
mod compile;
mod lexer;
mod prelude;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use compile::analyse::{
    AnalysisResult, CallSiteInfo, FunctionArgumentInfo, FunctionInfo, HoverInfo, InlayHintInfo,
    ParameterInfo, SignatureInfo, SymbolInfo, analyse,
};
pub use compile::symtab_visitor::GlobalInfo;
pub use compile::{CompileSettings, Property};
pub use reporting::format::DiagnosticCache;
pub use reporting::{
    Diagnostic, DiagnosticMessage, Diagnostics, ErrorKind, Severity, SourcePosition, SourceRange,
};
pub use tokens::Span;
pub use types::Type;

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    compile_settings: CompileSettings,
) -> Result<CompileResult, Diagnostics> {
    let output = compile::compile(filename, input, &compile_settings)?;
    let parts = output.bytecode.into_parts();

    Ok(CompileResult {
        bytecode: parts.bytecode,
        globals: parts.globals,
        properties: parts.properties,
        event_handlers: parts.event_handlers,
        triggers: parts.triggers,
        extern_functions: parts.extern_functions,
        warnings: output.warnings,
    })
}

pub struct CompileResult {
    pub bytecode: Box<[u32]>,
    pub globals: Box<[i32]>,
    pub properties: Box<[PropertyInfo]>,
    pub event_handlers: Box<[EventHandler]>,
    pub triggers: Box<[Trigger]>,
    pub extern_functions: Box<[ExternFunction]>,
    /// Warnings generated during compilation (empty if no warnings).
    pub warnings: Diagnostics,
}

/// Information about a declared property in the compiled script.
pub struct PropertyInfo {
    /// The name of the property as declared in the script.
    pub name: String,
    /// The type of the property.
    pub ty: Type,
    /// The index into the property array (matches declaration order).
    pub index: usize,
    /// The span where the property was declared.
    pub span: Span,
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
