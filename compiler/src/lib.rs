#![deny(clippy::all)]
use std::path::Path;

use lalrpop_util::lalrpop_mod;

mod ast;
mod compile;
mod file_loader;
mod import_resolver;
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
    ParameterInfo, SignatureInfo, SymbolInfo, analyse, analyse_with_loader,
};
pub use compile::disassemble;
pub use compile::symtab_visitor::GlobalInfo;
pub use compile::{CompileSettings, Property, PRELUDE_PATH, PRELUDE_SOURCE};
pub use file_loader::{FileLoader, FsFileLoader, TestFileLoader};
pub use reporting::format::DiagnosticCache;
pub use reporting::{
    Diagnostic, DiagnosticMessage, Diagnostics, ErrorKind, Severity, SourcePosition, SourceRange,
};
pub use tokens::Span;
pub use types::{StructDef, StructField, StructId, StructRegistry, Type};

/// Compile a single file without import support.
///
/// This is a convenience wrapper around `compile_with_loader` for single-file
/// compilation. If you need imports, use `compile_with_loader` directly with
/// an `FsFileLoader`.
pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    compile_settings: CompileSettings,
) -> Result<CompileResult, Diagnostics> {
    let loader = FsFileLoader::new();

    // Insert the input at the filename path
    loader.insert(filename.as_ref(), input);

    // Insert prelude if enabled
    if compile_settings.enable_prelude {
        loader.insert(PRELUDE_PATH, PRELUDE_SOURCE);
    }

    compile_with_loader(filename, &compile_settings, &loader)
}

/// Compile with a file loader for resolving imports.
///
/// This is the primary compile function for multi-file compilation.
/// The file loader owns source strings and provides import resolution.
///
/// **Setup requirements:**
/// - If `settings.enable_prelude` is true, the prelude must be inserted into
///   the file loader at `PRELUDE_PATH` before calling this function.
/// - The main file must be loadable from the file loader at `filename`.
///
/// Example:
/// ```ignore
/// let loader = FsFileLoader::new();
/// loader.insert(PRELUDE_PATH, PRELUDE_SOURCE);
/// // ... file is on disk, loader will read it
/// let result = compile_with_loader("script.tapir", &settings, &loader)?;
/// ```
pub fn compile_with_loader(
    filename: impl AsRef<Path>,
    compile_settings: &CompileSettings,
    file_loader: &dyn FileLoader,
) -> Result<CompileResult, Diagnostics> {
    let output = compile::compile_with_loader(filename, compile_settings, file_loader)?;
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
    /// For struct properties, this is the full path (e.g., "pos.x").
    pub name: String,
    /// The type of the property (always a scalar type for expanded properties).
    pub ty: Type,
    /// The index into the property array (matches declaration order).
    pub index: usize,
    /// The span where the property was declared.
    pub span: Span,
    /// For struct properties: info about the parent struct property.
    /// None for scalar properties.
    pub struct_info: Option<StructPropertyInfo>,
}

/// Metadata for a property that's part of an expanded struct.
/// Used by the macro to generate tuple conversion code.
pub struct StructPropertyInfo {
    /// The original property name in the Rust struct (e.g., "position").
    pub rust_field_name: String,
    /// Position of this field within the flattened tuple (0-indexed).
    pub tuple_position: usize,
    /// Types of all scalar fields in order (for generating the tuple type annotation).
    pub field_types: Box<[Type]>,
    /// The struct type ID for this property.
    pub struct_id: StructId,
}

impl StructPropertyInfo {
    /// Total number of scalar fields in the expanded struct.
    pub fn total_fields(&self) -> usize {
        self.field_types.len()
    }
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
