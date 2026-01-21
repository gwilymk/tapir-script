use std::collections::HashMap;

use crate::{PropertyInfo, reporting::Diagnostics, tokens::Span, types::Type};

use super::super::symtab_visitor::GlobalInfo;

/// Information about a symbol (local variable) in the script.
#[derive(Clone, Debug)]
pub struct SymbolInfo {
    /// The name of the symbol.
    pub name: String,
    /// The type of the symbol.
    pub ty: Type,
    /// The span where this symbol was defined.
    pub definition_span: Span,
}

/// Information for displaying on hover.
#[derive(Clone, Debug)]
pub struct HoverInfo {
    /// The name of the symbol/function.
    pub name: String,
    /// A description of the symbol (e.g., type or signature).
    pub description: String,
}

/// Information about a function call site for signature help.
#[derive(Clone, Debug)]
pub struct CallSiteInfo {
    /// The name of the function being called.
    pub function_name: String,
    /// The span covering the argument list (inside the parentheses).
    pub arguments_span: Span,
    /// The start positions of each argument (used for parameter name inlay hints).
    pub argument_start_offsets: Vec<usize>,
    /// The end positions of each argument (used to determine active parameter).
    pub argument_end_offsets: Vec<usize>,
}

/// Signature information for a function.
#[derive(Clone, Debug)]
pub struct SignatureInfo {
    /// The full signature label, e.g., "fn add(a: int, b: int) -> int"
    pub label: String,
    /// Parameter labels and their positions within the label string.
    pub parameters: Vec<ParameterInfo>,
}

/// Information about a function parameter for signature help.
#[derive(Clone, Debug)]
pub struct ParameterInfo {
    /// The parameter label, e.g., "a: int"
    pub label: String,
}

/// Information for an inlay hint (inline type annotation).
#[derive(Clone, Debug)]
pub struct InlayHintInfo {
    /// The position where the hint should appear (end of variable name).
    pub position: usize,
    /// The hint label, e.g., ": int"
    pub label: String,
}

/// Information about a function argument.
#[derive(Clone, Debug)]
pub struct FunctionArgumentInfo {
    /// The name of the argument.
    pub name: String,
    /// The type of the argument.
    pub ty: Type,
    /// The span of the argument definition.
    pub span: Span,
}

/// Information about a function in the script.
#[derive(Clone, Debug)]
pub struct FunctionInfo {
    /// The name of the function.
    pub name: String,
    /// The span of the function definition.
    pub span: Span,
    /// The function's arguments.
    pub arguments: Vec<FunctionArgumentInfo>,
    /// The function's return types.
    pub return_types: Vec<Type>,
    /// Whether this function is an event handler.
    pub is_event_handler: bool,
}

/// The result of analysing a tapir script.
///
/// Contains all the semantic information gathered during analysis,
/// including symbols, types, functions, and any diagnostics.
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from the analysis.
    pub diagnostics: Diagnostics,
    /// Information about local variables and their types.
    pub symbols: Vec<SymbolInfo>,
    /// Information about global variables.
    pub globals: Vec<GlobalInfo>,
    /// Information about declared properties.
    pub properties: Vec<PropertyInfo>,
    /// Information about functions (both internal and extern).
    pub functions: Vec<FunctionInfo>,
    /// Map from usage spans to definition spans (for go-to-definition).
    pub references: HashMap<Span, Span>,
    /// Map from spans to hover information.
    pub hover_info: HashMap<Span, HoverInfo>,
    /// Function call sites for signature help.
    pub call_sites: Vec<CallSiteInfo>,
    /// Function signatures for signature help.
    pub signatures: HashMap<String, SignatureInfo>,
    /// Inlay hints for variable type annotations.
    pub inlay_hints: Vec<InlayHintInfo>,
}
