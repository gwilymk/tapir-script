use std::borrow::Cow;

use crate::{
    ast::{FunctionReturn, MaybeResolved, FunctionArgument},
    types::Type,
};

use super::super::symtab_visitor::SymTab;

/// Get the name from a MaybeResolved, using the symtab for resolved symbols.
pub fn resolve_name<'a>(name: &'a MaybeResolved<'a>, symtab: &'a SymTab<'a>) -> Cow<'a, str> {
    match name {
        MaybeResolved::Unresolved(s) => Cow::Borrowed(*s),
        MaybeResolved::Resolved(id) => symtab.name_for_symbol(*id),
    }
}

/// Format a list of function arguments as a comma-separated string.
pub fn format_arguments(arguments: &[FunctionArgument<'_>], symtab: &SymTab<'_>) -> String {
    arguments
        .iter()
        .map(|arg| format!("{}: {}", resolve_name(&arg.name, symtab), arg.t.t))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format function return types as a string (empty, " -> T", or " -> (T, U)").
pub fn format_return_types(return_types: &FunctionReturn) -> String {
    if return_types.types.is_empty() {
        String::new()
    } else if return_types.types.len() == 1 {
        format!(" -> {}", return_types.types[0].t)
    } else {
        let types: Vec<String> = return_types
            .types
            .iter()
            .map(|t| t.t.to_string())
            .collect();
        format!(" -> ({})", types.join(", "))
    }
}

/// Get the description for a symbol based on its kind.
pub fn symbol_description(kind: SymbolKind, name: &str, ty: Type) -> String {
    match kind {
        SymbolKind::Builtin => format!("builtin {}: {}", name, ty),
        SymbolKind::Global => format!("global {}: {}", name, ty),
        SymbolKind::Property => format!("property {}: {}", name, ty),
        SymbolKind::Local => format!("var {}: {}", name, ty),
    }
}

/// The kind of symbol for description formatting.
#[derive(Clone, Copy, Debug)]
pub enum SymbolKind {
    Builtin,
    Global,
    Property,
    Local,
}
