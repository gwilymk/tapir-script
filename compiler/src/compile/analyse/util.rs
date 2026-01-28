use crate::{
    ast::{FunctionReturn, TypedIdent},
    types::{StructRegistry, Type},
};

/// Format a list of function arguments as a comma-separated string.
pub fn format_arguments(arguments: &[TypedIdent<'_>]) -> String {
    arguments
        .iter()
        .map(|arg| format!("{}: {}", arg.name(), arg.ty_required().resolved()))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format function return types as a string (empty, " -> T", or " -> (T, U)").
pub fn format_return_types(return_types: &FunctionReturn) -> String {
    if return_types.types.is_empty() {
        String::new()
    } else if return_types.types.len() == 1 {
        format!(" -> {}", return_types.types[0].resolved())
    } else {
        let types: Vec<String> = return_types
            .types
            .iter()
            .map(|t| t.resolved().to_string())
            .collect();
        format!(" -> ({})", types.join(", "))
    }
}

/// Get the description for a symbol based on its kind.
pub fn symbol_description(
    kind: SymbolKind,
    name: &str,
    ty: Type,
    registry: &StructRegistry,
) -> String {
    match kind {
        SymbolKind::Global => format!("global {}: {}", name, ty.display(registry)),
        SymbolKind::Property => format!("property {}: {}", name, ty.display(registry)),
        SymbolKind::Local => format!("var {}: {}", name, ty.display(registry)),
    }
}

/// Format a type for display, using the struct registry for struct names.
pub fn format_type(ty: Type, registry: &StructRegistry) -> String {
    ty.display(registry).to_string()
}

/// The kind of symbol for description formatting.
#[derive(Clone, Copy, Debug)]
pub enum SymbolKind {
    Global,
    Property,
    Local,
}
