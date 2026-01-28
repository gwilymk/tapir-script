use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionKind, Script, Statement, StatementKind, SymbolId},
    tokens::Span,
};

use super::{
    super::{
        symtab_visitor::{GlobalId, SymTab},
        type_visitor::TypeTable,
    },
    types::HoverInfo,
    util::{SymbolKind, format_arguments, format_return_types, symbol_description},
};

/// Extract hover information from the AST.
pub fn extract_hover_info(
    ast: &Script<'_>,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
) -> HashMap<Span, HoverInfo> {
    let mut hover_info = HashMap::new();
    let mut function_signatures: HashMap<&str, HoverInfo> = HashMap::new();

    // Add hover info for properties
    for prop in symtab.properties() {
        hover_info.insert(
            prop.span,
            HoverInfo {
                name: prop.name.clone(),
                description: symbol_description(SymbolKind::Property, &prop.name, prop.ty),
            },
        );
    }

    // Add hover info for globals
    for global in symtab.globals() {
        hover_info.insert(
            global.span,
            HoverInfo {
                name: global.name.clone(),
                description: symbol_description(SymbolKind::Global, &global.name, global.ty),
            },
        );
    }

    // Add hover info for functions and build signature map
    for function in &ast.functions {
        if function.name == "@toplevel" {
            continue;
        }

        let args = format_arguments(&function.arguments);
        let return_str = format_return_types(&function.return_types);

        let prefix = if function.modifiers.is_event_handler.is_some() {
            "event fn"
        } else {
            "fn"
        };

        let info = HoverInfo {
            name: function.name.to_string(),
            description: format!("{} {}({}){}", prefix, function.name, args, return_str),
        };

        hover_info.insert(function.span, info.clone());
        function_signatures.insert(function.name, info);
    }

    // Add hover info for extern functions and build signature map
    for function in &ast.extern_functions {
        let args = format_arguments(&function.arguments);
        let return_str = format_return_types(&function.return_types);

        let info = HoverInfo {
            name: function.name.to_string(),
            description: format!("extern fn {}({}){}", function.name, args, return_str),
        };

        hover_info.insert(function.span, info.clone());
        function_signatures.insert(function.name, info);
    }

    // Walk all functions to extract hover info for variables and function calls
    for func in &ast.functions {
        extract_from_statements(
            &func.statements,
            symtab,
            type_table,
            &function_signatures,
            &mut hover_info,
        );
    }

    hover_info
}

fn extract_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    function_signatures: &HashMap<&str, HoverInfo>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, values } => {
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_hover(ident.span(), *symbol_id, symtab, type_table, hover_info);
                    }
                }
                for expr in values {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Assignment { targets, values } => {
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (path, symbol_id) in targets.iter().zip(symbol_ids.iter()) {
                        // Add hover for the root variable (first ident in path)
                        if let Some(first) = path.first() {
                            add_symbol_hover(first.span, *symbol_id, symtab, type_table, hover_info);
                        }
                    }
                }
                for expr in values {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                extract_from_expression(
                    condition,
                    symtab,
                    type_table,
                    function_signatures,
                    hover_info,
                );
                extract_from_statements(
                    true_block,
                    symtab,
                    type_table,
                    function_signatures,
                    hover_info,
                );
                extract_from_statements(
                    false_block,
                    symtab,
                    type_table,
                    function_signatures,
                    hover_info,
                );
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_from_statements(block, symtab, type_table, function_signatures, hover_info);
            }
            StatementKind::Call { name, arguments } => {
                if let Some(sig) = function_signatures.get(name) {
                    hover_info.insert(stmt.span, sig.clone());
                }
                for expr in arguments {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Spawn { name, arguments } => {
                if let Some(sig) = function_signatures.get(name) {
                    hover_info.insert(stmt.span, sig.clone());
                }
                for expr in arguments {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Wait
            | StatementKind::Break
            | StatementKind::Continue
            | StatementKind::Nop
            | StatementKind::Error => {}
        }
    }
}

fn add_symbol_hover(
    span: Span,
    symbol_id: SymbolId,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
        let global = symtab.get_global(global_id);
        hover_info.insert(
            span,
            HoverInfo {
                name: global.name.clone(),
                description: symbol_description(SymbolKind::Global, &global.name, global.ty),
            },
        );
        return;
    }

    if let Some(property) = symtab.get_property(symbol_id) {
        hover_info.insert(
            span,
            HoverInfo {
                name: property.name.clone(),
                description: symbol_description(SymbolKind::Property, &property.name, property.ty),
            },
        );
        return;
    }

    // Local variable
    let ty = type_table.type_for_symbol(symbol_id, symtab);
    let name = symtab.name_for_symbol(symbol_id);
    hover_info.insert(
        span,
        HoverInfo {
            name: name.to_string(),
            description: symbol_description(SymbolKind::Local, &name, ty),
        },
    );
}

fn extract_from_expression(
    expr: &Expression<'_>,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    function_signatures: &HashMap<&str, HoverInfo>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    match &expr.kind {
        ExpressionKind::Variable(_) => {
            if let Some(symbol_id) = expr.meta.get::<SymbolId>() {
                add_symbol_hover(expr.span, *symbol_id, symtab, type_table, hover_info);
            }
        }
        ExpressionKind::Call { name, arguments } => {
            if let Some(sig) = function_signatures.get(name) {
                hover_info.insert(expr.span, sig.clone());
            }
            for arg in arguments {
                extract_from_expression(arg, symtab, type_table, function_signatures, hover_info);
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_from_expression(lhs, symtab, type_table, function_signatures, hover_info);
            extract_from_expression(rhs, symtab, type_table, function_signatures, hover_info);
        }
        ExpressionKind::FieldAccess { base, .. } => {
            extract_from_expression(base, symtab, type_table, function_signatures, hover_info);
            // TODO: Add hover info for the field itself (show field type)
        }
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}
