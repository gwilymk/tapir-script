use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionKind, Script, Statement, StatementKind, SymbolId},
    tokens::Span,
    types::StructRegistry,
};

use super::{
    symtab_visitor::{GlobalId, SymTab},
    type_visitor::{FieldAccessInfo, FieldAssignmentInfo},
};

pub fn extract_references(
    ast: &Script<'_>,
    symtab: &SymTab<'_>,
    struct_registry: &StructRegistry,
) -> HashMap<Span, Span> {
    let mut references = HashMap::new();

    // Build a map of function names to their definition spans
    let mut function_spans: HashMap<&str, Span> = HashMap::new();
    for func in &ast.functions {
        if func.name != "@toplevel" {
            function_spans.insert(func.name, func.span);
        }
    }
    for func in &ast.extern_functions {
        function_spans.insert(func.name, func.span);
    }

    // Walk all functions
    for func in &ast.functions {
        extract_references_from_statements(
            &func.statements,
            symtab,
            struct_registry,
            &function_spans,
            &mut references,
        );
    }

    references
}

fn extract_references_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    struct_registry: &StructRegistry,
    function_spans: &HashMap<&str, Span>,
    references: &mut HashMap<Span, Span>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, values } => {
                // For declarations, the idents ARE the definitions, so just process RHS
                for expr in values {
                    extract_references_from_expression(
                        expr,
                        symtab,
                        struct_registry,
                        function_spans,
                        references,
                    );
                }
                // But also add references for idents that refer to properties (re-assignment)
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_reference(ident.span(), *symbol_id, symtab, references);
                    }
                }
            }
            StatementKind::Assignment { targets, values } => {
                // For assignments, LHS idents refer to existing definitions
                let field_info: Option<&FieldAssignmentInfo> = stmt.meta.get();
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (i, (path, symbol_id)) in targets.iter().zip(symbol_ids.iter()).enumerate()
                    {
                        // Add reference for the root variable (first ident in path)
                        if let Some(first) = path.first() {
                            add_symbol_reference(first.span, *symbol_id, symtab, references);
                        }

                        // Add references for field paths (e.g., pos.x = 10)
                        if path.len() > 1
                            && let Some(FieldAssignmentInfo(info_list)) = field_info
                            && let Some(Some((struct_id, field_indices))) = info_list.get(i)
                        {
                            let mut current_struct_id = *struct_id;
                            for (j, field_ident) in path.iter().skip(1).enumerate() {
                                if let Some(&field_index) = field_indices.get(j) {
                                    let struct_def = struct_registry.get(current_struct_id);
                                    let field_def = &struct_def.fields[field_index];
                                    references.insert(field_ident.span, field_def.span);
                                    // Update current_struct_id for nested fields
                                    if let Some(next_struct_id) = field_def.ty.as_struct() {
                                        current_struct_id = next_struct_id;
                                    }
                                }
                            }
                        }
                    }
                }
                for expr in values {
                    extract_references_from_expression(
                        expr,
                        symtab,
                        struct_registry,
                        function_spans,
                        references,
                    );
                }
            }
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                extract_references_from_expression(
                    condition,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
                extract_references_from_statements(
                    true_block,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
                extract_references_from_statements(
                    false_block,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_references_from_statements(
                    block,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
            }
            StatementKind::Expression { expression } => {
                extract_references_from_expression(
                    expression,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_references_from_expression(
                        expr,
                        symtab,
                        struct_registry,
                        function_spans,
                        references,
                    );
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_references_from_expression(
                        expr,
                        symtab,
                        struct_registry,
                        function_spans,
                        references,
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

fn add_symbol_reference(
    usage_span: Span,
    symbol_id: SymbolId,
    symtab: &SymTab<'_>,
    references: &mut HashMap<Span, Span>,
) {
    // Check if it's a global
    if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
        let global = symtab.get_global(global_id);
        references.insert(usage_span, global.span);
    } else if let Some(property) = symtab.get_property(symbol_id) {
        // It's a property
        references.insert(usage_span, property.span);
    } else {
        // It's a local variable - get span from symtab
        let def_span = symtab.span_for_symbol(symbol_id);
        references.insert(usage_span, def_span);
    }
}

fn extract_references_from_expression(
    expr: &Expression<'_>,
    symtab: &SymTab<'_>,
    struct_registry: &StructRegistry,
    function_spans: &HashMap<&str, Span>,
    references: &mut HashMap<Span, Span>,
) {
    match &expr.kind {
        ExpressionKind::Variable(_name) => {
            if let Some(symbol_id) = expr.meta.get::<SymbolId>() {
                add_symbol_reference(expr.span, *symbol_id, symtab, references);
            }
        }
        ExpressionKind::Call { name, arguments } => {
            if let Some(&def_span) = function_spans.get(name) {
                references.insert(expr.span, def_span);
            }
            for arg in arguments {
                extract_references_from_expression(
                    arg,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_references_from_expression(
                lhs,
                symtab,
                struct_registry,
                function_spans,
                references,
            );
            extract_references_from_expression(
                rhs,
                symtab,
                struct_registry,
                function_spans,
                references,
            );
        }
        ExpressionKind::FieldAccess { base, field } => {
            extract_references_from_expression(
                base,
                symtab,
                struct_registry,
                function_spans,
                references,
            );
            // Add reference from field access to field definition
            if let Some(info) = expr.meta.get::<FieldAccessInfo>() {
                let struct_def = struct_registry.get(info.base_struct_id);
                let field_def = &struct_def.fields[info.field_index];
                references.insert(field.span, field_def.span);
            }
        }
        ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            extract_references_from_expression(
                receiver,
                symtab,
                struct_registry,
                function_spans,
                references,
            );
            for arg in arguments {
                extract_references_from_expression(
                    arg,
                    symtab,
                    struct_registry,
                    function_spans,
                    references,
                );
            }
        }
        ExpressionKind::Spawn { call } => {
            extract_references_from_expression(
                call,
                symtab,
                struct_registry,
                function_spans,
                references,
            );
        }
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}
