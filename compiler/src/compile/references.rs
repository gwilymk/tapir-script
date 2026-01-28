use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionKind, Script, Statement, StatementKind, SymbolId},
    tokens::Span,
};

use super::symtab_visitor::{GlobalId, SymTab};

pub fn extract_references(ast: &Script<'_>, symtab: &SymTab<'_>) -> HashMap<Span, Span> {
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
            &function_spans,
            &mut references,
        );
    }

    references
}

fn extract_references_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    function_spans: &HashMap<&str, Span>,
    references: &mut HashMap<Span, Span>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, values } => {
                // For declarations, the idents ARE the definitions, so just process RHS
                for expr in values {
                    extract_references_from_expression(expr, symtab, function_spans, references);
                }
                // But also add references for idents that refer to properties (re-assignment)
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_reference(ident.span(), *symbol_id, symtab, references);
                    }
                }
            }
            StatementKind::Assignment { idents, values } => {
                // For assignments, LHS idents refer to existing definitions
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_reference(ident.span, *symbol_id, symtab, references);
                    }
                }
                for expr in values {
                    extract_references_from_expression(expr, symtab, function_spans, references);
                }
            }
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                extract_references_from_expression(condition, symtab, function_spans, references);
                extract_references_from_statements(true_block, symtab, function_spans, references);
                extract_references_from_statements(false_block, symtab, function_spans, references);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_references_from_statements(block, symtab, function_spans, references);
            }
            StatementKind::Call { name, arguments } | StatementKind::Spawn { name, arguments } => {
                if let Some(&def_span) = function_spans.get(name) {
                    references.insert(stmt.span, def_span);
                }
                for expr in arguments {
                    extract_references_from_expression(expr, symtab, function_spans, references);
                }
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_references_from_expression(expr, symtab, function_spans, references);
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_references_from_expression(expr, symtab, function_spans, references);
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
                extract_references_from_expression(arg, symtab, function_spans, references);
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_references_from_expression(lhs, symtab, function_spans, references);
            extract_references_from_expression(rhs, symtab, function_spans, references);
        }
        ExpressionKind::FieldAccess { base, .. } => {
            extract_references_from_expression(base, symtab, function_spans, references);
            // TODO: Add reference to field definition
        }
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}
