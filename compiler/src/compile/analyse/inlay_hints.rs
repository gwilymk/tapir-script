use crate::{
    ast::{Script, Statement, StatementKind, SymbolId},
    types::Type,
};

use super::{
    super::{symtab_visitor::SymTab, type_visitor::TypeTable},
    types::InlayHintInfo,
};

/// Extract inlay hints for variable type annotations.
pub fn extract_inlay_hints(
    ast: &Script<'_>,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
) -> Vec<InlayHintInfo> {
    let mut hints = Vec::new();

    for func in &ast.functions {
        extract_from_statements(&func.statements, symtab, type_table, &mut hints);
    }

    hints
}

fn extract_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    hints: &mut Vec<InlayHintInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, .. } => {
                // Add type hints for each declared variable
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        // Skip if it's a property (properties have explicit types)
                        if symtab.get_property(*symbol_id).is_some() {
                            continue;
                        }

                        let ty = type_table.type_for_symbol(*symbol_id, symtab);
                        // Don't show hints for error types
                        if ty != Type::Error {
                            hints.push(InlayHintInfo {
                                position: ident.span.end(),
                                label: format!(": {}", ty),
                            });
                        }
                    }
                }
            }
            StatementKind::If { true_block, false_block, .. } => {
                extract_from_statements(true_block, symtab, type_table, hints);
                extract_from_statements(false_block, symtab, type_table, hints);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_from_statements(block, symtab, type_table, hints);
            }
            _ => {}
        }
    }
}
