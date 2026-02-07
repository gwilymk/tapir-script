use crate::{
    ast::{Script, Statement, StatementKind, SymbolId},
    tokens::FileId,
    types::{StructRegistry, Type},
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
    struct_registry: &StructRegistry,
    main_file_id: FileId,
) -> Vec<InlayHintInfo> {
    let mut hints = Vec::new();

    for func in &ast.functions {
        if func.span.file_id != main_file_id {
            continue;
        }
        extract_from_statements(
            &func.statements,
            symtab,
            type_table,
            struct_registry,
            &mut hints,
        );
    }

    hints
}

fn extract_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    struct_registry: &StructRegistry,
    hints: &mut Vec<InlayHintInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, .. } => {
                // Add type hints for each declared variable
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        // Skip if the variable already has an explicit type annotation
                        if ident.ty.is_some() {
                            continue;
                        }

                        // Skip if it's a property (properties have explicit types)
                        if symtab.get_property(*symbol_id).is_some() {
                            continue;
                        }

                        let ty = type_table.type_for_symbol(*symbol_id, symtab);
                        // Don't show hints for error types
                        if ty != Type::Error {
                            hints.push(InlayHintInfo {
                                position: ident.span().end(),
                                label: format!(": {}", ty.name(struct_registry)),
                            });
                        }
                    }
                }
            }
            StatementKind::If {
                true_block,
                false_block,
                ..
            } => {
                extract_from_statements(true_block, symtab, type_table, struct_registry, hints);
                extract_from_statements(false_block, symtab, type_table, struct_registry, hints);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_from_statements(block, symtab, type_table, struct_registry, hints);
            }
            _ => {}
        }
    }
}
