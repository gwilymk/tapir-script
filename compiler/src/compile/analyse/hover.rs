use std::collections::HashMap;

use crate::{
    ast::{
        Expression, ExpressionKind, InternalOrExternalFunctionId, MethodCallInfo, Script,
        Statement, StatementKind, SymbolId,
    },
    lexer::CommentTable,
    tokens::Span,
    types::StructRegistry,
};

use super::{
    super::{
        symtab_visitor::{GlobalId, SymTab},
        type_visitor::{FieldAccessInfo, FieldAssignmentInfo, TypeTable},
    },
    types::HoverInfo,
    util::{SymbolKind, format_arguments, format_return_types, format_type, symbol_description},
};

/// Extract hover information from the AST.
pub fn extract_hover_info(
    ast: &Script<'_>,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    struct_registry: &StructRegistry,
    comments: &CommentTable<'_>,
) -> HashMap<Span, HoverInfo> {
    let mut hover_info = HashMap::new();
    let mut function_signatures: HashMap<InternalOrExternalFunctionId, HoverInfo> = HashMap::new();

    // Add hover info for struct definitions
    for struct_def in struct_registry.iter() {
        let fields: Vec<String> = struct_def
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, format_type(f.ty, struct_registry)))
            .collect();
        let signature = format!("struct {} {{ {} }}", struct_def.name, fields.join(", "));
        hover_info.insert(
            struct_def.span,
            HoverInfo {
                name: struct_def.name.clone(),
                signature,
                doc_comment: comments.doc_for_item(struct_def.span),
            },
        );
    }

    // Add hover info for struct property bases (e.g., "pos" in "property pos: Point;")
    // These need to be added before expanded properties so they take precedence
    for (name, struct_id, span) in symtab.struct_property_bases() {
        let struct_def = struct_registry.get(struct_id);
        hover_info.insert(
            span,
            HoverInfo {
                name: name.clone(),
                signature: format!("property {}: {}", name, struct_def.name),
                doc_comment: comments.doc_for_item(span),
            },
        );
    }

    // Add hover info for properties (skip struct property fields as they share the span)
    for prop in symtab.properties() {
        // Skip struct property fields - they have struct_info and share span with the base
        if prop.struct_info.is_some() {
            continue;
        }
        hover_info.insert(
            prop.span,
            HoverInfo {
                name: prop.name.clone(),
                signature: symbol_description(
                    SymbolKind::Property,
                    &prop.name,
                    prop.ty,
                    struct_registry,
                ),
                doc_comment: comments.doc_for_item(prop.span),
            },
        );
    }

    // Add hover info for globals
    for global in symtab.globals() {
        hover_info.insert(
            global.span,
            HoverInfo {
                name: global.name.clone(),
                signature: symbol_description(
                    SymbolKind::Global,
                    &global.name,
                    global.ty,
                    struct_registry,
                ),
                doc_comment: comments.doc_for_item(global.span),
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

        let signature = format!("{} {}({}){}", prefix, function.name, args, return_str);

        let info = HoverInfo {
            name: function.name.to_string(),
            signature,
            doc_comment: comments.doc_for_item(function.span),
        };

        hover_info.insert(function.span, info.clone());
        if let Some(fid) = function.meta.get::<crate::ast::FunctionId>() {
            function_signatures.insert(InternalOrExternalFunctionId::Internal(*fid), info);
        }
    }

    // Add hover info for extern functions and build signature map
    for function in &ast.extern_functions {
        let args = format_arguments(&function.arguments);
        let return_str = format_return_types(&function.return_types);

        let signature = format!("extern fn {}({}){}", function.name, args, return_str);

        let info = HoverInfo {
            name: function.name.to_string(),
            signature,
            doc_comment: comments.doc_for_item(function.span),
        };

        hover_info.insert(function.span, info.clone());
        if let Some(fid) = function.meta.get::<crate::ast::ExternalFunctionId>() {
            function_signatures.insert(InternalOrExternalFunctionId::External(*fid), info);
        }
    }

    // Add hover info for builtin functions and build signature map
    for function in &ast.builtin_functions {
        let args = format_arguments(&function.arguments);
        let return_str = format_return_types(&function.return_type);

        // Format the function signature based on whether it's a method or regular function
        let signature = if let Some(receiver) = &function.receiver_type {
            format!(
                "builtin fn {}.{}({}){}",
                receiver.ident, function.name, args, return_str
            )
        } else {
            format!("builtin fn {}({}){}", function.name, args, return_str)
        };

        let info = HoverInfo {
            name: function.name.to_string(),
            signature,
            doc_comment: comments.doc_for_item(function.span),
        };

        hover_info.insert(function.span, info.clone());
        function_signatures.insert(
            InternalOrExternalFunctionId::Builtin(function.builtin_id),
            info,
        );
    }

    // Walk all functions to extract hover info for variables and function calls
    for func in &ast.functions {
        extract_from_statements(
            &func.statements,
            symtab,
            type_table,
            struct_registry,
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
    struct_registry: &StructRegistry,
    function_signatures: &HashMap<InternalOrExternalFunctionId, HoverInfo>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, values } => {
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_hover(
                            ident.span(),
                            *symbol_id,
                            symtab,
                            type_table,
                            struct_registry,
                            hover_info,
                        );
                    }
                }
                for expr in values {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        struct_registry,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Assignment { targets, values } => {
                let field_info: Option<&FieldAssignmentInfo> = stmt.meta.get();
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (i, (target, symbol_id)) in
                        targets.iter().zip(symbol_ids.iter()).enumerate()
                    {
                        // Extract path from target expression
                        if let Some(path) = target.as_lvalue_path() {
                            // Add hover for the root variable (first ident in path)
                            let (_, root_span) = path[0];
                            add_symbol_hover(
                                root_span,
                                *symbol_id,
                                symtab,
                                type_table,
                                struct_registry,
                                hover_info,
                            );

                            // Add hover for field paths (e.g., pos.x = 10)
                            if path.len() > 1
                                && let Some(FieldAssignmentInfo(info_list)) = field_info
                                && let Some(Some((struct_id, field_indices))) = info_list.get(i)
                            {
                                let mut current_struct_id = *struct_id;
                                for (j, (_, field_span)) in path.iter().skip(1).enumerate() {
                                    if let Some(&field_index) = field_indices.get(j) {
                                        let struct_def = struct_registry.get(current_struct_id);
                                        let field_def = &struct_def.fields[field_index];
                                        hover_info.insert(
                                            *field_span,
                                            HoverInfo {
                                                name: field_def.name.clone(),
                                                signature: format!(
                                                    "(field) {}.{}: {}",
                                                    struct_def.name,
                                                    field_def.name,
                                                    format_type(field_def.ty, struct_registry)
                                                ),
                                                doc_comment: None,
                                            },
                                        );
                                        // Update current_struct_id for nested fields
                                        if let Some(next_struct_id) = field_def.ty.as_struct() {
                                            current_struct_id = next_struct_id;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Also extract hover info from target expressions
                for target in targets {
                    extract_from_expression(
                        target,
                        symtab,
                        type_table,
                        struct_registry,
                        function_signatures,
                        hover_info,
                    );
                }
                for expr in values {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        struct_registry,
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
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
                extract_from_statements(
                    true_block,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
                extract_from_statements(
                    false_block,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_from_statements(
                    block,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
            }
            StatementKind::Expression { expression } => {
                extract_from_expression(
                    expression,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_from_expression(
                        expr,
                        symtab,
                        type_table,
                        struct_registry,
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
                        struct_registry,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Wait { frames } => {
                if let Some(frames_expr) = frames {
                    extract_from_expression(
                        frames_expr,
                        symtab,
                        type_table,
                        struct_registry,
                        function_signatures,
                        hover_info,
                    );
                }
            }
            StatementKind::Break
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
    struct_registry: &StructRegistry,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
        let global = symtab.get_global(global_id);
        hover_info.insert(
            span,
            HoverInfo {
                name: global.name.clone(),
                signature: symbol_description(
                    SymbolKind::Global,
                    &global.name,
                    global.ty,
                    struct_registry,
                ),
                doc_comment: None,
            },
        );
        return;
    }

    // Check if it's a struct property base (e.g., "pos" in "pos.x = 10")
    if let Some(struct_id) = symtab.get_property_base_struct_id(symbol_id) {
        let name = symtab.name_for_symbol(symbol_id);
        let struct_def = struct_registry.get(struct_id);
        hover_info.insert(
            span,
            HoverInfo {
                name: name.to_string(),
                signature: format!("property {}: {}", name, struct_def.name),
                doc_comment: None,
            },
        );
        return;
    }

    if let Some(property) = symtab.get_property(symbol_id) {
        hover_info.insert(
            span,
            HoverInfo {
                name: property.name.clone(),
                signature: symbol_description(
                    SymbolKind::Property,
                    &property.name,
                    property.ty,
                    struct_registry,
                ),
                doc_comment: None,
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
            signature: symbol_description(SymbolKind::Local, &name, ty, struct_registry),
            doc_comment: None,
        },
    );
}

fn extract_from_expression(
    expr: &Expression<'_>,
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    struct_registry: &StructRegistry,
    function_signatures: &HashMap<InternalOrExternalFunctionId, HoverInfo>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    match &expr.kind {
        ExpressionKind::Variable(_) => {
            if let Some(symbol_id) = expr.meta.get::<SymbolId>() {
                add_symbol_hover(
                    expr.span,
                    *symbol_id,
                    symtab,
                    type_table,
                    struct_registry,
                    hover_info,
                );
            }
        }
        ExpressionKind::Call { arguments, .. } => {
            // Check if this is a struct constructor call or regular function call
            if let Some(function_id) = expr.meta.get::<InternalOrExternalFunctionId>() {
                if let InternalOrExternalFunctionId::StructConstructor(struct_id) = function_id {
                    let struct_def = struct_registry.get(*struct_id);
                    let fields: Vec<String> = struct_def
                        .fields
                        .iter()
                        .map(|f| format!("{}: {}", f.name, format_type(f.ty, struct_registry)))
                        .collect();
                    let signature = format!("struct {}({})", struct_def.name, fields.join(", "));
                    hover_info.insert(
                        expr.span,
                        HoverInfo {
                            name: struct_def.name.clone(),
                            signature,
                            doc_comment: None,
                        },
                    );
                } else if let Some(sig) = function_signatures.get(function_id) {
                    hover_info.insert(expr.span, sig.clone());
                }
            }
            for arg in arguments {
                extract_from_expression(
                    arg,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_from_expression(
                lhs,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
            extract_from_expression(
                rhs,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
        }
        ExpressionKind::FieldAccess { base, field } => {
            extract_from_expression(
                base,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
            // Add hover info for the field itself
            if let Some(info) = expr.meta.get::<FieldAccessInfo>() {
                let struct_def = struct_registry.get(info.base_struct_id);
                let field_def = &struct_def.fields[info.field_index];
                hover_info.insert(
                    field.span,
                    HoverInfo {
                        name: field_def.name.clone(),
                        signature: format!(
                            "(field) {}.{}: {}",
                            struct_def.name,
                            field_def.name,
                            format_type(field_def.ty, struct_registry)
                        ),
                        doc_comment: None,
                    },
                );
            }
        }
        ExpressionKind::MethodCall {
            receiver,
            method,
            arguments,
        } => {
            // Add hover info for the method name
            if let Some(info) = expr.meta.get::<MethodCallInfo>()
                && let Some(sig) = function_signatures.get(&info.function_id)
            {
                hover_info.insert(method.span, sig.clone());
            }
            extract_from_expression(
                receiver,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
            for arg in arguments {
                extract_from_expression(
                    arg,
                    symtab,
                    type_table,
                    struct_registry,
                    function_signatures,
                    hover_info,
                );
            }
        }
        ExpressionKind::Spawn { call } => {
            extract_from_expression(
                call,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
        }
        ExpressionKind::UnaryOperation { operand, .. } => {
            extract_from_expression(
                operand,
                symtab,
                type_table,
                struct_registry,
                function_signatures,
                hover_info,
            );
        }
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}
