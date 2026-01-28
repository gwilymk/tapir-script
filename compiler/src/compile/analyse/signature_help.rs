use std::collections::HashMap;

use crate::ast::{Expression, ExpressionKind, Script, Statement, StatementKind};

use super::{
    types::{CallSiteInfo, ParameterInfo, SignatureInfo},
    util::format_return_types,
};

/// Extract signature help information from the AST.
pub fn extract_signature_help(
    ast: &Script<'_>,
) -> (HashMap<String, SignatureInfo>, Vec<CallSiteInfo>) {
    let mut signatures = HashMap::new();
    let mut call_sites = Vec::new();

    // Build signature info for internal functions
    for function in &ast.functions {
        if function.name == "@toplevel" {
            continue;
        }

        let params: Vec<ParameterInfo> = function
            .arguments
            .iter()
            .map(|arg| ParameterInfo {
                label: format!("{}: {}", arg.name(), arg.ty_required().resolved()),
            })
            .collect();

        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();
        let return_str = format_return_types(&function.return_types);

        let prefix = if function.modifiers.is_event_handler.is_some() {
            "event fn"
        } else {
            "fn"
        };

        signatures.insert(
            function.name.to_string(),
            SignatureInfo {
                label: format!(
                    "{} {}({}){}",
                    prefix,
                    function.name,
                    params_str.join(", "),
                    return_str
                ),
                parameters: params,
            },
        );
    }

    // Build signature info for extern functions
    for function in &ast.extern_functions {
        let params: Vec<ParameterInfo> = function
            .arguments
            .iter()
            .map(|arg| ParameterInfo {
                label: format!("{}: {}", arg.name(), arg.ty_required().resolved()),
            })
            .collect();

        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();
        let return_str = format_return_types(&function.return_types);

        signatures.insert(
            function.name.to_string(),
            SignatureInfo {
                label: format!(
                    "extern fn {}({}){}",
                    function.name,
                    params_str.join(", "),
                    return_str
                ),
                parameters: params,
            },
        );
    }

    // Walk all functions to extract call sites
    for func in &ast.functions {
        extract_from_statements(&func.statements, &signatures, &mut call_sites);
    }

    (signatures, call_sites)
}

fn extract_from_statements(
    statements: &[Statement<'_>],
    signatures: &HashMap<String, SignatureInfo>,
    call_sites: &mut Vec<CallSiteInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { values, .. }
            | StatementKind::Assignment { values, .. } => {
                for expr in values {
                    extract_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                extract_from_expression(condition, signatures, call_sites);
                extract_from_statements(true_block, signatures, call_sites);
                extract_from_statements(false_block, signatures, call_sites);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_from_statements(block, signatures, call_sites);
            }
            StatementKind::Call { name, arguments } | StatementKind::Spawn { name, arguments } => {
                if signatures.contains_key(*name) {
                    let argument_start_offsets: Vec<usize> =
                        arguments.iter().map(|arg| arg.span.start()).collect();
                    let argument_end_offsets: Vec<usize> =
                        arguments.iter().map(|arg| arg.span.end()).collect();

                    call_sites.push(CallSiteInfo {
                        function_name: name.to_string(),
                        arguments_span: stmt.span,
                        argument_start_offsets,
                        argument_end_offsets,
                    });
                }
                for expr in arguments {
                    extract_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_from_expression(expr, signatures, call_sites);
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

fn extract_from_expression(
    expr: &Expression<'_>,
    signatures: &HashMap<String, SignatureInfo>,
    call_sites: &mut Vec<CallSiteInfo>,
) {
    match &expr.kind {
        ExpressionKind::Call { name, arguments } => {
            if signatures.contains_key(*name) {
                let argument_start_offsets: Vec<usize> =
                    arguments.iter().map(|arg| arg.span.start()).collect();
                let argument_end_offsets: Vec<usize> =
                    arguments.iter().map(|arg| arg.span.end()).collect();

                call_sites.push(CallSiteInfo {
                    function_name: name.to_string(),
                    arguments_span: expr.span,
                    argument_start_offsets,
                    argument_end_offsets,
                });
            }
            for arg in arguments {
                extract_from_expression(arg, signatures, call_sites);
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_from_expression(lhs, signatures, call_sites);
            extract_from_expression(rhs, signatures, call_sites);
        }
        ExpressionKind::FieldAccess { base, .. } => {
            extract_from_expression(base, signatures, call_sites);
        }
        ExpressionKind::Variable(_)
        | ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}
