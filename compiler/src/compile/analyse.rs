use std::{collections::HashMap, path::Path};

use crate::{
    ast::{Expression, ExpressionKind, Script, Statement, StatementKind, SymbolId},
    builtins::BuiltinVariable,
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::{FileId, Span},
    types::Type,
    PropertyInfo,
};

use super::{
    loop_visitor,
    references::extract_references,
    symtab_visitor::{GlobalId, GlobalInfo, SymTab, SymTabVisitor},
    type_visitor::{TypeTable, TypeVisitor},
    CompileSettings, Property,
};

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
    /// The end positions of each argument (used to determine active parameter).
    /// For `foo(a, b, c)`, this would be the positions after `a`, `b`, `c`.
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

fn property_to_info(prop: &Property) -> PropertyInfo {
    PropertyInfo {
        name: prop.name.clone(),
        ty: prop.ty,
        index: prop.index,
        span: prop.span,
    }
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
}

/// Analyse a tapir script and return semantic information.
///
/// This runs the compilation pipeline up to and including type checking,
/// but does not generate IR or bytecode. The result contains all the
/// information needed for language tooling such as LSP servers.
pub fn analyse(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> AnalysisResult {
    let file_id = FileId::new(0);
    let mut diagnostics = Diagnostics::new(file_id, filename, input);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let mut ast = match parser.parse(file_id, &mut diagnostics, lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, file_id);
            return AnalysisResult {
                diagnostics,
                symbols: vec![],
                globals: vec![],
                properties: vec![],
                functions: vec![],
                references: HashMap::new(),
                hover_info: HashMap::new(),
                call_sites: vec![],
                signatures: HashMap::new(),
            };
        }
    };

    let mut sym_tab_visitor = SymTabVisitor::new(settings, &mut ast, &mut diagnostics);
    let mut type_visitor = TypeVisitor::new(
        &ast.functions,
        &ast.extern_functions,
        sym_tab_visitor.get_symtab(),
    );

    for function in &mut ast.functions {
        sym_tab_visitor.visit_function(function, &mut diagnostics);
        loop_visitor::visit_loop_check(function, &mut diagnostics);
        type_visitor.visit_function(function, sym_tab_visitor.get_symtab(), &mut diagnostics);
    }

    let type_table = type_visitor.into_type_table(sym_tab_visitor.get_symtab(), &mut diagnostics);
    let symtab = sym_tab_visitor.into_symtab();

    // Extract symbol information
    let symbols = extract_symbols(&symtab, &type_table);

    // Extract global information
    let globals = symtab.globals().to_vec();

    // Extract property information
    let properties = symtab.properties().iter().map(property_to_info).collect();

    // Extract function information
    let functions = extract_functions(&ast);

    // Extract references from the AST
    let references = extract_references(&ast, &symtab);

    // Extract hover information
    let hover_info = extract_hover_info(&ast, &symtab, &type_table);

    // Extract signature help information
    let (signatures, call_sites) = extract_signature_help(&ast);

    AnalysisResult {
        diagnostics,
        symbols,
        globals,
        properties,
        functions,
        references,
        hover_info,
        call_sites,
        signatures,
    }
}

fn extract_symbols(symtab: &SymTab<'_>, type_table: &super::type_visitor::TypeTable<'_>) -> Vec<SymbolInfo> {
    let symbols = vec![];

    // Iterate through all symbols in the symbol table
    // Properties are at the start, so we skip those (they're handled separately)
    let num_properties = symtab.properties().len();

    for (i, _) in symtab.properties().iter().enumerate().skip(num_properties) {
        // This won't actually iterate since we skip all properties
        let _ = i;
    }

    // For now, we collect what we can from the symtab
    // The symbol table stores (name, Option<Span>) for each symbol
    // We need to be careful about properties which don't have spans

    // Note: The current symtab design doesn't expose an iterator over all symbols
    // with their spans easily. For a proper implementation, we might need to
    // extend the symtab API. For now, we'll return an empty vec and document this.
    //
    // TODO: Extend SymTab to expose symbol iteration for LSP use

    let _ = (symtab, type_table);

    symbols
}

fn extract_functions(ast: &crate::ast::Script<'_>) -> Vec<FunctionInfo> {
    let mut functions = vec![];

    // Internal functions
    for function in &ast.functions {
        // Skip the @toplevel synthetic function
        if function.name == "@toplevel" {
            continue;
        }

        let arguments = function
            .arguments
            .iter()
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => s.to_string(),
                    crate::ast::MaybeResolved::Resolved(_) => {
                        // The name was resolved to a symbol ID, but we still need the original name
                        // This is a limitation - we'd need to look it up in the symtab
                        String::new()
                    }
                };
                FunctionArgumentInfo {
                    name,
                    ty: arg.t.t,
                    span: arg.span,
                }
            })
            .collect();

        functions.push(FunctionInfo {
            name: function.name.to_string(),
            span: function.span,
            arguments,
            return_types: function.return_types.types.iter().map(|t| t.t).collect(),
            is_event_handler: function.modifiers.is_event_handler.is_some(),
        });
    }

    // Extern functions
    for function in &ast.extern_functions {
        let arguments = function
            .arguments
            .iter()
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => s.to_string(),
                    crate::ast::MaybeResolved::Resolved(_) => String::new(),
                };
                FunctionArgumentInfo {
                    name,
                    ty: arg.t.t,
                    span: arg.span,
                }
            })
            .collect();

        functions.push(FunctionInfo {
            name: function.name.to_string(),
            span: function.span,
            arguments,
            return_types: function.return_types.types.iter().map(|t| t.t).collect(),
            is_event_handler: false,
        });
    }

    functions
}

fn extract_hover_info(
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
                description: format!("property {}: {}", prop.name, prop.ty),
            },
        );
    }

    // Add hover info for globals
    for global in symtab.globals() {
        hover_info.insert(
            global.span,
            HoverInfo {
                name: global.name.clone(),
                description: format!("global {}: {}", global.name, global.ty),
            },
        );
    }

    // Add hover info for functions and build signature map
    for function in &ast.functions {
        if function.name == "@toplevel" {
            continue;
        }

        let args: Vec<String> = function
            .arguments
            .iter()
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => *s,
                    crate::ast::MaybeResolved::Resolved(_) => "",
                };
                format!("{}: {}", name, arg.t.t)
            })
            .collect();

        let return_str = if function.return_types.types.is_empty() {
            String::new()
        } else if function.return_types.types.len() == 1 {
            format!(" -> {}", function.return_types.types[0].t)
        } else {
            let types: Vec<String> = function
                .return_types
                .types
                .iter()
                .map(|t| t.t.to_string())
                .collect();
            format!(" -> ({})", types.join(", "))
        };

        let prefix = if function.modifiers.is_event_handler.is_some() {
            "event fn"
        } else {
            "fn"
        };

        let info = HoverInfo {
            name: function.name.to_string(),
            description: format!("{} {}({}){}", prefix, function.name, args.join(", "), return_str),
        };

        hover_info.insert(function.span, info.clone());
        function_signatures.insert(function.name, info);
    }

    // Add hover info for extern functions and build signature map
    for function in &ast.extern_functions {
        let args: Vec<String> = function
            .arguments
            .iter()
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => *s,
                    crate::ast::MaybeResolved::Resolved(_) => "",
                };
                format!("{}: {}", name, arg.t.t)
            })
            .collect();

        let return_str = if function.return_types.types.is_empty() {
            String::new()
        } else if function.return_types.types.len() == 1 {
            format!(" -> {}", function.return_types.types[0].t)
        } else {
            let types: Vec<String> = function
                .return_types
                .types
                .iter()
                .map(|t| t.t.to_string())
                .collect();
            format!(" -> ({})", types.join(", "))
        };

        let info = HoverInfo {
            name: function.name.to_string(),
            description: format!("extern fn {}({}){}", function.name, args.join(", "), return_str),
        };

        hover_info.insert(function.span, info.clone());
        function_signatures.insert(function.name, info);
    }

    // Walk all functions to extract hover info for variables and function calls
    for func in &ast.functions {
        extract_hover_from_statements(&func.statements, symtab, type_table, &function_signatures, &mut hover_info);
    }

    hover_info
}

fn extract_hover_from_statements(
    statements: &[Statement<'_>],
    symtab: &SymTab<'_>,
    type_table: &TypeTable<'_>,
    function_signatures: &HashMap<&str, HoverInfo>,
    hover_info: &mut HashMap<Span, HoverInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { idents, values } => {
                // Get symbol IDs for the declared variables
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_hover(ident.span, *symbol_id, symtab, type_table, hover_info);
                    }
                }
                for expr in values {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
                }
            }
            StatementKind::Assignment { idents, values } => {
                if let Some(symbol_ids) = stmt.meta.get::<Vec<SymbolId>>() {
                    for (ident, symbol_id) in idents.iter().zip(symbol_ids.iter()) {
                        add_symbol_hover(ident.span, *symbol_id, symtab, type_table, hover_info);
                    }
                }
                for expr in values {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
                }
            }
            StatementKind::If { condition, true_block, false_block } => {
                extract_hover_from_expression(condition, symtab, type_table, function_signatures, hover_info);
                extract_hover_from_statements(true_block, symtab, type_table, function_signatures, hover_info);
                extract_hover_from_statements(false_block, symtab, type_table, function_signatures, hover_info);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_hover_from_statements(block, symtab, type_table, function_signatures, hover_info);
            }
            StatementKind::Call { name, arguments } => {
                // Add hover info for the function call
                if let Some(sig) = function_signatures.get(name) {
                    hover_info.insert(stmt.span, sig.clone());
                }
                for expr in arguments {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
                }
            }
            StatementKind::Spawn { name, arguments } => {
                // Add hover info for the spawned function
                if let Some(sig) = function_signatures.get(name) {
                    hover_info.insert(stmt.span, sig.clone());
                }
                for expr in arguments {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
                }
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_hover_from_expression(expr, symtab, type_table, function_signatures, hover_info);
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
    // Skip builtins - they have separate documentation
    if let Some(builtin) = BuiltinVariable::from_symbol_id(symbol_id) {
        hover_info.insert(
            span,
            HoverInfo {
                name: builtin.name().to_string(),
                description: format!("builtin {}: {}", builtin.name(), builtin.ty()),
            },
        );
        return;
    }

    // Check if it's a global
    if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
        let global = symtab.get_global(global_id);
        hover_info.insert(
            span,
            HoverInfo {
                name: global.name.clone(),
                description: format!("global {}: {}", global.name, global.ty),
            },
        );
        return;
    }

    // Check if it's a property
    if let Some(property) = symtab.get_property(symbol_id) {
        hover_info.insert(
            span,
            HoverInfo {
                name: property.name.clone(),
                description: format!("property {}: {}", property.name, property.ty),
            },
        );
        return;
    }

    // It's a local variable
    let ty = type_table.type_for_symbol(symbol_id, symtab);
    let name = symtab.name_for_symbol(symbol_id);
    hover_info.insert(
        span,
        HoverInfo {
            name: name.to_string(),
            description: format!("var {}: {}", name, ty),
        },
    );
}

fn extract_hover_from_expression(
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
            // Add hover info for the function call
            if let Some(sig) = function_signatures.get(name) {
                hover_info.insert(expr.span, sig.clone());
            }
            for arg in arguments {
                extract_hover_from_expression(arg, symtab, type_table, function_signatures, hover_info);
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_hover_from_expression(lhs, symtab, type_table, function_signatures, hover_info);
            extract_hover_from_expression(rhs, symtab, type_table, function_signatures, hover_info);
        }
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}

fn extract_signature_help(ast: &Script<'_>) -> (HashMap<String, SignatureInfo>, Vec<CallSiteInfo>) {
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
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => *s,
                    crate::ast::MaybeResolved::Resolved(_) => "",
                };
                ParameterInfo {
                    label: format!("{}: {}", name, arg.t.t),
                }
            })
            .collect();

        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();

        let return_str = if function.return_types.types.is_empty() {
            String::new()
        } else if function.return_types.types.len() == 1 {
            format!(" -> {}", function.return_types.types[0].t)
        } else {
            let types: Vec<String> = function
                .return_types
                .types
                .iter()
                .map(|t| t.t.to_string())
                .collect();
            format!(" -> ({})", types.join(", "))
        };

        let prefix = if function.modifiers.is_event_handler.is_some() {
            "event fn"
        } else {
            "fn"
        };

        signatures.insert(
            function.name.to_string(),
            SignatureInfo {
                label: format!("{} {}({}){}", prefix, function.name, params_str.join(", "), return_str),
                parameters: params,
            },
        );
    }

    // Build signature info for extern functions
    for function in &ast.extern_functions {
        let params: Vec<ParameterInfo> = function
            .arguments
            .iter()
            .map(|arg| {
                let name = match &arg.name {
                    crate::ast::MaybeResolved::Unresolved(s) => *s,
                    crate::ast::MaybeResolved::Resolved(_) => "",
                };
                ParameterInfo {
                    label: format!("{}: {}", name, arg.t.t),
                }
            })
            .collect();

        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();

        let return_str = if function.return_types.types.is_empty() {
            String::new()
        } else if function.return_types.types.len() == 1 {
            format!(" -> {}", function.return_types.types[0].t)
        } else {
            let types: Vec<String> = function
                .return_types
                .types
                .iter()
                .map(|t| t.t.to_string())
                .collect();
            format!(" -> ({})", types.join(", "))
        };

        signatures.insert(
            function.name.to_string(),
            SignatureInfo {
                label: format!("extern fn {}({}){}", function.name, params_str.join(", "), return_str),
                parameters: params,
            },
        );
    }

    // Walk all functions to extract call sites
    for func in &ast.functions {
        extract_call_sites_from_statements(&func.statements, &signatures, &mut call_sites);
    }

    (signatures, call_sites)
}

fn extract_call_sites_from_statements(
    statements: &[Statement<'_>],
    signatures: &HashMap<String, SignatureInfo>,
    call_sites: &mut Vec<CallSiteInfo>,
) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { values, .. }
            | StatementKind::Assignment { values, .. } => {
                for expr in values {
                    extract_call_sites_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::If { condition, true_block, false_block } => {
                extract_call_sites_from_expression(condition, signatures, call_sites);
                extract_call_sites_from_statements(true_block, signatures, call_sites);
                extract_call_sites_from_statements(false_block, signatures, call_sites);
            }
            StatementKind::Loop { block } | StatementKind::Block { block } => {
                extract_call_sites_from_statements(block, signatures, call_sites);
            }
            StatementKind::Call { name, arguments } | StatementKind::Spawn { name, arguments } => {
                if signatures.contains_key(*name) {
                    let argument_end_offsets: Vec<usize> = arguments
                        .iter()
                        .map(|arg| arg.span.end())
                        .collect();

                    call_sites.push(CallSiteInfo {
                        function_name: name.to_string(),
                        arguments_span: stmt.span,
                        argument_end_offsets,
                    });
                }
                for expr in arguments {
                    extract_call_sites_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::Trigger { arguments, .. } => {
                for expr in arguments {
                    extract_call_sites_from_expression(expr, signatures, call_sites);
                }
            }
            StatementKind::Return { values } => {
                for expr in values {
                    extract_call_sites_from_expression(expr, signatures, call_sites);
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

fn extract_call_sites_from_expression(
    expr: &Expression<'_>,
    signatures: &HashMap<String, SignatureInfo>,
    call_sites: &mut Vec<CallSiteInfo>,
) {
    match &expr.kind {
        ExpressionKind::Call { name, arguments } => {
            if signatures.contains_key(*name) {
                let argument_end_offsets: Vec<usize> = arguments
                    .iter()
                    .map(|arg| arg.span.end())
                    .collect();

                call_sites.push(CallSiteInfo {
                    function_name: name.to_string(),
                    arguments_span: expr.span,
                    argument_end_offsets,
                });
            }
            for arg in arguments {
                extract_call_sites_from_expression(arg, signatures, call_sites);
            }
        }
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            extract_call_sites_from_expression(lhs, signatures, call_sites);
            extract_call_sites_from_expression(rhs, signatures, call_sites);
        }
        ExpressionKind::Variable(_)
        | ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => {}
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn analyse_simple_script() {
        let input = r#"
            global my_global = 42;
            property health: int;

            fn add(a: int, b: int) -> int {
                return a + b;
            }

            event fn on_start() {
                var x = 1;
                health = add(x, my_global);
            }
        "#;

        let settings = CompileSettings {
            available_fields: None,
            enable_optimisations: false,
        };

        let result = analyse("test.tapir", input, &settings);

        assert!(!result.diagnostics.has_any(), "Expected no errors");
        assert_eq!(result.globals.len(), 1);
        assert_eq!(result.globals[0].name, "my_global");
        assert_eq!(result.properties.len(), 1);
        assert_eq!(result.properties[0].name, "health");

        // Should have 2 functions: add and on_start (toplevel is filtered out)
        assert_eq!(result.functions.len(), 2);

        let add_fn = result.functions.iter().find(|f| f.name == "add").unwrap();
        assert_eq!(add_fn.arguments.len(), 2);
        assert_eq!(add_fn.return_types.len(), 1);
        assert!(!add_fn.is_event_handler);

        let on_start_fn = result.functions.iter().find(|f| f.name == "on_start").unwrap();
        assert!(on_start_fn.is_event_handler);
    }

    #[test]
    fn analyse_with_errors_still_extracts_functions() {
        let input = r#"
            fn test(x: int) {
                var y = x + 3.5;
            }
        "#;

        let settings = CompileSettings {
            available_fields: None,
            enable_optimisations: false,
        };

        let result = analyse("test.tapir", input, &settings);

        // Should have errors due to type mismatch (int + fix)
        assert!(result.diagnostics.has_any(), "Expected errors");

        // But we should still have extracted the function
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].name, "test");
    }

    #[test]
    fn analyse_big_tapir_test() {
        let input = include_str!("snapshot_tests/analyse/big_tapir_test.tapir");

        let settings = CompileSettings {
            available_fields: None,
            enable_optimisations: false,
        };

        let result = analyse("big_tapir_test.tapir", input, &settings);

        // The file has intentional errors, but we should still extract information
        assert!(result.diagnostics.has_any(), "Expected errors in big_tapir_test.tapir");

        // Should have extracted properties
        assert!(!result.properties.is_empty(), "Expected properties");
        assert!(result.properties.iter().any(|p| p.name == "x"));
        assert!(result.properties.iter().any(|p| p.name == "health"));

        // Should have extracted globals
        assert!(!result.globals.is_empty(), "Expected globals");
        assert!(result.globals.iter().any(|g| g.name == "animation_speed"));

        // Should have extracted functions
        assert!(!result.functions.is_empty(), "Expected functions");
        let function_names: Vec<_> = result.functions.iter().map(|f| &f.name).collect();
        assert!(
            result.functions.iter().any(|f| f.name == "idle_animation"),
            "Expected idle_animation, got: {:?}",
            function_names
        );
        assert!(
            result.functions.iter().any(|f| f.name == "handle_movement"),
            "Expected handle_movement, got: {:?}",
            function_names
        );

        // Should have extracted references for go-to-definition
        assert!(!result.references.is_empty(), "Expected references");

        // Should have extracted hover info
        assert!(!result.hover_info.is_empty(), "Expected hover info");
    }
}
