use std::path::Path;

use crate::{
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::{FileId, Span},
    types::Type,
    PropertyInfo,
};

use super::{
    loop_visitor,
    symtab_visitor::{GlobalInfo, SymTab, SymTabVisitor},
    type_visitor::TypeVisitor,
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

    AnalysisResult {
        diagnostics,
        symbols,
        globals,
        properties,
        functions,
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
}
