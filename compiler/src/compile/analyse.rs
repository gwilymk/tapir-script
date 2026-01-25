mod hover;
mod inlay_hints;
mod signature_help;
mod types;
mod util;

use std::{collections::HashMap, path::Path};

use crate::{
    PropertyInfo, ast::Script, prelude::{self, USER_FILE_ID}, reporting::Diagnostics,
};

use super::{
    CompileSettings, Property, loop_visitor,
    references::extract_references,
    symtab_visitor::{SymTab, SymTabVisitor},
    type_visitor::{TypeTable, TypeVisitor},
};

pub use types::{
    AnalysisResult, CallSiteInfo, FunctionArgumentInfo, FunctionInfo, HoverInfo, InlayHintInfo,
    ParameterInfo, SignatureInfo, SymbolInfo,
};

use hover::extract_hover_info;
use inlay_hints::extract_inlay_hints;
use signature_help::extract_signature_help;

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
    let mut diagnostics = Diagnostics::new(USER_FILE_ID, &filename, input);

    let mut ast = match prelude::parse_with_prelude(&filename, input, &mut diagnostics) {
        Some(ast) => ast,
        None => {
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
                inlay_hints: vec![],
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
    let functions = extract_functions(&ast, &symtab);

    // Extract references from the AST
    let references = extract_references(&ast, &symtab);

    // Extract hover information
    let hover_info = extract_hover_info(&ast, &symtab, &type_table);

    // Extract signature help information
    let (signatures, call_sites) = extract_signature_help(&ast);

    // Extract inlay hints for variable types
    let inlay_hints = extract_inlay_hints(&ast, &symtab, &type_table);

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
        inlay_hints,
    }
}

fn property_to_info(prop: &Property) -> PropertyInfo {
    PropertyInfo {
        name: prop.name.clone(),
        ty: prop.ty,
        index: prop.index,
        span: prop.span,
    }
}

fn extract_symbols(_symtab: &SymTab<'_>, _type_table: &TypeTable<'_>) -> Vec<SymbolInfo> {
    // TODO: Extend SymTab to expose symbol iteration for LSP use
    vec![]
}

fn extract_functions(ast: &Script<'_>, _symtab: &SymTab<'_>) -> Vec<FunctionInfo> {
    let mut functions = vec![];

    // Internal functions
    for function in &ast.functions {
        if function.name == "@toplevel" {
            continue;
        }

        let arguments = function
            .arguments
            .iter()
            .map(|arg| FunctionArgumentInfo {
                name: arg.name().to_string(),
                ty: arg.ty_required().t,
                span: arg.span(),
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
            .map(|arg| FunctionArgumentInfo {
                name: arg.name().to_string(),
                ty: arg.ty_required().t,
                span: arg.span(),
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

        let on_start_fn = result
            .functions
            .iter()
            .find(|f| f.name == "on_start")
            .unwrap();
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
        assert!(
            result.diagnostics.has_any(),
            "Expected errors in big_tapir_test.tapir"
        );

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
