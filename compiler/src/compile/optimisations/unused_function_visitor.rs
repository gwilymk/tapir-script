use petgraph::prelude::*;

use serde::Serialize;

use crate::ast::{ExpressionKind, Function, FunctionId, StatementKind};

#[derive(Debug, Clone, Copy, Serialize)]
pub struct UnusedFunction;

pub fn unused_function_visitor(functions: &mut [Function]) {
    let mut call_graph = DiGraphMap::new();

    let mut roots = vec![FunctionId(0)];

    for function in functions.iter_mut() {
        let function_id: FunctionId = *function.meta.get().unwrap();

        if function.modifiers.is_event_handler.is_some() {
            roots.push(function_id);
        }

        call_graph.add_node(function_id);

        for statement in &mut function.statements {
            if matches!(statement.kind, StatementKind::Call { .. }) {
                call_graph.add_edge(function_id, *statement.meta.get().unwrap(), ());
            }

            for expr in statement.expressions_mut() {
                if matches!(expr.kind, ExpressionKind::Call { .. }) {
                    call_graph.add_edge(function_id, *expr.meta.get().unwrap(), ());
                }
            }
        }
    }

    let mut called_functions = Dfs::empty(&call_graph);

    for root in roots {
        called_functions.move_to(root);
        while called_functions.next(&call_graph).is_some() {}
    }

    let called_functions = called_functions.discovered;

    for function in functions.iter_mut() {
        let function_id: FunctionId = *function.meta.get().unwrap();

        if !called_functions.contains(&function_id) {
            function.meta.set(UnusedFunction);
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_snapshot, glob};

    use crate::{
        compile::{
            loop_visitor::visit_loop_check, symtab_visitor::SymTabVisitor,
            type_visitor::TypeVisitor,
        },
        grammar,
        lexer::Lexer,
        reporting::Diagnostics,
        tokens::FileId,
        CompileSettings,
    };

    use super::*;

    #[test]
    fn unused_function_snapshot_tests() {
        glob!("snapshot_tests", "unused_function/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let compile_settings = CompileSettings {
                properties: Vec::new(),
                enable_optimisations: true,
            };

            let mut symtab_visitor =
                SymTabVisitor::new(&compile_settings, &mut script.functions, &mut diagnostics);
            let mut type_visitor = TypeVisitor::new(&compile_settings, &script.functions);

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            unused_function_visitor(&mut script.functions);

            let pretty_printed = script.pretty_print();

            assert_snapshot!(pretty_printed);
        });
    }
}
