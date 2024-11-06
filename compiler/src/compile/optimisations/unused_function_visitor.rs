use petgraph::prelude::*;

use serde::Serialize;

use crate::ast::{ExpressionKind, Function, FunctionId, Statement, StatementKind};

use super::ConstantOptimisationResult;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct UnusedFunction;

pub fn unused_function_visitor(functions: &mut [Function]) -> ConstantOptimisationResult {
    let mut call_graph = DiGraphMap::new();

    let mut roots = vec![FunctionId(0)];

    for function in functions.iter_mut() {
        if function.meta.has::<UnusedFunction>() {
            continue; // don't need to inspect this since we already know it is unused
        }

        let function_id: FunctionId = *function.meta.get().unwrap();

        if function.modifiers.is_event_handler.is_some() {
            roots.push(function_id);
        }

        call_graph.add_node(function_id);

        visit_block(function_id, &function.statements, &mut call_graph);

        fn visit_block(
            calling_function: FunctionId,
            block: &[Statement],
            call_graph: &mut DiGraphMap<FunctionId, ()>,
        ) {
            macro_rules! visit_expr {
                ($expr:expr) => {
                    for expr in $expr.all_inner() {
                        if matches!(expr.kind, ExpressionKind::Call { .. }) {
                            if let Some(called_id) = expr.meta.get() {
                                call_graph.add_edge(calling_function, *called_id, ());
                            }
                        }
                    }
                };
            }

            macro_rules! visit_exprs {
                ($exprs:expr) => {
                    for expr in $exprs {
                        visit_expr!(expr);
                    }
                };
            }

            for statement in block {
                match &statement.kind {
                    StatementKind::Error
                    | StatementKind::Wait
                    | StatementKind::Continue
                    | StatementKind::Break
                    | StatementKind::Nop => continue,

                    StatementKind::VariableDeclaration { value, .. }
                    | StatementKind::Assignment { value, .. } => {
                        visit_expr!(value);
                    }
                    StatementKind::Loop { block } | StatementKind::Block { block } => {
                        visit_block(calling_function, block, call_graph)
                    }
                    StatementKind::If {
                        condition,
                        true_block,
                        false_block,
                    } => {
                        visit_expr!(condition);
                        visit_block(calling_function, true_block, call_graph);
                        visit_block(calling_function, false_block, call_graph);
                    }
                    StatementKind::Spawn { arguments, .. }
                    | StatementKind::Call { arguments, .. } => {
                        if let Some(called_id) = statement.meta.get() {
                            call_graph.add_edge(calling_function, *called_id, ());
                        }

                        visit_exprs!(arguments);
                    }
                    StatementKind::Return { values: arguments }
                    | StatementKind::Trigger { arguments, .. } => {
                        visit_exprs!(arguments);
                    }
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

    let mut did_something = ConstantOptimisationResult::DidNothing;
    for function in functions.iter_mut() {
        let function_id: FunctionId = *function.meta.get().unwrap();

        if !called_functions.contains(&function_id) && !function.meta.set(UnusedFunction) {
            // we've newly set a meta field
            did_something = ConstantOptimisationResult::DidSomething;
        }
    }

    did_something
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
