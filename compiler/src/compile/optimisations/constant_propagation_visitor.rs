use std::ops::BitOr;

use constant_propagation_map::ConstantPropagationMap;

use crate::{
    ast::{Expression, ExpressionKind, Function, Statement, StatementKind},
    CompileSettings,
};

use super::ConstantOptimisationResult;

mod constant_propagation_map;

pub fn constant_propagation(
    function: &mut Function,
    compile_settings: &CompileSettings,
) -> ConstantOptimisationResult {
    let mut constant_symbol = ConstantPropagationMap::default();
    constant_propagation_block(
        &mut function.statements,
        &mut constant_symbol,
        compile_settings,
    )
}

fn constant_propagation_block(
    block: &mut [Statement],
    constant_symbols: &mut ConstantPropagationMap,
    compile_settings: &CompileSettings,
) -> ConstantOptimisationResult {
    block
        .iter_mut()
        .map(|statement| match &mut statement.kind {
            StatementKind::Error
            | StatementKind::Continue
            | StatementKind::Break
            | StatementKind::Nop => ConstantOptimisationResult::DidNothing,
            StatementKind::Wait => {
                constant_symbols.poison_properties(compile_settings);

                ConstantOptimisationResult::DidNothing
            }
            StatementKind::Assignment { value, .. }
            | StatementKind::VariableDeclaration { value, .. } => {
                let did_propagate =
                    constant_propagation_expr(value, constant_symbols, compile_settings);

                let symbol_id = statement.meta.get().unwrap();
                constant_symbols.set(*symbol_id, &value.kind);

                did_propagate
            }
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                let mut did_propagate =
                    constant_propagation_expr(condition, constant_symbols, compile_settings);
                let mut true_block_symbols = constant_symbols.snapshot();
                did_propagate |= constant_propagation_block(
                    true_block,
                    &mut true_block_symbols,
                    compile_settings,
                );
                let mut false_block_symbols = constant_symbols.snapshot();
                did_propagate |= constant_propagation_block(
                    false_block,
                    &mut false_block_symbols,
                    compile_settings,
                );

                constant_symbols.apply_poisons(&true_block_symbols);
                constant_symbols.apply_poisons(&false_block_symbols);

                did_propagate
            }
            StatementKind::Loop { block } => {
                let mut loop_block_symbols = ConstantPropagationMap::default();
                let did_propagate =
                    constant_propagation_block(block, &mut loop_block_symbols, compile_settings);
                constant_symbols.apply_poisons(&loop_block_symbols);
                did_propagate
            }
            StatementKind::Block { block } => {
                constant_propagation_block(block, constant_symbols, compile_settings)
            }
            StatementKind::Call { arguments, .. } => {
                let did_propagate = arguments
                    .iter_mut()
                    .map(|expr| constant_propagation_expr(expr, constant_symbols, compile_settings))
                    .reduce(BitOr::bitor)
                    .unwrap_or(ConstantOptimisationResult::DidNothing);
                constant_symbols.poison_properties(compile_settings);
                did_propagate
            }
            StatementKind::Spawn {
                arguments: values, ..
            }
            | StatementKind::Trigger {
                arguments: values, ..
            }
            | StatementKind::Return { values } => values
                .iter_mut()
                .map(|expr| constant_propagation_expr(expr, constant_symbols, compile_settings))
                .reduce(BitOr::bitor)
                .unwrap_or(ConstantOptimisationResult::DidNothing),
        })
        .reduce(BitOr::bitor)
        .unwrap_or(ConstantOptimisationResult::DidNothing)
}

fn constant_propagation_expr(
    expression: &mut Expression,
    constant_symbols: &mut ConstantPropagationMap,
    compile_settings: &CompileSettings,
) -> ConstantOptimisationResult {
    match &mut expression.kind {
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Error
        | ExpressionKind::Nop => ConstantOptimisationResult::DidNothing,
        ExpressionKind::BinaryOperation {
            ref mut lhs,
            ref mut rhs,
            ..
        } => {
            constant_propagation_expr(lhs, constant_symbols, compile_settings)
                | constant_propagation_expr(rhs, constant_symbols, compile_settings)
        }
        ExpressionKind::Call { arguments, .. } => {
            let did_propagate = arguments
                .iter_mut()
                .map(|expr| constant_propagation_expr(expr, constant_symbols, compile_settings))
                .reduce(BitOr::bitor)
                .unwrap_or(ConstantOptimisationResult::DidNothing);
            constant_symbols.poison_properties(compile_settings);
            did_propagate
        }
        ExpressionKind::Variable(_) => {
            let symbol = expression.meta.get().expect("Variable should have symbol");
            if let Some(constant) = constant_symbols.get(*symbol) {
                expression.kind = constant.into();
                ConstantOptimisationResult::DidSomething
            } else {
                ConstantOptimisationResult::DidNothing
            }
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
        CompileSettings, Property, Type,
    };

    use super::*;

    #[test]
    fn constant_propagation_snapshot_tests() {
        glob!("snapshot_tests", "constant_propagation/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let compile_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_owned(),
                }],
                enable_optimisations: true,
            };

            let mut symtab_visitor = SymTabVisitor::new(&compile_settings);
            let mut type_visitor =
                TypeVisitor::new(&compile_settings, &script.functions, &mut diagnostics);

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );

                constant_propagation(function, &compile_settings);
            }

            let pretty_printed = script.pretty_print();

            assert_snapshot!(pretty_printed);
        });
    }
}
