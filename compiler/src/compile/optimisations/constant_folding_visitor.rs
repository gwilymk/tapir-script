use std::{mem, ops::BitOr};

use crate::ast::{BinaryOperator, Expression, ExpressionKind, Function};

use super::ConstantOptimisationResult;

pub fn constant_fold(function: &mut Function) -> ConstantOptimisationResult {
    function
        .statements
        .iter_mut()
        .flat_map(|statement| statement.expressions_mut())
        .map(fold)
        .reduce(BitOr::bitor)
        .unwrap_or(ConstantOptimisationResult::DidNothing)
}

fn fold(exp: &mut Expression) -> ConstantOptimisationResult {
    let ExpressionKind::BinaryOperation { lhs, operator, rhs } = &mut exp.kind else {
        return ConstantOptimisationResult::DidNothing;
    };

    let did_something = fold(lhs) | fold(rhs);

    use BinaryOperator as B;
    use ExpressionKind as E;

    exp.kind = match (mem::take(&mut lhs.kind), operator, mem::take(&mut rhs.kind)) {
        (E::Integer(lhs), B::Add, E::Integer(rhs)) => E::Integer(lhs + rhs),
        (any, B::Add | B::Sub, E::Integer(0)) => any,
        (_, B::Mul, E::Integer(0)) => E::Integer(0),

        (lhs_kind, _, rhs_kind) => {
            lhs.kind = lhs_kind;
            rhs.kind = rhs_kind;

            return did_something;
        }
    };

    ConstantOptimisationResult::DidSomething
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, glob};

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
        glob!("snapshot_tests", "constant_folding/*.tapir", |path| {
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

                constant_fold(function);
            }

            assert_ron_snapshot!(script, {
                ".**.span" => "[span]",
                ".**.meta" => "[meta]",
            });
        });
    }
}
