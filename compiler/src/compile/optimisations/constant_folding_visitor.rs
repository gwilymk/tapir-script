use std::{mem, ops::BitOr};

use crate::{
    ast::{BinaryOperator, Expression, ExpressionKind, Function},
    reporting::{CompilerErrorKind, Diagnostics},
};

use super::ConstantOptimisationResult;

pub fn constant_fold(
    function: &mut Function,
    diagnostics: &mut Diagnostics,
) -> ConstantOptimisationResult {
    function
        .statements
        .iter_mut()
        .flat_map(|statement| statement.expressions_mut())
        .map(|expr| fold(expr, diagnostics))
        .reduce(BitOr::bitor)
        .unwrap_or(ConstantOptimisationResult::DidNothing)
}

#[rustfmt::skip]
fn fold(exp: &mut Expression, diagnostics: &mut Diagnostics) -> ConstantOptimisationResult {
    let ExpressionKind::BinaryOperation { lhs, operator, rhs } = &mut exp.kind else {
        return ConstantOptimisationResult::DidNothing;
    };

    let did_something = fold(lhs, diagnostics) | fold(rhs, diagnostics);

    use BinaryOperator as B;
    use ExpressionKind as E;

    exp.kind = match (mem::take(&mut lhs.kind), operator, mem::take(&mut rhs.kind)) {
        // ========================
        // Integer maths operations
        // ========================
        (E::Integer(lhs), B::Add,              E::Integer(rhs)) => E::Integer(lhs + rhs),
        (E::Integer(lhs), B::Sub,              E::Integer(rhs)) => E::Integer(lhs - rhs),
        (E::Integer(lhs), B::Mul,              E::Integer(rhs)) => E::Integer(lhs * rhs),
        (E::Integer(lhs), B::Div | B::RealDiv, E::Integer(rhs)) => E::Integer(lhs / rhs), // FIXME: div_floor
        (E::Integer(lhs), B::Mod | B::RealMod, E::Integer(rhs)) => E::Integer(lhs.rem_euclid(rhs)),

        // ===================
        // Integer comparisons
        // ===================
        (E::Integer(lhs), B::EqEq, E::Integer(rhs)) => E::Bool(lhs == rhs),
        (E::Integer(lhs), B::NeEq, E::Integer(rhs)) => E::Bool(lhs != rhs),
        (E::Integer(lhs), B::Gt,   E::Integer(rhs)) => E::Bool(lhs >  rhs),
        (E::Integer(lhs), B::GtEq, E::Integer(rhs)) => E::Bool(lhs >= rhs),
        (E::Integer(lhs), B::Lt,   E::Integer(rhs)) => E::Bool(lhs <  rhs),
        (E::Integer(lhs), B::LtEq, E::Integer(rhs)) => E::Bool(lhs <= rhs),

        // ===============
        // Fix comparisons
        // ===============
        (E::Fix(lhs), B::EqEq, E::Fix(rhs)) => E::Bool(lhs == rhs),
        (E::Fix(lhs), B::NeEq, E::Fix(rhs)) => E::Bool(lhs != rhs),
        (E::Fix(lhs), B::Gt,   E::Fix(rhs)) => E::Bool(lhs >  rhs),
        (E::Fix(lhs), B::GtEq, E::Fix(rhs)) => E::Bool(lhs >= rhs),
        (E::Fix(lhs), B::Lt,   E::Fix(rhs)) => E::Bool(lhs <  rhs),
        (E::Fix(lhs), B::LtEq, E::Fix(rhs)) => E::Bool(lhs <= rhs),

        // ====================
        // Fix maths operations
        // ====================
        (E::Fix(lhs), B::Add,    E::Fix(rhs)) => E::Fix(lhs + rhs),
        (E::Fix(lhs), B::Sub,    E::Fix(rhs)) => E::Fix(lhs - rhs),
        (E::Fix(lhs), B::FixMul, E::Fix(rhs)) => E::Fix(lhs * rhs),
        (E::Fix(lhs), B::FixDiv, E::Fix(rhs)) => E::Fix(lhs / rhs),

        // ===================
        // add / subtract zero
        // ===================
        (E::Integer(0), B::Add | B::Sub, any) | (any, B::Add | B::Sub, E::Integer(0)) => any,
        (any, B::Add | B::Sub, E::Fix(n)) | (E::Fix(n), B::Add | B::Sub, any) if n == 0.into() => any,

        // ================
        // mulitply by zero
        // ================
        (_, B::Mul, E::Integer(0)) | (E::Integer(0), B::Mul, _) => E::Integer(0),
        (_, B::FixMul, E::Fix(n)) | (E::Fix(n), B::FixMul, _) if n == 0.into() => E::Fix(0.into()),

        // ======================
        // multiply / divide by 1
        // ======================
        (any, B::Mul | B::Div | B::RealDiv, E::Integer(1)) => any,
        (E::Integer(1), B::Mul, any) => any,
        (any, B::FixDiv | B::FixMul, E::Fix(n)) if n == 1.into() => any,
        (E::Fix(n), B::FixDiv | B::FixMul, any) if n == 1.into() => any,

        // ===========
        // Divide by 0
        // ===========
        (_, B::Div | B::RealDiv | B::Mod | B::RealMod, E::Integer(0)) => {
            diagnostics.add_message(CompilerErrorKind::DivideByZero.into_message(rhs.span));
            E::Error
        }
        (_, B::Div | B::RealDiv | B::Mod | B::RealMod, E::Fix(n)) if n == 0.into() => {
            diagnostics.add_message(CompilerErrorKind::DivideByZero.into_message(rhs.span));
            E::Error
        }

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

                constant_fold(function, &mut diagnostics);
            }

            assert_ron_snapshot!(script, {
                ".**.span" => "[span]",
                ".**.meta" => "[meta]",
            });
        });
    }
}
