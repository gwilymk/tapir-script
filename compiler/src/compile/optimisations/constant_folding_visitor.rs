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
    
    macro_rules! replace_op {
        ($lhs:expr, $op:expr, $rhs:expr) => {{
            lhs.kind = $lhs;
            rhs.kind = $rhs;
            *operator = $op;

            return ConstantOptimisationResult::DidSomething;
        }};
    }

    macro_rules! take_side {
        (lhs, $lhs:expr) => {{
            exp.meta = mem::take(&mut lhs.meta);
            $lhs
        }};

        (rhs, $rhs:expr) => {{
            exp.meta = mem::take(&mut rhs.meta);
            $rhs
        }};
    }

    exp.kind = match (mem::take(&mut lhs.kind), *operator, mem::take(&mut rhs.kind)) {
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
        (E::Integer(0), B::Add | B::Sub, any) => take_side!(rhs, any),
        (any, B::Add | B::Sub, E::Integer(0)) => take_side!(lhs, any),

        (any, B::Add | B::Sub, E::Fix(n)) if n == 0.into() => take_side!(lhs, any),
        (E::Fix(n), B::Add | B::Sub, any) if n == 0.into() => take_side!(rhs, any),

        // ==============================================
        // mulitply by zero
        // (5 + foo(x)) * 0 -> (5 + foo(x)) then 0
        // ==============================================
        (any, B::Mul, E::Integer(0)) => replace_op!(any, B::Then, E::Integer(0)),
        (any, B::FixMul | B::Mul, E::Fix(n)) if n == 0.into() => replace_op!(any, B::Then, E::Integer(0)),

        // ======================
        // multiply / divide by 1
        // ======================
        (any, B::Mul | B::Div | B::RealDiv, E::Integer(1)) => take_side!(lhs, any),
        (E::Integer(1), B::Mul, any) => take_side!(rhs, any),
        (any, B::FixDiv | B::FixMul | B::RealDiv, E::Fix(n)) if n == 1.into() => take_side!(lhs, any),
        (E::Fix(n), B::FixDiv | B::FixMul, any) if n == 1.into() => take_side!(rhs, any),

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

        // ==========================
        //         Association
        // (a + 1) + 2 => a + (1 + 2)
        // ==========================
        (
            E::BinaryOperation { lhs: a, operator: op1 @ (B::Add | B::Mul | B::FixMul), rhs: b }, 
            op2 @ (B::Add | B::Mul | B::FixMul | B::Sub), 
            c @ (E::Integer(_) | E::Fix(_))
        )
            if matches!(b.kind, E::Fix(_) | E::Integer(_)) && (op1 == op2 || matches!((op1, op2), (B::Add, B::Sub))) =>
                replace_op!(a.kind, op1, 
                    E::BinaryOperation { 
                        lhs: b, 
                        operator: op2, 
                        rhs: Box::new(c.with_span(a.span.file_id, a.span.start, a.span.end)),
                    }),
            
        // ==========================
        // Fix multiply by an integer
        // ==========================
        (any, B::FixMul, E::Fix(n))
            if n.frac() == 0 => replace_op!(any, B::Mul, E::Integer(n.floor())),

        // ==================================
        // Canonicalise commutative operators
        // (1 + a) + 2 => (a + 1) + 2
        // ==================================
        (llhs @ (E::Fix(_) | E::Integer(_)), B::Add | B::Mul | B::FixMul, rrhs)
            if !matches!(rrhs, E::Fix(_) | E::Integer(_)) => 
                {
                    std::mem::swap(&mut lhs.meta, &mut rhs.meta);
                    lhs.kind = rrhs;
                    rhs.kind = llhs;

                    return ConstantOptimisationResult::DidSomething;
                },

        // Put it back the way it was
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
        glob!("snapshot_tests", "constant_folding/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let compile_settings = CompileSettings {
                properties: vec![
                    Property {
                        ty: Type::Int,
                        index: 0,
                        name: "int_prop".to_owned(),
                    },
                    Property {
                        ty: Type::Fix,
                        index: 1,
                        name: "fix_prop".to_owned(),
                    },
                    Property {
                        ty: Type::Bool,
                        index: 2,
                        name: "bool_prop".to_owned(),
                    },
                ],
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

                while constant_fold(function, &mut diagnostics)
                    == ConstantOptimisationResult::DidSomething
                {}
            }

            let pretty_printed = script.pretty_print();

            assert_snapshot!(pretty_printed);
        });
    }
}
