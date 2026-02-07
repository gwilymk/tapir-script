//! AST-level constant expression evaluator.
//!
//! This module provides compile-time evaluation of constant expressions in the AST,
//! enabling expressions like `-5`, `1 + 2`, and `!true` to be evaluated before IR generation.
//! This is distinct from IR constant folding which operates on IR instructions after lowering.

use agb_fixnum::Num;

use crate::{
    ast::{BinaryOperator, Expression, ExpressionKind, UnaryOperator},
    tokens::Span,
};

/// A compile-time constant value.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstantValue {
    Int(i32),
    Fix(Num<i32, 8>),
    Bool(bool),
}

/// Errors that can occur during constant expression evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantEvalError {
    /// Expression is not a compile-time constant (e.g., contains variables or function calls).
    NotConstant { span: Span },
    /// Division by zero in constant expression.
    DivisionByZero { span: Span },
    /// Integer overflow in constant expression.
    IntegerOverflow { span: Span },
    /// Type mismatch in constant expression (e.g., `1 + true`).
    TypeMismatch {
        span: Span,
        expected: &'static str,
        found: &'static str,
    },
}

/// Attempt to evaluate an AST expression as a compile-time constant.
///
/// Returns `Ok(ConstantValue)` if the expression can be evaluated at compile time,
/// or `Err(ConstantEvalError)` if not.
pub fn eval_constant_expr(expr: &Expression<'_>) -> Result<ConstantValue, ConstantEvalError> {
    match &expr.kind {
        // Literals
        ExpressionKind::Integer(i) => Ok(ConstantValue::Int(*i)),
        ExpressionKind::Fix(f) => Ok(ConstantValue::Fix(*f)),
        ExpressionKind::Bool(b) => Ok(ConstantValue::Bool(*b)),

        // Binary operations
        ExpressionKind::BinaryOperation { operator, lhs, rhs } => {
            let l = eval_constant_expr(lhs)?;
            let r = eval_constant_expr(rhs)?;
            eval_binary_op(*operator, l, r, expr.span)
        }

        // Unary operations
        ExpressionKind::UnaryOperation { operator, operand } => {
            let val = eval_constant_expr(operand)?;
            eval_unary_op(*operator, val, expr.span)
        }

        // Everything else is not a constant expression
        ExpressionKind::Variable(_)
        | ExpressionKind::Call { .. }
        | ExpressionKind::FieldAccess { .. }
        | ExpressionKind::MethodCall { .. }
        | ExpressionKind::Spawn { .. }
        | ExpressionKind::Error
        | ExpressionKind::Nop => Err(ConstantEvalError::NotConstant { span: expr.span }),
        ExpressionKind::SpawnBlock { .. } => {
            unreachable!("SpawnBlock should have been desugared before this point")
        }
    }
}

/// Evaluate a binary operation on two constant values.
fn eval_binary_op(
    op: BinaryOperator,
    lhs: ConstantValue,
    rhs: ConstantValue,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use BinaryOperator as B;
    use ConstantValue as C;

    match (lhs, op, rhs) {
        // Integer operations
        (C::Int(i1), op, C::Int(i2)) => eval_int_op(i1, op, i2, span),

        // Fix operations
        (C::Fix(f1), op, C::Fix(f2)) => eval_fix_op(f1, op, f2, span),

        // Fix/int mixed operations
        (C::Fix(f), op, C::Int(i)) => eval_fix_int_op(f, op, i, span),

        // Boolean operations
        (C::Bool(b1), B::And, C::Bool(b2)) => Ok(C::Bool(b1 && b2)),
        (C::Bool(b1), B::Or, C::Bool(b2)) => Ok(C::Bool(b1 || b2)),
        (C::Bool(b1), B::EqEq, C::Bool(b2)) => Ok(C::Bool(b1 == b2)),
        (C::Bool(b1), B::NeEq, C::Bool(b2)) => Ok(C::Bool(b1 != b2)),

        // Type mismatches
        (C::Int(_), _, C::Bool(_)) | (C::Bool(_), _, C::Int(_)) => {
            Err(ConstantEvalError::TypeMismatch {
                span,
                expected: "matching types",
                found: "int and bool",
            })
        }
        // Note: (C::Fix, _, C::Int) is handled by eval_fix_int_op above for valid operations.
        // For invalid operations (like comparison), eval_fix_int_op returns an error.
        (C::Int(_), _, C::Fix(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "matching types",
            found: "int and fix",
        }),
        (C::Fix(_), _, C::Bool(_)) | (C::Bool(_), _, C::Fix(_)) => {
            Err(ConstantEvalError::TypeMismatch {
                span,
                expected: "matching types",
                found: "fix and bool",
            })
        }
        _ => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "matching types",
            found: "mismatched types",
        }),
    }
}

/// Evaluate a binary operation on two integers.
fn eval_int_op(
    i1: i32,
    op: BinaryOperator,
    i2: i32,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use BinaryOperator as B;
    use ConstantValue as C;

    match op {
        B::Add => i1
            .checked_add(i2)
            .map(C::Int)
            .ok_or(ConstantEvalError::IntegerOverflow { span }),
        B::Sub => i1
            .checked_sub(i2)
            .map(C::Int)
            .ok_or(ConstantEvalError::IntegerOverflow { span }),
        B::Mul => i1
            .checked_mul(i2)
            .map(C::Int)
            .ok_or(ConstantEvalError::IntegerOverflow { span }),
        B::Div => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Int(i1.div_euclid(i2)))
            }
        }
        B::Mod => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Int(i1.rem_euclid(i2)))
            }
        }
        B::RealDiv => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                i1.checked_div(i2)
                    .map(C::Int)
                    .ok_or(ConstantEvalError::IntegerOverflow { span })
            }
        }
        B::RealMod => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                i1.checked_rem(i2)
                    .map(C::Int)
                    .ok_or(ConstantEvalError::IntegerOverflow { span })
            }
        }
        B::EqEq => Ok(C::Bool(i1 == i2)),
        B::NeEq => Ok(C::Bool(i1 != i2)),
        B::Gt => Ok(C::Bool(i1 > i2)),
        B::GtEq => Ok(C::Bool(i1 >= i2)),
        B::Lt => Ok(C::Bool(i1 < i2)),
        B::LtEq => Ok(C::Bool(i1 <= i2)),
        B::Shl => {
            let shift = i2 & 31;
            Ok(C::Int(i1 << shift))
        }
        B::Shr => {
            let shift = i2 & 31;
            Ok(C::Int(i1 >> shift))
        }
        B::BitAnd => Ok(C::Int(i1 & i2)),
        B::BitOr => Ok(C::Int(i1 | i2)),
        B::Then => Ok(C::Int(i2)),
        // These operators should not be used with two integers
        B::FixMul | B::FixDiv => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "fix",
            found: "int",
        }),
        B::And | B::Or => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "bool",
            found: "int",
        }),
    }
}

/// Evaluate a binary operation on two fixed-point numbers.
fn eval_fix_op(
    n1: Num<i32, 8>,
    op: BinaryOperator,
    n2: Num<i32, 8>,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use BinaryOperator as B;
    use ConstantValue as C;

    let f0 = Num::new(0);

    match op {
        B::Add => Ok(C::Fix(n1 + n2)),
        B::Sub => Ok(C::Fix(n1 - n2)),
        B::FixMul => Ok(C::Fix(n1 * n2)),
        B::FixDiv => {
            if n2 == f0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Fix(n1 / n2))
            }
        }
        B::EqEq => Ok(C::Bool(n1 == n2)),
        B::NeEq => Ok(C::Bool(n1 != n2)),
        B::Gt => Ok(C::Bool(n1 > n2)),
        B::GtEq => Ok(C::Bool(n1 >= n2)),
        B::Lt => Ok(C::Bool(n1 < n2)),
        B::LtEq => Ok(C::Bool(n1 <= n2)),
        B::Then => Ok(C::Fix(n2)),
        // These operators are not valid for two fix values
        B::Mul | B::Div | B::Mod | B::RealDiv | B::RealMod => {
            Err(ConstantEvalError::TypeMismatch {
                span,
                expected: "int",
                found: "fix",
            })
        }
        B::Shl | B::Shr | B::BitAnd | B::BitOr => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int",
            found: "fix",
        }),
        B::And | B::Or => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "bool",
            found: "fix",
        }),
    }
}

/// Evaluate a binary operation on a fixed-point number and an integer.
fn eval_fix_int_op(
    n1: Num<i32, 8>,
    op: BinaryOperator,
    i2: i32,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use BinaryOperator as B;
    use ConstantValue as C;

    match op {
        B::Add => Ok(C::Fix(n1 + i2)),
        B::Sub => Ok(C::Fix(n1 - i2)),
        B::Mul => Ok(C::Fix(n1 * i2)),
        B::Div => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Fix(n1 / i2))
            }
        }
        B::Mod => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Fix(n1.rem_euclid(i2.into())))
            }
        }
        B::RealDiv => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Fix(n1 / i2))
            }
        }
        B::RealMod => {
            if i2 == 0 {
                Err(ConstantEvalError::DivisionByZero { span })
            } else {
                Ok(C::Fix(n1 % i2))
            }
        }
        B::Then => Ok(C::Int(i2)),
        // Type mismatches
        _ => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "matching types",
            found: "fix and int",
        }),
    }
}

/// Evaluate a unary operation on a constant value.
fn eval_unary_op(
    op: UnaryOperator,
    val: ConstantValue,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use ConstantValue as C;

    match (op, val) {
        (UnaryOperator::Neg, C::Int(v)) => v
            .checked_neg()
            .map(C::Int)
            .ok_or(ConstantEvalError::IntegerOverflow { span }),
        (UnaryOperator::Neg, C::Fix(v)) => Ok(C::Fix(-v)),
        (UnaryOperator::Not, C::Bool(v)) => Ok(C::Bool(!v)),
        (UnaryOperator::BitNot, C::Int(v)) => Ok(C::Int(!v)),
        // Type mismatches
        (UnaryOperator::Neg, C::Bool(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int or fix",
            found: "bool",
        }),
        (UnaryOperator::Not, C::Int(_) | C::Fix(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "bool",
            found: "int or fix",
        }),
        (UnaryOperator::BitNot, C::Bool(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int",
            found: "bool",
        }),
        (UnaryOperator::BitNot, C::Fix(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int",
            found: "fix",
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::FileId;

    /// Helper to create a test expression from ExpressionKind
    fn make_expr(kind: ExpressionKind<'static>) -> Expression<'static> {
        Expression {
            kind,
            span: Span::new(FileId::new(0), 0, 0),
            meta: Default::default(),
        }
    }

    fn int_expr(i: i32) -> Expression<'static> {
        make_expr(ExpressionKind::Integer(i))
    }

    fn fix_expr(f: f32) -> Expression<'static> {
        make_expr(ExpressionKind::Fix(Num::from_f32(f)))
    }

    fn bool_expr(b: bool) -> Expression<'static> {
        make_expr(ExpressionKind::Bool(b))
    }

    fn binop_expr<'a>(
        lhs: Expression<'a>,
        op: BinaryOperator,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        Expression {
            kind: ExpressionKind::BinaryOperation {
                operator: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span: Span::new(FileId::new(0), 0, 0),
            meta: Default::default(),
        }
    }

    #[test]
    fn test_literal_int() {
        let expr = int_expr(42);
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(42)));
    }

    #[test]
    fn test_literal_fix() {
        let expr = fix_expr(3.5);
        let result = eval_constant_expr(&expr);
        assert!(matches!(result, Ok(ConstantValue::Fix(_))));
    }

    #[test]
    fn test_literal_bool() {
        let expr = bool_expr(true);
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Bool(true)));
    }

    #[test]
    fn test_int_addition() {
        let expr = binop_expr(int_expr(1), BinaryOperator::Add, int_expr(2));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(3)));
    }

    #[test]
    fn test_int_subtraction() {
        let expr = binop_expr(int_expr(10), BinaryOperator::Sub, int_expr(3));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(7)));
    }

    #[test]
    fn test_int_multiplication() {
        let expr = binop_expr(int_expr(4), BinaryOperator::Mul, int_expr(5));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(20)));
    }

    #[test]
    fn test_int_division() {
        let expr = binop_expr(int_expr(10), BinaryOperator::Div, int_expr(3));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(3)));
    }

    #[test]
    fn test_int_modulo() {
        let expr = binop_expr(int_expr(10), BinaryOperator::Mod, int_expr(3));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(1)));
    }

    #[test]
    fn test_division_by_zero() {
        let expr = binop_expr(int_expr(1), BinaryOperator::Div, int_expr(0));
        let result = eval_constant_expr(&expr);
        assert!(matches!(
            result,
            Err(ConstantEvalError::DivisionByZero { .. })
        ));
    }

    #[test]
    fn test_integer_overflow() {
        let expr = binop_expr(int_expr(i32::MAX), BinaryOperator::Add, int_expr(1));
        let result = eval_constant_expr(&expr);
        assert!(matches!(
            result,
            Err(ConstantEvalError::IntegerOverflow { .. })
        ));
    }

    #[test]
    fn test_nested_expression() {
        // (1 + 2) * 3 = 9
        let add = binop_expr(int_expr(1), BinaryOperator::Add, int_expr(2));
        let expr = binop_expr(add, BinaryOperator::Mul, int_expr(3));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(9)));
    }

    #[test]
    fn test_comparison() {
        let expr = binop_expr(int_expr(5), BinaryOperator::Gt, int_expr(3));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Bool(true)));
    }

    #[test]
    fn test_boolean_and() {
        let expr = binop_expr(bool_expr(true), BinaryOperator::And, bool_expr(false));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Bool(false)));
    }

    #[test]
    fn test_boolean_or() {
        let expr = binop_expr(bool_expr(true), BinaryOperator::Or, bool_expr(false));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Bool(true)));
    }

    #[test]
    fn test_variable_not_constant() {
        let expr = make_expr(ExpressionKind::Variable("x"));
        let result = eval_constant_expr(&expr);
        assert!(matches!(result, Err(ConstantEvalError::NotConstant { .. })));
    }

    #[test]
    fn test_bitwise_operations() {
        // 0xFF & 0x0F = 0x0F
        let expr = binop_expr(int_expr(0xFF), BinaryOperator::BitAnd, int_expr(0x0F));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(0x0F)));

        // 0xF0 | 0x0F = 0xFF
        let expr = binop_expr(int_expr(0xF0), BinaryOperator::BitOr, int_expr(0x0F));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(0xFF)));

        // 1 << 4 = 16
        let expr = binop_expr(int_expr(1), BinaryOperator::Shl, int_expr(4));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(16)));

        // 16 >> 2 = 4
        let expr = binop_expr(int_expr(16), BinaryOperator::Shr, int_expr(2));
        assert_eq!(eval_constant_expr(&expr), Ok(ConstantValue::Int(4)));
    }
}
