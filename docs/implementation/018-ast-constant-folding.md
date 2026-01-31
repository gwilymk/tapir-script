# AST-Level Constant Folding

## Overview

Add a compile-time constant expression evaluator that operates on the AST, enabling expressions like `-5`, `!true`, and `1 + 2` to be evaluated before IR generation.

## Motivation

Currently, global variable initializers only accept literal values:

```tapir
global foo = 5;       # OK
global bar = -5;      # ERROR: not a constant (once unary operators are added)
global baz = 1 + 2;   # ERROR: not a constant
```

The problem is that `evaluate_constant_initializer` in `symtab_visitor.rs` only recognizes `ExpressionKind::Integer`, `ExpressionKind::Fix`, and `ExpressionKind::Bool`. Any other expression kind is rejected.

With the addition of unary operators (014-unary-operators.md), `-5` will parse as `UnaryOperation(Neg, Integer(5))` rather than `Integer(-5)`. This would break `global x = -5;` unless we extend constant evaluation.

Rather than special-casing unary operations, this plan introduces a general AST constant evaluator that can handle:

- Literals
- Unary operations (`-`, `!`, `~`)
- Binary operations (`+`, `-`, `*`, `/`, etc.)
- Parenthesized expressions (already handled by AST structure)

This is distinct from IR constant folding (`optimisations/constant_folding.rs`) which operates on IR instructions after lowering. AST constant folding runs earlier, during symbol table construction, and enables constant expressions in contexts that require compile-time values.

## Design

### New Type: `ConstantValue`

A simple enum representing evaluated constant values:

```rust
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstantValue {
    Int(i32),
    Fix(Num<i32, 8>),
    Bool(bool),
}
```

This mirrors `ir::Constant` but lives in the AST/compile layer and doesn't require IR dependencies.

### Core Function: `eval_constant_expr`

A recursive function that attempts to evaluate an AST expression to a constant:

```rust
pub fn eval_constant_expr(expr: &Expression<'_>) -> Result<ConstantValue, ConstantEvalError> {
    match &expr.kind {
        // Literals
        ExpressionKind::Integer(i) => Ok(ConstantValue::Int(*i)),
        ExpressionKind::Fix(f) => Ok(ConstantValue::Fix(*f)),
        ExpressionKind::Bool(b) => Ok(ConstantValue::Bool(*b)),

        // Unary operations
        ExpressionKind::UnaryOperation { operator, operand } => {
            let val = eval_constant_expr(operand)?;
            eval_unary_op(*operator, val, expr.span)
        }

        // Binary operations
        ExpressionKind::BinaryOperation { operator, lhs, rhs } => {
            let l = eval_constant_expr(lhs)?;
            let r = eval_constant_expr(rhs)?;
            eval_binary_op(*operator, l, r, expr.span)
        }

        // Parenthesized expressions are already unwrapped by the AST

        // Everything else is not a constant
        _ => Err(ConstantEvalError::NotConstant { span: expr.span }),
    }
}
```

This should live in its own file, `compiler/src/compile/constant_eval.rs`, with dedicated tests. To enable easy unit testing, the expression parser should be made public in the grammar so tests can evaluate single expressions directly.

### Error Type

```rust
#[derive(Debug, Clone)]
pub enum ConstantEvalError {
    /// Expression is not a compile-time constant
    NotConstant { span: Span },
    /// Division by zero
    DivisionByZero { span: Span },
    /// Integer overflow
    IntegerOverflow { span: Span },
    /// Type mismatch (e.g., `-true` or `!5`)
    TypeMismatch {
        span: Span,
        expected: &'static str,
        found: &'static str,
    },
}
```

These errors should be converted to proper compiler diagnostics when emitted.

### Unary Operation Evaluation

```rust
fn eval_unary_op(
    op: UnaryOperator,
    val: ConstantValue,
    span: Span,
) -> Result<ConstantValue, ConstantEvalError> {
    use ConstantValue as C;
    use UnaryOperator as U;

    match (op, val) {
        // Negation
        (U::Neg, C::Int(i)) => i
            .checked_neg()
            .map(C::Int)
            .ok_or(ConstantEvalError::IntegerOverflow { span }),
        (U::Neg, C::Fix(f)) => Ok(C::Fix(-f)),

        // Logical NOT
        (U::Not, C::Bool(b)) => Ok(C::Bool(!b)),

        // Bitwise NOT
        (U::BitNot, C::Int(i)) => Ok(C::Int(!i)),

        // Type mismatches
        (U::Neg, C::Bool(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int or fix",
            found: "bool",
        }),
        (U::Not, C::Int(_) | C::Fix(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "bool",
            found: "int or fix",
        }),
        (U::BitNot, C::Bool(_) | C::Fix(_)) => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "int",
            found: "bool or fix",
        }),
    }
}
```

### Binary Operation Evaluation

Reuse the logic from IR constant folding where possible:

```rust
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
        _ => Err(ConstantEvalError::TypeMismatch {
            span,
            expected: "matching types",
            found: "mismatched types",
        }),
    }
}
```

The helper functions `eval_int_op`, `eval_fix_op`, and `eval_fix_int_op` should be shared with `ir/optimisations/constant_folding.rs`. Extract the core evaluation logic into a common module (e.g., `compiler/src/compile/ops.rs`) that can be used by both AST constant evaluation and IR constant folding, parameterized by the error type.

## Integration with Global Initializers

Replace the current `evaluate_constant_initializer` function:

```rust
fn evaluate_constant_initializer(
    expr: &Expression<'_>,
    diagnostics: &mut Diagnostics,
    global_name: &str,
) -> (Type, i32) {
    match eval_constant_expr(expr) {
        Ok(ConstantValue::Int(i)) => (Type::Int, i),
        Ok(ConstantValue::Fix(f)) => (Type::Fix, f.to_raw()),
        Ok(ConstantValue::Bool(b)) => (Type::Bool, b as i32),
        Err(err) => {
            emit_constant_eval_error(err, diagnostics, global_name);
            (Type::Error, 0)
        }
    }
}

fn emit_constant_eval_error(
    err: ConstantEvalError,
    diagnostics: &mut Diagnostics,
    global_name: &str,
) {
    match err {
        ConstantEvalError::NotConstant { span } => {
            ErrorKind::GlobalInitializerNotConstant {
                name: global_name.to_string(),
            }
            .at(span, DiagnosticMessage::NotAConstant)
            .note(DiagnosticMessage::GlobalInitializersMustBeConstant)
            .emit(diagnostics);
        }
        ConstantEvalError::DivisionByZero { span } => {
            ErrorKind::DivisionByZeroInConstant
                .at(span, DiagnosticMessage::DivisionByZero)
                .emit(diagnostics);
        }
        ConstantEvalError::IntegerOverflow { span } => {
            ErrorKind::OverflowInConstant
                .at(span, DiagnosticMessage::IntegerOverflow)
                .emit(diagnostics);
        }
        ConstantEvalError::TypeMismatch { span, expected, found } => {
            ErrorKind::TypeMismatchInConstant { expected, found }
                .at(span, DiagnosticMessage::TypeMismatch { expected, found })
                .emit(diagnostics);
        }
    }
}
```

## Files to Create

### `compiler/src/compile/constant_eval.rs`

New module containing:

- `ConstantValue` enum
- `ConstantEvalError` enum
- `eval_constant_expr` function
- `eval_unary_op` helper
- `eval_binary_op` helper (delegates to shared ops module)

### `compiler/src/compile/ops.rs`

Shared operation evaluation module containing:

- `eval_int_op`, `eval_fix_op`, `eval_fix_int_op` helpers
- Generic over error type to support both AST constant eval and IR constant folding
- Extracted from existing `ir/optimisations/constant_folding.rs`

## Files to Modify

### `compiler/src/compile/mod.rs`

Add module declarations:

```rust
pub mod constant_eval;
pub mod ops;
```

### `compiler/src/compile/symtab_visitor.rs`

1. Import the new module:

```rust
use super::constant_eval::{eval_constant_expr, ConstantValue, ConstantEvalError};
```

2. Replace `evaluate_constant_initializer` to use `eval_constant_expr`

### `compiler/src/compile/ir/optimisations/constant_folding.rs`

Refactor to use the shared `ops.rs` module instead of duplicating arithmetic evaluation logic.

### `compiler/src/reporting.rs`

Add new error kinds if needed:

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Division by zero in constant expression
    DivisionByZeroInConstant,

    /// Integer overflow in constant expression
    OverflowInConstant,

    /// Type mismatch in constant expression
    TypeMismatchInConstant {
        expected: &'static str,
        found: &'static str,
    },
}
```

## Implementation Order

1. **Extract shared `ops.rs`** - extract `eval_int_op`, `eval_fix_op`, `eval_fix_int_op` from `constant_folding.rs` into a shared module, generic over error type
2. **Refactor `constant_folding.rs`** - update to use the shared ops module
3. **Create `constant_eval.rs`** with `ConstantValue`, `ConstantEvalError`, and `eval_constant_expr` supporting only literals initially, using shared ops
4. **Integrate with `symtab_visitor.rs`** - replace `evaluate_constant_initializer`
5. **Add unary operator support** - implement `eval_unary_op` (requires 014-unary-operators to be done first, or can be done in parallel)
6. **Add binary operator support** - implement `eval_binary_op` using shared ops
7. **Add error reporting** - add new error kinds and diagnostic messages
8. **Tests** - add snapshot tests for constant expressions in globals
9. **Expose expression parser** - make expression parsing public in grammar for unit testing

## Testing Strategy

### Valid Constant Expressions

```tapir
# Literals (existing behavior)
global a = 5;
global b = 3.14;
global c = true;

# Unary operations (new)
global d = -5;
global e = -3.14;
global f = !true;
global g = ~0xFF;

# Nested unary (new)
global h = --5;       # 5
global i = !!true;    # true
global j = ~~0xFF;    # 0xFF

# Binary operations (new)
global k = 1 + 2;     # 3
global l = 10 - 3;    # 7
global m = 4 * 5;     # 20
global n = 10 / 3;    # 3
global o = 10 % 3;    # 1

# Mixed (new)
global p = -(1 + 2);  # -3
global q = 1 + -2;    # -1
```

### Invalid Constant Expressions

```tapir
# Variables are not constants
global bad1 = x;              # ERROR: not a constant

# Function calls are not constants
global bad2 = some_fn();      # ERROR: not a constant

# Division by zero
global bad3 = 1 / 0;          # ERROR: division by zero

# Type mismatches
global bad4 = -true;          # ERROR: cannot negate bool
global bad5 = !5;             # ERROR: cannot NOT int
global bad6 = ~3.14;          # ERROR: cannot bitwise NOT fix
```

### Snapshot Tests

Add test files in `compiler/src/compile/snapshot_tests/symtab_visitor/`:

- `global_unary_neg_success.tapir` - `global x = -5;`
- `global_unary_not_success.tapir` - `global x = !true;`
- `global_binary_add_success.tapir` - `global x = 1 + 2;`
- `global_division_by_zero_fail.tapir` - `global x = 1 / 0;`
- `global_type_mismatch_fail.tapir` - `global x = -true;`

## Relationship to IR Constant Folding

This AST-level constant evaluator is intentionally separate from IR constant folding:

| Aspect  | AST Constant Eval                   | IR Constant Folding          |
| ------- | ----------------------------------- | ---------------------------- |
| Stage   | Symbol resolution                   | Optimization                 |
| Input   | AST expressions                     | IR instructions              |
| Purpose | Compile-time constants              | Runtime optimization         |
| Scope   | Global initializers, future `const` | All expressions              |
| Errors  | Hard errors                         | Warnings (e.g., div by zero) |

The implementations share similar logic for evaluating operations. The core arithmetic evaluation logic (integer operations, fixed-point operations, etc.) should be extracted into a shared module to avoid duplication. The shared module can be parameterized by error type to handle the different error reporting requirements of each context.

## Future Extensions

### `const` Declarations

With this infrastructure, `const` declarations could be added, though it's not trivial since we'd need to track constant values during symbol resolution to allow references like `const TAU = PI * 2;`:

```tapir
const PI = 3.14159;
const TAU = PI * 2;    # Requires tracking PI's value during compilation
```

This would require extending the symbol table to store evaluated constant values and modifying `eval_constant_expr` to look up constant symbols.

## Notes

- The evaluator should be conservative: if in doubt, return `NotConstant` rather than potentially miscompiling
- Integer overflow in negation (`-(-2147483648)`) should be caught and reported
- The evaluator doesn't need to handle short-circuit evaluation specially since both sides must be constants anyway
