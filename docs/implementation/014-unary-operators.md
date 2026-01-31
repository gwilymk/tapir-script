# Implementation Plan: Unary Operators

## Overview

Add unary operators to tapir-script, specifically:

- **`-` (negation)**: Negates numeric values (`int` and `fix`)
- **`!` (logical not)**: Inverts boolean values
- **`~` (bitwise not)**: Bitwise complement for integers

```tapir
var x = -5;              # int negation
var y = -3.14;           # fix negation
var neg = -x;            # negate variable
var notDone = !done;     # logical not
var mask = ~flags;       # bitwise complement
var complex = -(-x);     # double negation
```

## Design Decisions

### Supported Operators

| Operator | Operand Type | Result Type | Description            |
| -------- | ------------ | ----------- | ---------------------- |
| `-`      | `int`        | `int`       | Integer negation       |
| `-`      | `fix`        | `fix`       | Fixed-point negation   |
| `!`      | `bool`       | `bool`      | Logical NOT            |
| `~`      | `int`        | `int`       | Bitwise NOT (complement) |

### Precedence

Unary operators bind tighter than all binary operators. They are right-associative (applied right-to-left):

```tapir
var a = -1 + 2;      # (-1) + 2 = 1
var b = !true || x;  # (!true) || x
var c = --x;         # -(-x)
var d = !!done;      # !(!done)
var e = ~~x;         # ~(~x) = x
```

### Implementation Approach: Desugaring vs. Native

**Option A: Desugar to binary operations**

- `-x` becomes `0 - x`
- `!x` becomes `x == false` or similar

**Option B: Native unary operations**

- Add dedicated AST node, IR instruction, and bytecode opcode

**Decision: Native implementation (Option B)**

Rationale:

1. Cleaner AST representation matches source code intent
2. Better error messages can reference the unary operator directly
3. Simpler IR (one instruction vs. two)
4. More efficient bytecode (one opcode vs. constant + binop)
5. Future extensibility for operator overloading on structs

## Lexer Changes

### `lexer.rs`

Add the `!` and `~` tokens (the `-` token already exists as `OperatorSub`):

```rust
#[derive(Debug, Clone, PartialEq, Logos)]
pub enum Token<'input> {
    // ... existing tokens ...

    #[token("!")]
    OperatorNot,

    #[token("~")]
    OperatorBitNot,
}
```

## Grammar Changes

### `grammar.lalrpop`

Add the `!` and `~` token mappings and unary expression rules:

```lalrpop
extern {
    // ... existing tokens ...
    "!" => Token::OperatorNot,
    "~" => Token::OperatorBitNot,
}

Expression: Expression<'input> = {
    // Level 0: Unary operators (highest precedence)
    #[precedence(level="0")]
    <UnaryExpr>,

    #[precedence(level="1")]
    <PostfixExpr>,

    // ... rest of binary operators (levels shifted up by 1) ...
}

UnaryExpr: Expression<'input> = {
    <Term>,
    // Unary negation
    <start:@L> "-" <operand:UnaryExpr> <end:@R> =>
        ExpressionKind::UnaryOperation {
            operator: UnaryOperator::Neg,
            operand: Box::new(operand),
        }.with_span(file_id, start, end),
    // Logical NOT
    <start:@L> "!" <operand:UnaryExpr> <end:@R> =>
        ExpressionKind::UnaryOperation {
            operator: UnaryOperator::Not,
            operand: Box::new(operand),
        }.with_span(file_id, start, end),
    // Bitwise NOT
    <start:@L> "~" <operand:UnaryExpr> <end:@R> =>
        ExpressionKind::UnaryOperation {
            operator: UnaryOperator::BitNot,
            operand: Box::new(operand),
        }.with_span(file_id, start, end),
}
```

**Important**: Unary `-` must be distinguished from binary `-` by the grammar. LALRPOP's precedence handling places unary operators at a higher precedence level than binary operators, ensuring `-1 + 2` parses as `(-1) + 2`.

### Handling Negative Literals

Currently, negative integer/fix literals like `-5` are handled specially during parsing (the lexer recognizes them as a single token). With unary `-`, we have two options:

**Option A: Keep lexer handling of negative literals**

- `-5` lexes as `Integer("-5")`
- Unary `-` only applies to non-literal expressions
- Pro: Backward compatible, efficient constant representation
- Con: Grammar complexity, inconsistent handling

**Option B: Unify all negation through unary operator**

- `-5` parses as `UnaryOperation(Neg, Integer(5))`
- Constant folding reduces it to `Integer(-5)` during optimization
- Pro: Simpler grammar, uniform handling
- Con: Extra optimization step needed

**Decision: Option B** - Parse all negation as unary operator. The constant folding pass already exists and can handle this case.

### Lexer Update for Negative Numbers

Modify the lexer to not include the leading `-` in number tokens:

```rust
// Change from:
#[regex(r"-?[0-9]+", |lex| lex.slice())]
Integer(&'input str),

// To:
#[regex(r"[0-9]+", |lex| lex.slice())]
Integer(&'input str),
```

Similarly for fix literals.

## AST Changes

### `ast.rs`

Add `UnaryOperator` enum and `UnaryOperation` expression kind:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Eq)]
pub enum UnaryOperator {
    /// Arithmetic negation: -x
    Neg,
    /// Logical NOT: !x
    Not,
    /// Bitwise NOT: ~x
    BitNot,
}

impl UnaryOperator {
    /// Returns whether this operator can be applied to the given type.
    pub fn can_handle_type(self, operand_type: Type) -> bool {
        match self {
            UnaryOperator::Neg => matches!(operand_type, Type::Int | Type::Fix),
            UnaryOperator::Not => matches!(operand_type, Type::Bool),
            UnaryOperator::BitNot => matches!(operand_type, Type::Int),
        }
    }

    /// Returns the result type of applying this operator to the given type.
    pub fn resulting_type(self, operand_type: Type) -> Type {
        match self {
            UnaryOperator::Neg => operand_type, // int -> int, fix -> fix
            UnaryOperator::Not => Type::Bool,
            UnaryOperator::BitNot => Type::Int,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Neg => "-",
                UnaryOperator::Not => "!",
                UnaryOperator::BitNot => "~",
            }
        )
    }
}

#[derive(Clone, Default, Debug, Serialize)]
pub enum ExpressionKind<'input> {
    // ... existing variants ...

    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression<'input>>,
    },
}
```

### Update `Expression::all_inner()`

Add handling for the new variant:

```rust
impl<'input> Expression<'input> {
    pub fn all_inner(&self) -> Box<dyn Iterator<Item = &Expression<'input>> + '_> {
        match &self.kind {
            // ... existing cases ...

            ExpressionKind::UnaryOperation { operand, .. } => {
                Box::new(iter::once(self).chain(operand.all_inner()))
            }
        }
    }
}
```

## Symbol Table Changes

### `symtab_visitor.rs`

Add visitor handling for unary operations:

```rust
fn visit_expr(&mut self, expr: &mut Expression<'input>, diagnostics: &mut Diagnostics) {
    match &mut expr.kind {
        // ... existing cases ...

        ExpressionKind::UnaryOperation { operand, .. } => {
            self.visit_expr(operand, diagnostics);
        }
    }
}
```

## Type Checking Changes

### `type_visitor.rs`

Add type checking for unary operations:

```rust
fn type_for_expression(
    &mut self,
    expression: &mut Expression<'input>,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Type {
    match &mut expression.kind {
        // ... existing cases ...

        ExpressionKind::UnaryOperation { operator, operand } => {
            let operand_type = self.type_for_expression(operand, symtab, diagnostics);

            if operand_type == Type::Error {
                return Type::Error;
            }

            if !operator.can_handle_type(operand_type) {
                ErrorKind::InvalidTypeForUnaryOperator {
                    operator: *operator,
                    operand_type,
                }
                .at(expression.span)
                .label(
                    operand.span,
                    DiagnosticMessage::UnaryOperatorCannotHandleType {
                        operator: *operator,
                        operand_type,
                    },
                )
                .emit(diagnostics);
                return Type::Error;
            }

            operator.resulting_type(operand_type)
        }
    }
}
```

## IR Changes

### `ir.rs`

Add a new IR instruction for unary operations:

```rust
use crate::ast::UnaryOperator;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TapIr {
    // ... existing variants ...

    UnaryOp {
        target: SymbolId,
        operand: SymbolId,
        op: UnaryOperator,
    },
}
```

Update the helper methods:

```rust
impl TapIr {
    pub fn targets(&self) -> impl Iterator<Item = SymbolId> {
        // Add UnaryOp to target iteration
    }

    pub fn sources(&self) -> impl Iterator<Item = SymbolId> {
        // Add UnaryOp operand to source iteration
    }

    pub fn could_have_side_effects(&self) -> bool {
        match self {
            TapIr::UnaryOp { .. } => false, // Pure operation
            // ... existing cases ...
        }
    }
}
```

### `symbol_iter.rs`

Add handling for `UnaryOp` in the symbol iterators:

```rust
// In SymbolIter::new_target
TapIr::UnaryOp { target, .. } => Some(*target),

// In SymbolIter::new_source
TapIr::UnaryOp { operand, .. } => Some(*operand),
```

### `lowering.rs`

Add IR generation for unary operations:

```rust
ast::ExpressionKind::UnaryOperation { operator, operand } => {
    let operand_target = symtab.new_temporary();
    self.blocks_for_expression(operand, operand_target, symtab);

    self.current_block.push(TapIr::UnaryOp {
        target: target_symbol,
        operand: operand_target,
        op: *operator,
    });
}
```

## Optimization Changes

### `optimisations/constant_folding.rs`

Add constant folding for unary operations:

```rust
fn fold_unary_op(
    target: SymbolId,
    operand: SymbolId,
    op: UnaryOperator,
    constants: &HashMap<SymbolId, Constant>,
) -> Option<TapIr> {
    let operand_val = constants.get(&operand)?;

    let result = match (op, operand_val) {
        (UnaryOperator::Neg, Constant::Int(v)) => Constant::Int(-v),
        (UnaryOperator::Neg, Constant::Fix(v)) => Constant::Fix(-*v),
        (UnaryOperator::Not, Constant::Bool(v)) => Constant::Bool(!v),
        (UnaryOperator::BitNot, Constant::Int(v)) => Constant::Int(!v),
        _ => return None, // Type error - shouldn't happen after type checking
    };

    Some(TapIr::Constant(target, result))
}
```

### `optimisations/dead_code.rs`

Add `UnaryOp` to dead code elimination (it has no side effects, so can be removed if target is unused).

## Bytecode Changes

### `bytecode/src/lib.rs`

Add new opcodes:

```rust
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Opcode {
    // ... existing opcodes ...

    /// Negate a numeric value: target = -operand
    /// Type1: target, operand, unused
    Neg,

    /// Logical NOT: target = !operand
    /// Type1: target, operand, unused
    Not,

    /// Bitwise NOT: target = ~operand
    /// Type1: target, operand, unused
    BitNot,
}

impl Type1 {
    pub const fn neg(target: u8, operand: u8) -> Self {
        Self::new2(Opcode::Neg, target, operand)
    }

    pub const fn not(target: u8, operand: u8) -> Self {
        Self::new2(Opcode::Not, target, operand)
    }

    pub const fn bit_not(target: u8, operand: u8) -> Self {
        Self::new2(Opcode::BitNot, target, operand)
    }
}
```

## Code Generation Changes

### `compile/compile.rs` (or wherever bytecode is emitted)

Add bytecode generation for `UnaryOp`:

```rust
TapIr::UnaryOp { target, operand, op } => {
    let target_reg = reg_alloc.get(target);
    let operand_reg = reg_alloc.get(operand);

    let instr = match op {
        UnaryOperator::Neg => Type1::neg(target_reg, operand_reg),
        UnaryOperator::Not => Type1::not(target_reg, operand_reg),
        UnaryOperator::BitNot => Type1::bit_not(target_reg, operand_reg),
    };

    bytecode.push(instr.encode());
}
```

## VM Changes

### `vm/src/state.rs`

Add instruction handlers:

```rust
O::Neg => {
    type1!(target, operand, _unused);
    let value = self.get_reg(operand);
    // Negation works the same for int and fix (just negate the raw bits)
    self.set_reg(target, -value);
}

O::Not => {
    type1!(target, operand, _unused);
    let value = self.get_reg(operand);
    // Logical NOT: 0 becomes 1, non-zero becomes 0
    self.set_reg(target, (value == 0) as i32);
}

O::BitNot => {
    type1!(target, operand, _unused);
    let value = self.get_reg(operand);
    // Bitwise NOT: flip all bits
    self.set_reg(target, !value);
}
```

**Note**: For `Neg`, negating the raw i32 works correctly for both int and fix types because fixed-point negation is just two's complement negation of the underlying representation.

## Diagnostic Changes

### Error Types

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Invalid type for unary operator
    InvalidTypeForUnaryOperator {
        operator: UnaryOperator,
        operand_type: Type,
    },
}
```

### Diagnostic Messages

```rust
pub enum DiagnosticMessage {
    // ... existing variants ...

    UnaryOperatorCannotHandleType {
        operator: UnaryOperator,
        operand_type: Type,
    },
}
```

Example error messages:

```
error: cannot apply unary `-` to type `bool`
  --> script.tapir:5:9
   |
 5 | var x = -done;
   |         ^^^^^
   |         |
   |         expected `int` or `fix`, found `bool`

error: cannot apply `!` to type `int`
  --> script.tapir:6:9
   |
 6 | var y = !count;
   |         ^^^^^^
   |         |
   |         expected `bool`, found `int`
```

## Analysis Changes

### `analyse/hover.rs`

Add hover support for unary operations:

```rust
ExpressionKind::UnaryOperation { operand, .. } => {
    extract_from_expression(operand, symtab, struct_registry, global_info, result);
}
```

### `analyse/signature_help.rs`

Add handling (no special signature info needed, just recurse into operand):

```rust
ExpressionKind::UnaryOperation { operand, .. } => {
    extract_from_expression(operand, signatures, call_sites);
}
```

### `references.rs`

Add reference extraction:

```rust
ExpressionKind::UnaryOperation { operand, .. } => {
    extract_references_from_expression(operand, symtab, struct_registry, global_info, result);
}
```

## Implementation Order

### Phase 1: Lexer and Parser

1. **lexer.rs**: Add `OperatorNot` token for `!`
2. **lexer.rs**: Add `OperatorBitNot` token for `~`
3. **lexer.rs**: Remove leading `-` from integer and fix token patterns
4. **grammar.lalrpop**: Add `"!"` and `"~"` token mappings
5. **grammar.lalrpop**: Add `UnaryExpr` rule with negation, NOT, and bitwise NOT
6. **grammar.lalrpop**: Adjust precedence levels (shift binary operators up)
7. **Tests**: Parser snapshot tests for unary expressions

**Compilable**: Yes - parses but semantic analysis fails

### Phase 2: AST and Type Checking

1. **ast.rs**: Add `UnaryOperator` enum
2. **ast.rs**: Add `UnaryOperation` to `ExpressionKind`
3. **ast.rs**: Update `Expression::all_inner()`
4. **symtab_visitor.rs**: Visit unary operation operands
5. **type_visitor.rs**: Type check unary operations
6. **reporting.rs**: Add error types and messages
7. **Tests**: Type checking tests for valid and invalid unary usage

**Compilable**: Yes - type checks correctly

### Phase 3: IR Generation

1. **ir.rs**: Add `UnaryOp` variant to `TapIr`
2. **symbol_iter.rs**: Add `UnaryOp` handling
3. **lowering.rs**: Generate `UnaryOp` IR
4. **Tests**: IR snapshot tests

**Compilable**: Yes - generates IR

### Phase 4: Optimization

1. **constant_folding.rs**: Fold constant unary operations
2. **dead_code.rs**: Handle `UnaryOp` in dead code elimination
3. **Tests**: Verify constant folding works

**Compilable**: Yes - optimizations work

### Phase 5: Bytecode and VM

1. **bytecode/src/lib.rs**: Add `Neg`, `Not`, and `BitNot` opcodes
2. **bytecode/src/lib.rs**: Add `Type1::neg()`, `Type1::not()`, and `Type1::bit_not()`
3. **compile/compile.rs**: Generate bytecode for `UnaryOp`
4. **vm/src/state.rs**: Implement `Neg`, `Not`, and `BitNot` handlers
5. **Tests**: Runtime tests

**Compilable**: Yes - full unary operator support

### Phase 6: Analysis and Polish

1. **analyse/hover.rs**: Add unary operation handling
2. **analyse/signature_help.rs**: Add unary operation handling
3. **references.rs**: Add reference extraction
4. **Tests**: IDE feature tests

**Compilable**: Yes - complete feature

### Phase 7: Documentation

1. **tapir-reference.md**: Document unary operators in Operators section
2. **tapir-reference.md**: Update precedence table

## Testing Strategy

### Parser Tests

- `-5` parses as unary negation of integer 5
- `-3.14` parses as unary negation of fix 3.14
- `!true` parses as logical NOT
- `~flags` parses as bitwise NOT
- `-x` parses as unary negation of variable
- `--x` parses as nested negation
- `!!done` parses as nested NOT
- `~~x` parses as nested bitwise NOT
- `-1 + 2` parses as `(-1) + 2`
- `!a && b` parses as `(!a) && b`
- `~x & mask` parses as `(~x) & mask`

### Type Checking Tests

- `-5` (int) succeeds with type int
- `-3.14` (fix) succeeds with type fix
- `-x` where x: int succeeds with type int
- `-x` where x: fix succeeds with type fix
- `-done` where done: bool fails (wrong type)
- `!true` succeeds with type bool
- `!x` where x: bool succeeds with type bool
- `!5` where 5: int fails (wrong type)
- `~x` where x: int succeeds with type int
- `~x` where x: fix fails (wrong type - int only)
- `~done` where done: bool fails (wrong type)
- `--x` where x: int succeeds (nested negation)
- `~~x` where x: int succeeds (nested bitwise NOT)

### IR Tests

- `-x` generates `UnaryOp { op: Neg, ... }`
- `!b` generates `UnaryOp { op: Not, ... }`
- Complex: `-x + y` generates correct structure

### Constant Folding Tests

- `-5` folds to `Constant::Int(-5)`
- `-3.5` folds to `Constant::Fix(-3.5)`
- `!true` folds to `Constant::Bool(false)`
- `!false` folds to `Constant::Bool(true)`
- `~0` folds to `Constant::Int(-1)` (all bits set)
- `~(-1)` folds to `Constant::Int(0)` (all bits cleared)
- `--5` folds to `Constant::Int(5)`
- `!!true` folds to `Constant::Bool(true)`
- `~~5` folds to `Constant::Int(5)`
- `-x` (non-constant) does not fold

### Runtime Tests

- `-5` evaluates to -5
- `-(-5)` evaluates to 5
- `!true` evaluates to false
- `!false` evaluates to true
- `~0` evaluates to -1 (0xFFFFFFFF)
- `~(-1)` evaluates to 0
- `-x` with x=10 evaluates to -10
- `-x` with x: fix = 3.5 evaluates to -3.5
- `~x` with x=5 evaluates to -6 (bitwise complement)
- Mixed: `-(1 + 2)` evaluates to -3
- Mixed: `~(x & 0xFF)` clears all but low byte, then inverts

### Error Tests

- `-true` produces helpful error about bool not supporting negation
- `!5` produces helpful error about int not supporting NOT
- `~3.14` produces helpful error about fix not supporting bitwise NOT
- `~done` where done: bool produces error
- `-Point(1, 2)` produces error (structs not supported without overload)
- `!Point(1, 2)` produces error (structs not supported without overload)
- `~Point(1, 2)` produces error (structs not supported without overload)

## Future Extensions

### Unary Plus

```tapir
var x = +5;  # Currently not supported, but trivial to add
```

Could be added as a no-op unary operator for symmetry with negation.

### Operator Overloading for Unary Operators

Following the pattern in 013-operator-overloads.md, unary operators could be overloadable for struct types:

```tapir
struct Vec2 { x: fix, y: fix }

# Unary negation
fn -Vec2(v) -> Vec2 {
    return Vec2(-v.x, -v.y);
}

# Logical not (if it makes sense for the type)
fn !State(s) -> bool {
    return s.is_empty;
}

var v = Vec2(1.0, 2.0);
var neg = -v;  # Vec2(-1.0, -2.0)
```

#### Syntax

The pattern mirrors the expression being defined:

```tapir
fn op Type(arg_name) -> ReturnType { ... }
```

Where `op` is one of `-`, `!`, or `~`.

#### Name Mangling

Unary operator functions would be mangled as `op@Type`:

| Definition      | Mangled Name |
| --------------- | ------------ |
| `fn -Vec2(v)`   | `-@Vec2`     |
| `fn !State(s)`  | `!@State`    |
| `fn ~Flags(f)`  | `~@Flags`    |

#### Grammar Extension

```lalrpop
FunctionDefinition: Function<'input> = {
    // ... existing rules ...

    // Unary operator: fn op Type(...) { ... }
    <modifiers: FunctionModifiers> "fn"
    <start: @L> <op: UnaryOverloadableOp> <end: @R>
    <operand_type: TypeIdentifier>
    "(" <arguments: UnaryOperatorArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function {
            unary_operator_def: Some(UnaryOperatorDef {
                op,
                operand_type,
            }),
            // ...
        },
}

UnaryOverloadableOp: UnaryOperator = {
    "-" => UnaryOperator::Neg,
    "!" => UnaryOperator::Not,
    "~" => UnaryOperator::BitNot,
}

// Unary operator arguments: exactly 1 untyped identifier
UnaryOperatorArguments: Vec<TypedIdent<'input>> = {
    <arg: Identifier> => vec![
        TypedIdent { ident: arg, ty: None },
    ],
}
```

#### Type Checking

When type-checking a unary operation on a struct type, look up the operator function:

```rust
fn try_unary_operator_overload(
    &mut self,
    operator: UnaryOperator,
    operand_type: Type,
    expression: &mut Expression<'input>,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Option<Type> {
    // Only check overloads for struct types
    if !operand_type.is_struct() {
        return None;
    }

    // Look up: op@TypeName
    let function_id = symtab.lookup_unary_operator(
        operator,
        operand_type,
        self.struct_registry,
    )?;

    // Store for IR lowering
    expression.meta.set(UnaryOperatorOverloadInfo { function_id });

    // Return the function's return type
    let function_info = self.functions.get(&function_id)?;
    Some(function_info.rets[0])
}
```

#### Constraints

- The operand type must be a struct (cannot redefine primitives)
- Must take exactly one argument
- The argument's type is inferred from the operand type in the pattern
- Return type is flexible (doesn't need to match operand type)

#### IR Lowering

When lowering a unary operation with an overload, generate a function call:

```rust
if let Some(overload_info) = expr.meta.get::<UnaryOperatorOverloadInfo>() {
    let operand_target = symtab.new_temporary();
    self.blocks_for_expression(operand, operand_target, symtab);

    self.current_block.push(TapIr::Call {
        target: Box::new([target_symbol]),
        f: overload_info.function_id,
        args: Box::new([operand_target]),
    });
    return;
}
```

#### Implementation Considerations

This could be implemented:

1. **As part of this PR**: Add full unary operator overloading support
2. **In a follow-up**: Focus on primitive unary operators first, add overloading later
3. **Together with binary overloads**: Extend 013-operator-overloads to cover both

**Recommendation**: Implement basic unary operators first (Phase 1-7 above), then add overloading support as an extension that builds on both this work and 013-operator-overloads.md.

## Example Code

```tapir
# Basic usage
var a = -5;
var b = -3.14;
var c = !true;
var d = ~0;               # -1 (all bits set)

# With variables
var x = 10;
var neg_x = -x;           # -10

var done = false;
var not_done = !done;     # true

var flags = 0xFF;
var inverted = ~flags;    # 0xFFFFFF00

# In expressions
var result = -x + 5;      # -10 + 5 = -5
var check = !done && x > 0;  # true && true = true
var masked = ~flags & 0xF0;  # Clear low bits, keep high nibble

# Nested
var double_neg = --x;     # 10
var double_not = !!done;  # false
var double_inv = ~~flags; # 0xFF (back to original)

# With function calls
fn negate(n: int) -> int {
    return -n;
}

var neg_result = negate(-5);  # 5

# With fix arithmetic
var angle: fix = 3.14;
var opposite = -angle;    # -3.14
var back = -opposite;     # 3.14

# Bitwise operations
fn clear_bit(value: int, bit: int) -> int {
    return value & ~(1 << bit);
}

var cleared = clear_bit(0xFF, 3);  # 0xF7

# Complex expressions
var complex = -(a + b) * -c;  # Where a, b, c are fix values
```

## Implementation Notes

This section will document differences between the plan and actual implementation.
