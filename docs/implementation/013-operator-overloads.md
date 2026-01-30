# Implementation Plan: Operator Overloads

## Overview

Add operator overloading for user-defined types using a pattern-based syntax that mirrors the expression being defined:

```tapir
struct Point { x: int, y: int }

fn Point + Point(a, b) -> Point {
    return Point(a.x + b.x, a.y + b.y);
}

var p1 = Point(1, 2);
var p2 = Point(3, 4);
var p3 = p1 + p2;  // Point(4, 6)
```

The syntax `fn T1 op T2(args)` defines what happens when the operator `op` is used with a left operand of type `T1` and a right operand of type `T2`. This makes asymmetric operators natural:

```tapir
struct Vec2 { x: fix, y: fix }

fn Vec2 * fix(v, s) -> Vec2 {
    return Vec2(v.x * s, v.y * s);
}

fn fix * Vec2(s, v) -> Vec2 {
    return Vec2(s * v.x, s * v.y);
}

var v = Vec2(1.0, 2.0);
var v2 = v * 2.0;   // Uses Vec2 * fix
var v3 = 2.0 * v;   // Uses fix * Vec2
```

## Design Decisions

### Pattern-Based Syntax

The operator definition syntax mirrors the expression it enables:

```tapir
fn LeftType op RightType(left_name, right_name) -> ReturnType { ... }
```

- The types in the pattern (`LeftType op RightType`) determine when this operator applies
- The argument names are user-chosen (like method `self`, types are inferred from pattern)
- No `self` - both operands are explicit and symmetric

### Overloadable Operators

| Operator | Pattern Example                   | Notes               |
| -------- | --------------------------------- | ------------------- |
| `+`      | `fn Point + Point(a, b)`          |                     |
| `-`      | `fn Point - Point(a, b)`          |                     |
| `*`      | `fn Vec2 * fix(v, s)`             |                     |
| `/`      | `fn Vec2 / fix(v, s)`             | Floor division      |
| `%`      | `fn Point % int(p, n)`            | Euclidean modulo    |
| `//`     | `fn Vec2 // fix(v, s)`            | Truncating division |
| `%%`     | `fn Point %% int(p, n)`           | Truncating modulo   |
| `==`     | `fn Point == Point(a, b) -> bool` |                     |
| `!=`     | `fn Point != Point(a, b) -> bool` |                     |
| `<`      | `fn Point < Point(a, b) -> bool`  |                     |
| `<=`     | `fn Point <= Point(a, b) -> bool` |                     |
| `>`      | `fn Point > Point(a, b) -> bool`  |                     |
| `>=`     | `fn Point >= Point(a, b) -> bool` |                     |

**Not overloadable:**

- `&&`, `||` - Short-circuit semantics require special evaluation order
- `<<`, `>>`, `&`, `|` - Reserved for int-specific bitwise operations
- `then` - Special evaluation semantics

### At Least One Struct Required

At least one operand type must be a struct. Primitive-only operators use builtins:

```tapir
fn int + int(a, b) -> int { ... }  // ERROR: cannot redefine primitive operators
fn Point + int(p, n) -> Point { ... }  // OK: left is struct
fn int + Point(n, p) -> Point { ... }  // OK: right is struct
fn Point + Point(a, b) -> Point { ... }  // OK: both are structs
```

### Exact Type Matching

Operator lookup requires exact type matches. No implicit conversions:

```tapir
fn Vec2 + Vec2(a, b) -> Vec2 { ... }

var v1: Vec2 = Vec2(1.0, 2.0);
var v2: Vec2 = Vec2(3.0, 4.0);
var v3 = v1 + v2;    // OK: Vec2 + Vec2
var v4 = v1 + 1.0;   // ERROR: no operator fn Vec2 + fix defined
```

### Return Type Flexibility

Operators can return any type:

```tapir
# Dot product returns scalar
fn Vec2 * Vec2(a, b) -> fix {
    return a.x * b.x + a.y * b.y;
}

# Comparison returns bool
fn Point == Point(a, b) -> bool {
    return a.x == b.x && a.y == b.y;
}

# Scaling returns vector
fn Vec2 * fix(v, s) -> Vec2 {
    return Vec2(v.x * s, v.y * s);
}
```

### Name Mangling

Operator functions are mangled as `LeftType@op@RightType`:

| Definition                 | Mangled Name      |
| -------------------------- | ----------------- |
| `fn Point + Point(a, b)`   | `Point@+@Point`   |
| `fn Vec2 * fix(v, s)`      | `Vec2@*@fix`      |
| `fn fix * Vec2(s, v)`      | `fix@*@Vec2`      |
| `fn Point == Point(a, b)`  | `Point@==@Point`  |
| `fn Vec2 // fix(v, s)`     | `Vec2@//@fix`     |

The `@` character is not allowed in user identifiers, preventing collisions.

## Grammar Changes

### `tokens.rs`

No new tokens needed. Operators are already tokens.

### `grammar.lalrpop`

Add operator function definition as a new alternative. Uses the existing `TypeIdentifier` rule which accepts both identifiers and builtin type keywords (`int`, `fix`, `bool`):

```lalrpop
FunctionDefinition: Function<'input> = {
    // Regular function: fn name(...) { ... }
    <modifiers: FunctionModifiers> "fn" <start: @L> <name: identifier> <end: @R>
    "(" <arguments: FunctionArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function { ... },

    // Method: fn Type.name(...) { ... }
    <modifiers: FunctionModifiers> "fn" <receiver: TypeIdentifier> "."
    <start: @L> <name: identifier> <end: @R>
    "(" <arguments: MethodArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function { ... },

    // Operator: fn Type op Type(...) { ... }  (NEW)
    <modifiers: FunctionModifiers> "fn"
    <left_type: TypeIdentifier>
    <start: @L> <op: OverloadableOp> <end: @R>
    <right_type: TypeIdentifier>
    "(" <arguments: OperatorArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function {
            operator_def: Some(OperatorDef {
                left_type,
                op,
                right_type,
            }),
            name: "", // Not used for operators
            arguments,
            return_types,
            statements,
            span: Span::new(file_id, start, end),
            modifiers,
            receiver_type: None,
            meta: Metadata::new(),
        },
}

// Operators that can be overloaded
OverloadableOp: BinaryOperator = {
    "+" => BinaryOperator::Add,
    "-" => BinaryOperator::Sub,
    "*" => BinaryOperator::Mul,
    "/" => BinaryOperator::Div,
    "%" => BinaryOperator::Mod,
    "//" => BinaryOperator::RealDiv,
    "%%" => BinaryOperator::RealMod,
    "==" => BinaryOperator::EqEq,
    "!=" => BinaryOperator::NeEq,
    "<" => BinaryOperator::Lt,
    "<=" => BinaryOperator::LtEq,
    ">" => BinaryOperator::Gt,
    ">=" => BinaryOperator::GtEq,
}

// Operator arguments: exactly 2 untyped identifiers (types come from pattern)
OperatorArguments: Vec<TypedIdent<'input>> = {
    <left: Identifier> "," <right: Identifier> => vec![
        TypedIdent { ident: left, ty: None },
        TypedIdent { ident: right, ty: None },
    ],
}
```

## AST Changes

### `ast.rs`

Add operator definition info to `Function`:

```rust
/// Information about an operator function definition
#[derive(Clone, Debug, Serialize)]
pub struct OperatorDef<'input> {
    /// Left operand type (e.g., "Point", "int")
    pub left_type: Ident<'input>,
    /// The operator being overloaded
    pub op: BinaryOperator,
    /// Right operand type (e.g., "Point", "fix")
    pub right_type: Ident<'input>,
}

/// A function definition
#[derive(Clone, Debug, Serialize)]
pub struct Function<'input> {
    /// If Some, this is an operator overload definition
    pub operator_def: Option<OperatorDef<'input>>,
    /// If Some, this is a method on the given type
    pub receiver_type: Option<Ident<'input>>,
    /// The function/method name (empty string for operators)
    pub name: &'input str,
    // ... rest unchanged
}

impl<'input> Function<'input> {
    /// Returns true if this is an operator overload definition
    pub fn is_operator(&self) -> bool {
        self.operator_def.is_some()
    }

    /// Returns the mangled function name.
    /// For operators: "LeftType@op@RightType"
    /// For methods: "Type@method"
    /// For regular functions: just the name
    pub fn mangled_name(&self) -> Cow<'_, str> {
        if let Some(ref op_def) = self.operator_def {
            Cow::Owned(format!(
                "{}@{}@{}",
                op_def.left_type.ident,
                op_def.op,
                op_def.right_type.ident
            ))
        } else if let Some(ref receiver) = self.receiver_type {
            Cow::Owned(format!("{}@{}", receiver.ident, self.name))
        } else {
            Cow::Borrowed(self.name)
        }
    }
}
```

Add metadata for operator expressions:

```rust
/// Stored in BinaryOperation expression metadata when the operator
/// is resolved to a user-defined operator function.
#[derive(Clone, Copy, Debug)]
pub struct OperatorOverloadInfo {
    /// The resolved function ID for the operator
    pub function_id: InternalOrExternalFunctionId,
}
```

## Struct Visitor Changes

### `struct_visitor.rs`

Extend type resolution to handle operator function definitions:

```rust
/// Resolve types in operator function definitions.
/// Called as part of resolve_all_types.
pub fn resolve_operator_types<'input>(
    functions: &mut [Function<'input>],
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    for function in functions {
        let Some(ref op_def) = function.operator_def else {
            continue;
        };

        // Resolve left operand type
        let left_type = resolve_type_name(
            op_def.left_type.ident,
            struct_names,
            op_def.left_type.span,
            diagnostics,
        );

        // Resolve right operand type
        let right_type = resolve_type_name(
            op_def.right_type.ident,
            struct_names,
            op_def.right_type.span,
            diagnostics,
        );

        // At least one must be a struct
        if !left_type.is_struct() && !right_type.is_struct() {
            ErrorKind::OperatorRequiresStruct {
                left: op_def.left_type.ident.to_string(),
                right: op_def.right_type.ident.to_string(),
            }
            .at(function.span)
            .emit(diagnostics);
            continue;
        }

        // Store resolved types in argument metadata
        // Arguments are (left, right) with types inferred from pattern
        if function.arguments.len() == 2 {
            function.arguments[0].set_resolved_type(left_type);
            function.arguments[1].set_resolved_type(right_type);
        }

        // Store resolved operator info for later phases
        function.meta.set(ResolvedOperatorTypes {
            left: left_type,
            right: right_type,
        });
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ResolvedOperatorTypes {
    pub left: Type,
    pub right: Type,
}
```

## Symbol Table Changes

### `symtab_visitor.rs`

Register operator functions with their mangled names:

```rust
impl<'input> SymTabVisitor<'input> {
    pub fn new(
        settings: &CompileSettings,
        script: &mut Script<'input>,
        struct_registry: &StructRegistry,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // ... existing initialization ...

        for (i, function) in script.functions.iter_mut().enumerate() {
            // Validate operator functions
            if function.is_operator() {
                // Event modifier not allowed on operators
                if function.modifiers.is_event_handler.is_some() {
                    ErrorKind::OperatorCannotBeEventHandler
                        .at(function.modifiers.is_event_handler.unwrap())
                        .emit(diagnostics);
                    continue;
                }

                // Must have exactly 2 arguments
                if function.arguments.len() != 2 {
                    ErrorKind::OperatorRequiresTwoArguments {
                        actual: function.arguments.len(),
                    }
                    .at(function.span)
                    .emit(diagnostics);
                    continue;
                }
            }

            let fid = FunctionId(i);
            function.meta.set(fid);

            // Use mangled_name() - works for operators, methods, and regular functions
            register_function(
                function.mangled_name().into_owned(),
                function.span,
                InternalOrExternalFunctionId::Internal(fid),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            );
        }

        // ... rest of initialization ...
    }
}
```

Add operator lookup helper:

```rust
impl<'input> SymTab<'input> {
    /// Look up an operator function by operand types.
    pub fn lookup_operator(
        &self,
        left_type: Type,
        op: BinaryOperator,
        right_type: Type,
        struct_registry: &StructRegistry,
    ) -> Option<InternalOrExternalFunctionId> {
        let left_name = type_to_name(left_type, struct_registry);
        let right_name = type_to_name(right_type, struct_registry);
        let mangled = format!("{}@{}@{}", left_name, op, right_name);
        self.function_by_mangled_name(&mangled)
    }
}

fn type_to_name(ty: Type, struct_registry: &StructRegistry) -> &str {
    match ty {
        Type::Int => "int",
        Type::Fix => "fix",
        Type::Bool => "bool",
        Type::Struct(id) => &struct_registry.get(id).name,
        Type::Error => "error",
    }
}
```

## Type Checking Changes

### `type_visitor.rs`

Modify `BinaryOperation` handling to check for operator overloads:

```rust
fn type_for_expression(
    &mut self,
    expression: &mut Expression<'input>,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Type {
    match &mut expression.kind {
        // ... existing cases ...

        ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
            let lhs_type = self.type_for_expression(lhs, symtab, diagnostics);
            let rhs_type = self.type_for_expression(rhs, symtab, diagnostics);

            if lhs_type == Type::Error || rhs_type == Type::Error {
                return Type::Error;
            }

            // Check for operator overload if at least one operand is a struct
            // AND the operator is overloadable
            if (lhs_type.is_struct() || rhs_type.is_struct())
                && is_overloadable_operator(*operator)
            {
                if let Some(result) = self.try_operator_overload(
                    lhs_type,
                    *operator,
                    rhs_type,
                    expression,
                    symtab,
                    diagnostics,
                ) {
                    return result;
                }

                // No overload found - emit helpful error
                ErrorKind::NoOperatorOverload {
                    left_type: lhs_type.display(self.struct_registry).to_string(),
                    operator: *operator,
                    right_type: rhs_type.display(self.struct_registry).to_string(),
                }
                .at(expression.span)
                .label(lhs.span, DiagnosticMessage::HasType { ty: lhs_type })
                .label(rhs.span, DiagnosticMessage::HasType { ty: rhs_type })
                .help(DiagnosticMessage::DefineOperatorFunction {
                    left: lhs_type.display(self.struct_registry).to_string(),
                    op: *operator,
                    right: rhs_type.display(self.struct_registry).to_string(),
                })
                .emit(diagnostics);
                return Type::Error;
            }

            // Existing primitive handling...
            if lhs_type != rhs_type && *operator != BinaryOperator::Then {
                ErrorKind::BinaryOperatorTypeError { lhs_type, rhs_type }
                    .at(expression.span)
                    .label(
                        expression.span,
                        DiagnosticMessage::MismatchingTypesOnBinaryOperator,
                    )
                    .emit(diagnostics);
                return Type::Error;
            }

            if !operator.can_handle_type(lhs_type) {
                ErrorKind::InvalidTypeForBinaryOperator { type_: lhs_type }
                    .at(lhs.span)
                    .label(lhs.span, DiagnosticMessage::BinaryOperatorCannotHandleType)
                    .emit(diagnostics);
                return Type::Error;
            }

            operator.update_type_with_lhs(lhs_type);
            operator.resulting_type(lhs_type, rhs_type)
        }

        // ... rest of cases ...
    }
}

fn is_overloadable_operator(op: BinaryOperator) -> bool {
    use BinaryOperator as B;
    matches!(
        op,
        B::Add | B::Sub | B::Mul | B::Div | B::Mod | B::RealDiv | B::RealMod |
        B::EqEq | B::NeEq | B::Lt | B::LtEq | B::Gt | B::GtEq
    )
}

fn try_operator_overload(
    &mut self,
    lhs_type: Type,
    operator: BinaryOperator,
    rhs_type: Type,
    expression: &mut Expression<'input>,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Option<Type> {
    // Look up the operator function
    let function_id = symtab.lookup_operator(
        lhs_type,
        operator,
        rhs_type,
        self.struct_registry,
    )?;

    let function_info = self.functions.get(&function_id)?;

    // Store operator overload info for IR lowering
    expression.meta.set(OperatorOverloadInfo { function_id });

    // Store return types for struct return handling
    let return_types = function_info.rets.clone();
    expression.meta.set(CallReturnInfo(return_types.clone()));

    // Operators must return exactly one value
    if return_types.len() != 1 {
        ErrorKind::OperatorMustReturnOneValue {
            operator,
            actual_count: return_types.len(),
        }
        .at(expression.span)
        .emit(diagnostics);
        return Some(Type::Error);
    }

    Some(return_types[0])
}
```

### Registering Operator Functions

In `TypeVisitor::new`, operator functions are registered like regular functions. The argument types come from the resolved `ResolvedOperatorTypes` metadata:

```rust
for function in functions {
    let args: Vec<FunctionArgumentInfo> = if function.is_operator() {
        // Get resolved types from metadata (set by struct_visitor)
        let op_types = function.meta.get::<ResolvedOperatorTypes>()
            .expect("Operator types should be resolved");

        function.arguments.iter().enumerate().map(|(i, arg)| {
            let ty = if i == 0 { op_types.left } else { op_types.right };
            FunctionArgumentInfo {
                name: Cow::Borrowed(arg.name()),
                ty,
                span: arg.span(),
            }
        }).collect()
    } else {
        // Existing logic for regular functions/methods
        function.arguments.iter().map(|arg| FunctionArgumentInfo {
            name: Cow::Borrowed(arg.name()),
            ty: arg.ty_required().resolved(),
            span: arg.span(),
        }).collect()
    };

    resolved_functions.insert(
        InternalOrExternalFunctionId::Internal(*function.meta.get().unwrap()),
        FunctionInfo {
            name: function.mangled_name().into_owned().leak(),
            span: function.span,
            args,
            rets: function.return_types.types.iter()
                .map(|t| t.resolved())
                .collect(),
            modifiers: function.modifiers.clone(),
        },
    );
}
```

## IR Lowering Changes

### `lowering.rs`

Check for `OperatorOverloadInfo` when lowering binary operations:

```rust
ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
    // Check if this is an overloaded operator
    if let Some(overload_info) = expr.meta.get::<OperatorOverloadInfo>() {
        // Lower as a function call
        let lhs_symbol = symtab.new_temporary();
        self.blocks_for_expression(lhs, lhs_symbol, symtab);

        let rhs_symbol = symtab.new_temporary();
        self.blocks_for_expression(rhs, rhs_symbol, symtab);

        let args = Box::new([lhs_symbol, rhs_symbol]);

        match overload_info.function_id {
            InternalOrExternalFunctionId::Internal(f) => {
                // Handle struct return expansion if needed
                let return_info = expr.meta.get::<CallReturnInfo>();
                let targets = self.expand_call_targets(
                    target_symbol,
                    return_info,
                    symtab,
                );

                self.current_block.push(TapIr::Call {
                    target: targets,
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::External(f) => {
                let return_info = expr.meta.get::<CallReturnInfo>();
                let targets = self.expand_call_targets(
                    target_symbol,
                    return_info,
                    symtab,
                );

                self.current_block.push(TapIr::CallExternal {
                    target: targets,
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::Builtin(f) => {
                self.current_block.push(TapIr::CallBuiltin {
                    target: target_symbol,
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::StructConstructor(_) => {
                unreachable!("Operator cannot be a struct constructor")
            }
        }
        return;
    }

    // Existing primitive BinOp handling
    let lhs_symbol = symtab.new_temporary();
    self.blocks_for_expression(lhs, lhs_symbol, symtab);

    let rhs_symbol = symtab.new_temporary();
    self.blocks_for_expression(rhs, rhs_symbol, symtab);

    self.current_block.push(TapIr::BinOp {
        target: target_symbol,
        lhs: lhs_symbol,
        op: *operator,
        rhs: rhs_symbol,
    });
}
```

## Diagnostic Changes

### Error Types

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Operator definition requires at least one struct type
    OperatorRequiresStruct {
        left: String,
        right: String,
    },

    /// Operator function cannot be an event handler
    OperatorCannotBeEventHandler,

    /// Operator function must have exactly 2 arguments
    OperatorRequiresTwoArguments {
        actual: usize,
    },

    /// No operator overload found for types
    NoOperatorOverload {
        left_type: String,
        operator: BinaryOperator,
        right_type: String,
    },

    /// Operator function must return exactly one value
    OperatorMustReturnOneValue {
        operator: BinaryOperator,
        actual_count: usize,
    },
}
```

### Diagnostic Messages

```rust
pub enum DiagnosticMessage {
    // ... existing variants ...

    /// Suggestion to define an operator function
    DefineOperatorFunction {
        left: String,
        op: BinaryOperator,
        right: String,
    },
}
```

Example error messages:

```
error: no operator `+` defined for `Point` + `Point`
  --> script.tapir:5:12
   |
 5 | var p = p1 + p2;
   |         -- ^ -- Point
   |         |
   |         Point
   |
   = help: define `fn Point + Point(a, b) -> ... { ... }` to enable this operator

error: operator function must have at least one struct type
  --> script.tapir:3:1
   |
 3 | fn int + int(a, b) -> int { ... }
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: cannot redefine operators on primitive types
```

## Implementation Order

### Phase 1: AST and Grammar

1. **ast.rs**: Add `OperatorDef` struct
2. **ast.rs**: Add `operator_def: Option<OperatorDef>` to `Function`
3. **ast.rs**: Add `OperatorOverloadInfo` metadata struct
4. **ast.rs**: Update `Function::mangled_name()` to handle operators
5. **ast.rs**: Add `Function::is_operator()` helper
6. **grammar.lalrpop**: Add `OverloadableOp` rule
7. **grammar.lalrpop**: Add `OperatorArguments` rule
8. **grammar.lalrpop**: Add operator function alternative to `FunctionDefinition`
9. **Tests**: Parser snapshot tests for operator definitions

**Compilable**: Yes - operators parse but aren't processed

### Phase 2: Type Resolution

1. **struct_visitor.rs**: Add `ResolvedOperatorTypes` metadata struct
2. **struct_visitor.rs**: Add `resolve_operator_types` function
3. **struct_visitor.rs**: Validate at least one operand is struct
4. **compile.rs**: Call `resolve_operator_types` in pipeline
5. **Tests**: Type resolution tests for operator functions

**Compilable**: Yes - operator types resolved

### Phase 3: Symbol Registration

1. **symtab_visitor.rs**: Register operators with mangled names
2. **symtab_visitor.rs**: Validate operator constraints (2 args, no event)
3. **symtab_visitor.rs**: Add `lookup_operator` method
4. **Tests**: Symbol table tests for operator lookup

**Compilable**: Yes - operators registered in symbol table

### Phase 4: Type Checking

1. **type_visitor.rs**: Add `is_overloadable_operator` helper
2. **type_visitor.rs**: Add `try_operator_overload` method
3. **type_visitor.rs**: Modify `BinaryOperation` to check for overloads
4. **type_visitor.rs**: Register operator functions in `TypeVisitor::new`
5. **Tests**: Type checking tests for operator expressions

**Compilable**: Yes - operators type check correctly

### Phase 5: IR Lowering

1. **lowering.rs**: Check for `OperatorOverloadInfo` in `BinaryOperation`
2. **lowering.rs**: Generate function call IR for overloaded operators
3. **lowering.rs**: Handle struct return expansion
4. **Tests**: IR snapshot tests for operator calls

**Compilable**: Yes - full operator overload support

### Phase 6: Diagnostics

1. **reporting.rs**: Add new error kinds
2. **reporting.rs**: Add diagnostic messages with helpful hints
3. **Tests**: Error message snapshot tests

**Compilable**: Yes - polished error messages

### Phase 7: Documentation

1. **tapir-reference.md**: Add "Operator Overloading" section
2. **tapir-reference.md**: Document syntax and examples
3. **tapir-reference.md**: List overloadable operators

## Testing Strategy

### Parser Tests

- Simple operator: `fn Point + Point(a, b) -> Point { }`
- Mixed types: `fn Vec2 * fix(v, s) -> Vec2 { }`
- Primitive on left: `fn int * Vec2(n, v) -> Vec2 { }`
- Comparison: `fn Point == Point(a, b) -> bool { }`
- All overloadable operators parse correctly
- Non-overloadable operators in definition position fail

### Type Checking Tests

- `Point + Point` with operator defined resolves correctly
- `Point + Point` without operator gives helpful error
- `Vec2 * fix` and `fix * Vec2` as separate overloads
- Return type flows through correctly
- Primitive-only operator definition rejected
- Operator with wrong arg count rejected
- Event handler on operator rejected

### IR Tests

- Operator generates `Call` IR (not `BinOp`)
- Arguments in correct order (lhs first, rhs second)
- Struct return expansion works
- Chained operators: `a + b + c` generates correct calls

### Runtime Tests

- `Point + Point` executes and returns correct result
- `Vec2 * fix` and `fix * Vec2` both work
- Comparison operators return correct bool
- Chained operators evaluate correctly
- Mixed with field access: `(p1 + p2).x`

### Error Tests

- Struct + struct without operator defined
- Type mismatch (defined `Point + Point`, used `Point + int`)
- Multi-return operator function rejected
- Primitive-only definition rejected

## Example Code

```tapir
struct Vec2 { x: fix, y: fix }

# Vector addition
fn Vec2 + Vec2(a, b) -> Vec2 {
    return Vec2(a.x + b.x, a.y + b.y);
}

# Vector subtraction
fn Vec2 - Vec2(a, b) -> Vec2 {
    return Vec2(a.x - b.x, a.y - b.y);
}

# Scalar multiplication (both directions)
fn Vec2 * fix(v, s) -> Vec2 {
    return Vec2(v.x * s, v.y * s);
}

fn fix * Vec2(s, v) -> Vec2 {
    return Vec2(s * v.x, s * v.y);
}

# Dot product
fn Vec2 * Vec2(a, b) -> fix {
    return a.x * b.x + a.y * b.y;
}

# Equality
fn Vec2 == Vec2(a, b) -> bool {
    return a.x == b.x && a.y == b.y;
}

# Usage
var v1 = Vec2(1.0, 2.0);
var v2 = Vec2(3.0, 4.0);

var sum = v1 + v2;           // Vec2(4.0, 6.0)
var scaled = v1 * 2.0;       // Vec2(2.0, 4.0)
var scaled2 = 2.0 * v1;      // Vec2(2.0, 4.0)
var dot = v1 * v2;           // 11.0
var same = v1 == v2;         // false
var chain = v1 + v2 + v1;    // Vec2(5.0, 8.0)
```

## Future Extensions

### Compound Assignment Operators

```tapir
var v = Vec2(1.0, 2.0);
v += Vec2(1.0, 1.0);  // Desugars to: v = v + Vec2(1.0, 1.0)
```

Requires grammar support for `+=`, `-=`, etc.

### Unary Operators

```tapir
fn -Vec2(v) -> Vec2 {
    return Vec2(-v.x, -v.y);
}

var neg = -v;
```

Requires separate unary operator syntax and handling.

### Index Operator

```tapir
fn Vec2[int](v, i) -> fix {
    if i == 0 { return v.x; }
    return v.y;
}

var x = v[0];  // v.x
```

Requires `[]` operator support in grammar and type checking.

## Implementation Notes

This section will document differences between the plan and actual implementation.
