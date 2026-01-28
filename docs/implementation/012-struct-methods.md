# Implementation Plan: Struct Methods

## Overview

Add instance methods to struct types, primitive types (`int`, `fix`), and support for static methods. Methods provide a cleaner syntax for operations on values:

```tapir
struct Point { x: fix, y: fix }

fn Point.distance(self, other: Point) -> fix {
    let dx = other.x - self.x;
    let dy = other.y - self.y;
    return sqrt(dx * dx + dy * dy);
}

var p1 = Point(1.0, 2.0);
var p2 = Point(4.0, 6.0);
var d = p1.distance(p2);  // Method call syntax
```

Methods can also be defined on primitive types:

```tapir
builtin(5) fn fix.round(self) -> int;
builtin(3) fn fix.floor(self) -> int;

var x: fix = 3.7;
var rounded = x.round();  // Returns 4
```

## Design Decisions

### Method Syntax

**Definition syntax**: `fn <Type>.<method_name>(<params>) -> <return> { ... }`

```tapir
fn Point.scale(self, factor: fix) -> Point {
    return Point(self.x * factor, self.y * factor);
}
```

**Call syntax**: `<expr>.<method_name>(<args>)`

```tapir
var scaled = point.scale(2.0);
var chained = get_point().scale(2.0).normalize();
```

### Instance vs Static Methods

- **Instance methods**: First parameter must be named `self` with type matching the method's type
- **Static methods**: No `self` parameter; called as `Type.method(args)` (future extension)

For now, we focus on instance methods. Static methods can be added later by checking whether the first parameter is `self`.

### Immutable Self

Methods cannot mutate `self`. To modify a value, return a new one:

```tapir
fn Point.translate(self, dx: fix, dy: fix) -> Point {
    return Point(self.x + dx, self.y + dy);
}

// Usage: reassign to update
pos = pos.translate(1.0, 0.0);
```

### Name Mangling

Methods are transformed into regular functions with mangled names: `<Type>@<method_name>`

- `Point.distance` becomes `Point@distance`
- `fix.round` becomes `fix@round`

The `@` character is not allowed in user identifiers, preventing name collisions.

### Method Call Transformation

A method call `receiver.method(args)` is transformed into a regular function call `Type@method(receiver, args)`:

```tapir
// Source code
p1.distance(p2)

// Transformed (conceptually, not valid tapir syntax)
Point@distance(p1, p2)
```

This transformation happens during type checking once we know the receiver's type.

### Allowed Method Modifiers

- **`builtin`**: Allowed. E.g., `builtin(5) fn fix.round(self) -> int;`
- **`event`**: Not allowed. Event handlers cannot be methods.
- **`extern`**: Not allowed (for now). Could be added later if needed.

### Field vs Method Disambiguation

A type can have both a field and a method with the same name:

```tapir
struct Foo { x: int }
fn Foo.x(self) -> int { return self.x * 2; }

var f = Foo(5);
var field = f.x;      // Field access: 5
var method = f.x();   // Method call: 10
```

The grammar distinguishes these: `expr.ident` is field access, `expr.ident(args)` is a method call.

## Grammar Changes

### `grammar.lalrpop`

Extend the existing function definition to support an optional receiver type:

```lalrpop
// Function definition - now supports optional receiver type for methods
FunctionDefinition: Function<'input> = {
    // Regular function: fn name(...) { ... }
    <modifiers: FunctionModifiers> "fn" <start: @L> <name: identifier> <end: @R>
    "(" <arguments: FunctionArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function {
            receiver_type: None,
            name,
            arguments,
            return_types,
            statements,
            span: Span::new(file_id, start, end),
            modifiers,
            meta: Metadata::new(),
        },

    // Method: fn Type.name(...) { ... }
    <modifiers: FunctionModifiers> "fn" <receiver: Identifier> "."
    <start: @L> <name: identifier> <end: @R>
    "(" <arguments: MethodArguments> ")" <return_types: FunctionReturn> <statements: Block> =>
        Function {
            receiver_type: Some(receiver),
            name,
            arguments,
            return_types,
            statements,
            span: Span::new(file_id, start, end),
            modifiers,
            meta: Metadata::new(),
        },
}

// Builtin function declaration - now supports optional receiver type
BuiltinFunctionDecl: BuiltinFunction<'input> = {
    // Regular builtin: builtin(id) fn name(...) -> ret;
    builtin "(" <builtin_id: BuiltinId> ")" "fn"
    <start: @L> <name: identifier> <end: @R>
    "(" <arguments: FunctionArguments> ")" <return_type: FunctionReturn> ";" =>
        BuiltinFunction {
            receiver_type: None,
            name,
            builtin_id,
            arguments,
            return_type,
            span: Span::new(file_id, start, end),
            meta: Metadata::new(),
        },

    // Builtin method: builtin(id) fn Type.name(...) -> ret;
    builtin "(" <builtin_id: BuiltinId> ")" "fn" <receiver: Identifier> "."
    <start: @L> <name: identifier> <end: @R>
    "(" <arguments: MethodArguments> ")" <return_type: FunctionReturn> ";" =>
        BuiltinFunction {
            receiver_type: Some(receiver),
            name,
            builtin_id,
            arguments,
            return_type,
            span: Span::new(file_id, start, end),
            meta: Metadata::new(),
        },
}
```

Add `self` keyword to the lexer and grammar:

```lalrpop
// In the token definitions
"self" => Token::KeywordSelf,

// Allow self to be used as a variable name (for reading self inside method bodies)
// Use this instead of `identifier` where variable references are allowed
IdentOrSelf: &'input str = {
    <identifier>,
    "self" => "self",
};

// Update Term to use IdentOrSelf for variable references
Term: Expression<'input> = {
    // ... other rules ...
    <start: @L> <ident: IdentOrSelf> <end: @R> =>
        ExpressionKind::Variable(ident).with_span(file_id, start, end),
    // ... rest ...
};

// Method arguments: optional self (never typed) followed by regular typed arguments
// self's type is always implicitly the receiver type - no explicit type allowed
MethodArguments: Vec<TypedIdent<'input>> = {
    // No arguments (static method)
    () => vec![],

    // Just self
    <start:@L> "self" <end:@R> =>
        vec![TypedIdent {
            ident: Ident { ident: "self", span: Span::new(file_id, start, end) },
            ty: None,
        }],

    // self followed by more arguments
    <start:@L> "self" <end:@R> "," <rest:CommaSeparated<TypedIdentifierRequired>> => {
        let mut args = vec![TypedIdent {
            ident: Ident { ident: "self", span: Span::new(file_id, start, end) },
            ty: None,
        }];
        args.extend(rest);
        args
    },

    // Static method - no self, just regular typed arguments
    <CommaSeparated<TypedIdentifierRequired>>,
};
```

Add method call to expressions:

```lalrpop
// In PostfixExpr - method call as expression
PostfixExpr: Expression<'input> = {
    <Term>,
    // Field access (no parentheses)
    <start:@L> <base:PostfixExpr> "." <field:Identifier> <end:@R> =>
        ExpressionKind::FieldAccess { base: Box::new(base), field }.with_span(file_id, start, end),

    // Method call (with parentheses) - NEW
    <start:@L> <receiver:PostfixExpr> "." <method:Identifier>
    "(" <arguments:CommaSeparated<Expression>> ")" <end:@R> =>
        ExpressionKind::MethodCall {
            receiver: Box::new(receiver),
            method,
            arguments
        }.with_span(file_id, start, end),
}
```

**Note**: Method call statements (e.g., `p.method();`) are handled by the existing expression statement grammar rule, which parses call-like expressions followed by `;` as `StatementKind::Expression`. No separate `StatementKind::MethodCall` is needed.

The method call rule must come after field access in the grammar to ensure proper precedence. LALRPOP will try alternatives in order and the method call pattern is more specific (requires parentheses).

## AST Changes

### `ast.rs`

Extend existing function types with optional receiver:

```rust
/// A function definition - methods are functions with a receiver_type
#[derive(Clone, Debug, Serialize)]
pub struct Function<'input> {
    /// If Some, this is a method on the given type (e.g., "Point", "fix").
    /// Uses Ident to preserve span for error messages.
    pub receiver_type: Option<Ident<'input>>,
    /// The function/method name
    pub name: &'input str,
    /// Function arguments (for methods, first should be `self`)
    pub arguments: Vec<TypedIdent<'input>>,
    /// Return type(s)
    pub return_types: FunctionReturn<'input>,
    /// Function body
    pub statements: Vec<Statement<'input>>,
    /// Span of the function name
    pub span: Span,
    /// Modifiers (event not allowed for methods)
    pub modifiers: FunctionModifiers,
    /// Metadata for compilation passes
    pub meta: Metadata,
}

/// A builtin function declaration - methods are builtins with a receiver_type
#[derive(Clone, Debug, Serialize)]
pub struct BuiltinFunction<'input> {
    /// If Some, this is a method on the given type.
    /// Uses Ident to preserve span for error messages.
    pub receiver_type: Option<Ident<'input>>,
    pub name: &'input str,
    pub builtin_id: BuiltinFunctionId,
    pub arguments: Vec<TypedIdent<'input>>,
    pub return_type: FunctionReturn<'input>,
    pub span: Span,
    pub meta: Metadata,
}

impl Function<'_> {
    /// Returns true if this is a method (has a receiver type)
    pub fn is_method(&self) -> bool {
        self.receiver_type.is_some()
    }

    /// Returns the mangled function name.
    /// For methods: "Type@method", for regular functions: just the name.
    pub fn mangled_name(&self) -> Cow<'_, str> {
        match &self.receiver_type {
            Some(receiver) => Cow::Owned(format!("{}@{}", receiver.ident, self.name)),
            None => Cow::Borrowed(self.name),
        }
    }
}

impl BuiltinFunction<'_> {
    pub fn is_method(&self) -> bool {
        self.receiver_type.is_some()
    }

    pub fn mangled_name(&self) -> Cow<'_, str> {
        match &self.receiver_type {
            Some(receiver) => Cow::Owned(format!("{}@{}", receiver.ident, self.name)),
            None => Cow::Borrowed(self.name),
        }
    }
}
```

Add method call to expressions only:

```rust
pub enum ExpressionKind<'input> {
    // ... existing variants ...

    /// Method call: receiver.method(args)
    MethodCall {
        receiver: Box<Expression<'input>>,
        method: Ident<'input>,
        arguments: Vec<Expression<'input>>,
    },
}
```

**Note**: Unlike the old `Call` pattern (which had both `ExpressionKind::Call` and `StatementKind::Call`), we only need `ExpressionKind::MethodCall`. Method call statements are handled by the existing `StatementKind::Expression` - e.g., `p.method();` parses as `StatementKind::Expression { expression: ExpressionKind::MethodCall { ... } }`.

No changes needed to `Script` or `TopLevelStatement` - methods are just `Function`s with `receiver_type = Some(...)`.

### Metadata for Method Calls

Store resolved method information in expression/statement metadata:

```rust
/// Stored in MethodCall expression/statement metadata during type checking.
/// Contains the resolved function ID for IR lowering.
#[derive(Clone, Copy, Debug)]
pub struct MethodCallInfo {
    /// The resolved function ID (maps to mangled name like Point@distance)
    pub function_id: InternalOrExternalFunctionId,
}
```

The receiver type is only needed during type checking to look up the method - once we have the function ID, that's all IR lowering needs.

## Type Resolution for Method Receiver Types

Methods can be defined on struct types or primitive types. During struct/type resolution, we need to resolve the `receiver_type` in function definitions that are methods.

### `struct_visitor.rs` additions

Extend the existing type resolution to handle method receiver types:

```rust
/// Resolve receiver_type in method definitions to actual Type values.
/// This runs after struct registration so struct names are available.
/// Call this as part of resolve_all_types or as a separate pass.
pub fn resolve_method_receiver_types<'input>(
    functions: &mut [Function<'input>],
    builtin_functions: &mut [BuiltinFunction<'input>],
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    for function in functions {
        if let Some(ref receiver) = function.receiver_type {
            let receiver_type = resolve_type_name(receiver.ident, struct_names, receiver.span, diagnostics);

            // Store resolved receiver type in metadata
            function.meta.set(ResolvedReceiverType(receiver_type));

            // Fill in self's type from receiver type (self never has explicit type)
            if let Some(self_param) = function.arguments.first_mut() {
                if self_param.name() == "self" {
                    self_param.set_resolved_type(receiver_type);
                }
            }
        }
    }

    for function in builtin_functions {
        if let Some(ref receiver) = function.receiver_type {
            let receiver_type = resolve_type_name(receiver.ident, struct_names, receiver.span, diagnostics);
            function.meta.set(ResolvedReceiverType(receiver_type));

            if let Some(self_param) = function.arguments.first_mut() {
                if self_param.name() == "self" {
                    self_param.set_resolved_type(receiver_type);
                }
            }
        }
    }
}

/// Stored in function metadata after receiver type resolution.
#[derive(Clone, Copy, Debug)]
pub struct ResolvedReceiverType(pub Type);

fn resolve_type_name(
    name: &str,
    struct_names: &HashMap<&str, StructId>,
    span: Span,
    diagnostics: &mut Diagnostics,
) -> Type {
    // Check primitive types first
    if let Some(ty) = Type::parse_builtin(name) {
        return ty;
    }

    // Check struct types
    if let Some(&id) = struct_names.get(name) {
        return Type::Struct(id);
    }

    ErrorKind::UnknownMethodType { name: name.to_string() }
        .at(span)
        .emit(diagnostics);
    Type::Error
}

```

## Symbol Table Changes

### `symtab_visitor.rs`

The existing function registration code already handles this - we just need to use `mangled_name()`:

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
            // Check for event modifier on methods (not allowed)
            if function.is_method() && function.modifiers.is_event_handler.is_some() {
                ErrorKind::MethodCannotBeEventHandler {
                    method_name: function.name.to_string(),
                }
                .at(function.modifiers.is_event_handler.unwrap())
                .emit(diagnostics);
                continue;
            }

            let fid = FunctionId(i);
            function.meta.set(fid);

            // Use mangled_name() - returns "Type@method" for methods, "name" for functions
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

        // Similar for builtin_functions - use mangled_name()
        for function in script.builtin_functions.iter_mut() {
            let fid = function.builtin_id;
            function.meta.set(fid);

            if !register_function(
                function.mangled_name().into_owned(),
                function.span,
                InternalOrExternalFunctionId::Builtin(fid),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            ) {
                continue;
            }

            // ... rest of builtin registration ...
        }

        // ... rest of initialization ...
    }
}
```

### Method Lookup

Add a method to look up methods by receiver type and name:

```rust
impl SymTab<'input> {
    /// Look up a method by receiver type and method name.
    pub fn lookup_method(
        &self,
        receiver_type: Type,
        method_name: &str,
        struct_registry: &StructRegistry,
    ) -> Option<InternalOrExternalFunctionId> {
        // Build the mangled name based on receiver type
        let type_name = match receiver_type {
            Type::Int => "int",
            Type::Fix => "fix",
            Type::Bool => "bool",
            Type::Struct(id) => &struct_registry.get(id).name,
            Type::Error => return None,
        };

        let mangled = format!("{}@{}", type_name, method_name);
        self.function_by_mangled_name(&mangled)
    }
}
```

## Type Checking Changes

### `type_visitor.rs`

Add handling for method calls in `type_for_expression`. Method call statements are handled automatically via the existing `StatementKind::Expression` case, which calls `type_for_expression` on the inner expression.

#### In `type_for_expression`:

```rust
fn type_for_expression(
    &mut self,
    expression: &mut Expression<'input>,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Type {
    match &mut expression.kind {
        // ... existing cases ...

        ExpressionKind::MethodCall { receiver, method, arguments } => {
            // Type check the receiver
            let receiver_type = self.type_for_expression(receiver, symtab, diagnostics);

            if receiver_type == Type::Error {
                return Type::Error;
            }

            // Look up the method
            let Some(function_id) = symtab.lookup_method(receiver_type, method.ident, self.struct_registry) else {
                ErrorKind::UnknownMethod {
                    type_name: receiver_type.display(self.struct_registry).to_string(),
                    method_name: method.ident.to_string(),
                }
                .at(method.span)
                .label(receiver.span, DiagnosticMessage::HasType { ty: receiver_type })
                .emit(diagnostics);
                return Type::Error;
            };

            // Type check arguments (receiver becomes implicit first argument)
            // The function's first parameter is `self`, which we skip in argument matching
            let function_info = self.functions.get(&function_id).unwrap();

            // Validate argument count (excluding self)
            let expected_args = function_info.args.len() - 1; // -1 for self
            if arguments.len() != expected_args {
                ErrorKind::IncorrectNumberOfArguments {
                    function_name: format!("{}.{}", receiver_type.display(self.struct_registry), method.ident),
                    expected: expected_args,
                    actual: arguments.len(),
                }
                .at(expression.span)
                .emit(diagnostics);
            }

            // Type check each argument (skip self parameter in function_info.args)
            for (arg, param_info) in arguments.iter_mut().zip(function_info.args.iter().skip(1)) {
                let arg_type = self.type_for_expression(arg, symtab, diagnostics);
                if arg_type != param_info.ty && arg_type != Type::Error && param_info.ty != Type::Error {
                    ErrorKind::FunctionArgumentTypeError {
                        function_name: format!("{}.{}", receiver_type.display(self.struct_registry), method.ident),
                        argument_name: param_info.name.to_string(),
                        expected: param_info.ty,
                        actual: arg_type,
                    }
                    .at(arg.span)
                    .emit(diagnostics);
                }
            }

            // Store method call info for IR lowering
            expression.meta.set(MethodCallInfo { function_id });

            // Return type is the function's return type
            if function_info.rets.len() == 1 {
                function_info.rets[0]
            } else if function_info.rets.is_empty() {
                Type::Error // void method used as expression
            } else {
                Type::Error // multi-return method used as expression
            }
        }
    }
}
```

**Note on statement form**: The existing `StatementKind::Expression` case in `visit_block` already handles expression statements by calling `type_for_expression` on the inner expression. For method calls used as statements, the method call expression will be type-checked through this path automatically.

However, there's a subtlety: `type_for_expression` for calls normally requires exactly one return value. For method calls used as statements (where the result is discarded), we need to allow any return count. The existing infrastructure handles this - the `StatementKind::Expression` case special-cases `ExpressionKind::Call` to use `type_for_call` directly. We'll need to extend this to also handle `ExpressionKind::MethodCall`:

```rust
StatementKind::Expression { expression } => {
    // For call/method call expressions, allow any return count since result is discarded
    match &mut expression.kind {
        ExpressionKind::Call { name, arguments } => {
            self.type_for_call(...);  // existing handling
        }
        ExpressionKind::MethodCall { receiver, method, arguments } => {
            self.type_check_method_call(receiver, method, arguments, ...);
        }
        _ => {
            self.type_for_expression(expression, symtab, diagnostics);
        }
    }
}
```

### Registering Functions in TypeVisitor

The existing registration loop handles methods automatically since they're just `Function`s:

```rust
impl<'input, 'reg> TypeVisitor<'input, 'reg> {
    pub fn new(
        functions: &[Function<'input>],
        // ... other params ...
    ) -> Self {
        // ... existing code ...

        for function in functions {
            let args = function.arguments.iter()
                .map(|arg| FunctionArgumentInfo {
                    name: Cow::Borrowed(arg.name()),
                    ty: arg.ty_required().resolved(),
                    span: arg.span(),
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::Internal(*function.meta.get().unwrap()),
                FunctionInfo {
                    // Use mangled_name() - works for both functions and methods
                    name: Box::leak(function.mangled_name().into_owned().into_boxed_str()),
                    span: function.span,
                    args,
                    rets: function.return_types.types.iter()
                        .map(|t| t.resolved())
                        .collect(),
                    modifiers: function.modifiers.clone(),
                },
            );
        }

        // Similar for builtin_functions...
    }
}
```

## IR Lowering Changes

### `lowering.rs`

Transform method calls into regular function calls in `blocks_for_expression`:

```rust
ExpressionKind::MethodCall { receiver, method: _, arguments } => {
    let info: &MethodCallInfo = expr.meta.get()
        .expect("MethodCallInfo should be set during type checking");

    // Lower the receiver - this becomes the first argument (self)
    let receiver_symbol = symtab.new_temporary();
    self.blocks_for_expression(receiver, receiver_symbol, symtab);

    // Lower the explicit arguments
    let mut all_args = vec![receiver_symbol];
    for arg in arguments {
        let arg_sym = symtab.new_temporary();
        self.blocks_for_expression(arg, arg_sym, symtab);
        all_args.push(arg_sym);
    }

    // Generate call IR (same as regular function call)
    // The target_symbol was passed in by the caller
    match info.function_id {
        InternalOrExternalFunctionId::Internal(f) => {
            self.current_block.push(TapIr::Call {
                target: Box::new([target_symbol]),
                f,
                args: all_args.into_boxed_slice(),
            });
        }
        InternalOrExternalFunctionId::Builtin(f) => {
            self.current_block.push(TapIr::CallBuiltin {
                target: target_symbol,
                f,
                args: all_args.into_boxed_slice(),
            });
        }
        // ... handle External, StructConstructor cases
    }
}
```

The key insight is that by the time we reach IR lowering, the method call has been fully resolved to a function ID. We just need to:

1. Evaluate the receiver expression
2. Pass it as the first argument (becomes `self`)
3. Emit a normal function call

Method bodies are compiled identically to regular function bodies - `self` is just another parameter.

**Statement form**: Method call statements are handled by the existing `StatementKind::Expression` case, which evaluates the inner expression into a temporary and discards the result. No special handling needed.

## Prelude Updates

### `stdlib/prelude.tapir`

Move existing rounding functions to method syntax:

```tapir
// Rounding methods on fix
builtin(3) fn fix.floor(self) -> int;
builtin(4) fn fix.ceil(self) -> int;
builtin(5) fn fix.round(self) -> int;

// Mathematical methods. Keep these global since it matches how you write maths better
builtin(0) fn sin(self) -> fix;
builtin(1) fn cos(self) -> fix;
builtin(2) fn sqrt(self) -> fix;

// Keep frame() as a regular function (no receiver)
builtin(-1) fn frame() -> int;
```

**Note**: This is a breaking change for existing code using `floor(x)` syntax.

## Diagnostic Changes

### Error Types

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Method defined on unknown type
    UnknownMethodType { name: String },

    /// Unknown method on type
    UnknownMethod {
        type_name: String,
        method_name: String,
    },

    /// Event modifier not allowed on methods
    MethodCannotBeEventHandler { method_name: String },
}
```

## Implementation Order

### Phase 1: Grammar and AST

1. **Lexer**: Add `self` keyword token
2. **Grammar**: Add `IdentOrSelf` rule so `self` can be used as a variable in expressions
3. **Grammar**: Add `MethodArguments` rule (handles `self` parameter without type)
4. **Grammar**: Extend `FunctionDefinition` and `BuiltinFunctionDecl` with optional receiver type
5. **Grammar**: Add method call to `PostfixExpr` (expression form only)
6. **AST**: Add `receiver_type: Option<Ident<'input>>` to `Function` and `BuiltinFunction`
7. **AST**: Add helper methods: `is_method()`, `mangled_name()`
8. **AST**: Add `MethodCall` variant to `ExpressionKind` only (statements use `StatementKind::Expression`)
9. **AST**: Add `MethodCallInfo` metadata struct
10. **Tests**: Parser snapshot tests for method definitions and calls

**Compilable**: Yes - methods parse as regular functions with receiver_type set

### Phase 2: Receiver Type Resolution

1. **struct_visitor**: Add `resolve_method_receiver_types` function
2. **struct_visitor**: Fill in `self` type from receiver type (self never has explicit type)
3. **struct_visitor**: Add `ResolvedReceiverType` metadata struct
4. **Integrate**: Call in compilation pipeline after struct registration
5. **Tests**: Method receiver type resolution tests

**Compilable**: Yes - receiver types resolved but not used in registration

### Phase 3: Symbol Table Registration

1. **symtab_visitor**: Use `mangled_name()` when registering functions
2. **symtab_visitor**: Check for event modifier on methods (error)
3. **SymTab**: Add `lookup_method` function
4. **Tests**: Symbol resolution tests for methods

**Compilable**: Yes - methods registered with mangled names

### Phase 4: Type Checking

1. **type_visitor**: Use `mangled_name()` when building function info
2. **type_visitor**: Handle `ExpressionKind::MethodCall` in `type_for_expression`
3. **type_visitor**: Extend `StatementKind::Expression` handling to also check for `MethodCall` (similar to existing `Call` handling)
4. **type_visitor**: Store `MethodCallInfo` metadata on method calls
5. **Tests**: Type checking tests for method calls (both expression and statement forms)

**Compilable**: Yes - method calls type checked

### Phase 5: IR Lowering

1. **lowering**: Handle `ExpressionKind::MethodCall` in `blocks_for_expression` - prepend receiver, emit call
2. **Tests**: IR snapshot tests for method calls

**Note**: Method call statements are handled automatically by the existing `StatementKind::Expression` case, which calls `blocks_for_expression` on the inner expression.

**Compilable**: Yes - full method support

### Phase 6: Prelude Migration

1. **prelude.tapir**: Convert `floor`, `ceil`, `round` to method syntax
2. **prelude.tapir**: Convert `sin`, `cos`, `sqrt` to method syntax
3. **Tests**: Update existing tests for new syntax
4. **Documentation**: Update language reference

## Testing Strategy

### Parser Tests

- Method with self parses correctly: `fn Point.foo(self) { }`
- Method with self and other args: `fn Point.foo(self, x: int) { }`
- Using self in method body: `fn Point.foo(self) { return self; }`
- Accessing self fields: `fn Point.get_x(self) -> fix { return self.x; }`
- Static method (no self): `fn Point.origin() -> Point { }`
- Builtin method parses correctly: `builtin(5) fn fix.round(self) -> int;`
- Method call expression parses correctly: `var x = p.method()`
- Method call statement parses correctly: `p.method();`
- Method call on expression result parses correctly: `get_point().method()`
- Chained method calls parse correctly: `p.method1().method2()`
- Field access vs method call disambiguation: `p.x` vs `p.x()`
- Regular functions still parse correctly (no regression)

### Type Checking Tests

- Method on struct type resolves correctly
- Method on primitive type (`int`, `fix`) resolves correctly
- Method on primitive type from expression resolves correctly (`(1.3 + 2.9).floor()`)
- Wrong argument count produces error (accounting for implicit self)
- Wrong argument types produces error
- Unknown method on type produces error
- Event modifier on method produces error
- Method call on non-method-having type produces error

### IR Tests

- Method call expression generates correct IR (call with receiver as first arg)
- Method call statement generates correct IR (expression statement wrapping method call)
- Receiver expression evaluated before explicit arguments
- Chained method calls generate correct IR
- Method body compiles same as regular function body

### Runtime Tests

- Method call executes correctly
- Builtin method works (`x.round()`)
- Method returning struct works
- Chained method calls work
- Method accessing `self` fields works

## Future Extensions

### Static Methods

Add support for methods without `self`:

```tapir
fn Point.origin() -> Point {
    return Point(0.0, 0.0);
}

var p = Point.origin();  // Static method call
```

This would require:

- Grammar for static method calls: `identifier "." identifier "(" args ")"`
- Distinguishing static vs instance at call site
- No implicit receiver argument in IR
