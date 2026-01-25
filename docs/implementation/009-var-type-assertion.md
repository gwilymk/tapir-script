# Variable and Global Type Annotations with Unified TypedIdent

## Overview

This document outlines two related changes:

1. **Optional type annotations** for variable and global declarations (currently types are inferred)
2. **Unified `TypedIdent` AST structure** replacing the separate `FunctionArgument`, property name+type, and variable/global identifier patterns

These changes allow explicit type annotations for clarity:

```tapir
var x: int = 3;
var ratio: fix = 1.5;
var enabled: bool = true;

global SPEED: int = 10;
global GRAVITY: fix = 0.5;
```

## Motivation

### Type Annotations

While type inference works well, explicit type annotations provide several benefits:

1. **Documentation**: Makes programmer intent clear when reading code
2. **Error catching**: Annotations catch mistakes when the RHS type doesn't match expectations
3. **Consistency**: Function parameters and properties already use `: type` syntax; variables should too

### Unified TypedIdent

The AST currently has multiple structures for "name with type":
- `FunctionArgument` with `MaybeResolved` name
- `PropertyDeclaration` with separate `name` and `ty` fields
- `Ident` for variables (no type)

Unifying these provides:
1. **Simpler AST**: One pattern instead of three
2. **Immutable AST**: Remove `MaybeResolved` mutation during symbol resolution
3. **Easier maintenance**: Changes to identifier handling affect one struct

## Current State

### Variable Declaration Syntax

```tapir
var x = 3;
var a, b = 1, 2;
```

### Global Declaration Syntax

```tapir
global SPEED = 10;
```

### AST Structure (`ast.rs`)

```rust
pub enum StatementKind<'input> {
    VariableDeclaration {
        idents: Vec<Ident<'input>>,
        values: Vec<Expression<'input>>,
    },
    // ...
}

pub struct GlobalDeclaration<'input> {
    pub name: Ident<'input>,
    pub value: Expression<'input>,
    pub span: Span,
}

pub struct Ident<'input> {
    pub span: Span,
    pub ident: &'input str,
}
```

### Related Structures (to be unified)

There are similar "name with type" patterns elsewhere in the AST that should be unified:

```rust
// Function arguments - required type, uses MaybeResolved for symbol resolution
pub struct FunctionArgument<'input> {
    pub span: Span,
    pub t: TypeWithLocation,
    pub name: MaybeResolved<'input>,
}

pub enum MaybeResolved<'input> {
    Unresolved(&'input str),
    Resolved(SymbolId),  // mutated during symbol resolution
}

// Properties - required type, no initializer
pub struct PropertyDeclaration<'input> {
    pub name: Ident<'input>,
    pub ty: TypeWithLocation,
    pub span: Span,
}
```

All four patterns (var, global, fn arg, property) share the same structure: a name with an associated type. The differences are:
- Whether the type is required (fn arg, property) or optional (var, global)
- Whether there's an initializer (var, global) or not (fn arg, property)

The type requirement can be enforced by the grammar. The initializer is a separate concern handled by the containing structure.

### Grammar (`grammar.lalrpop`)

```lalrpop
StatementKind: StatementKind<'input> = {
    var <idents: CommaSeparated<Identifier>> "=" <values: CommaSeparated<Expression>> ";"
        => StatementKind::VariableDeclaration { <> },
    // ...
}

GlobalDecl: GlobalDeclaration<'input> = {
    <start:@L> global <name:Identifier> "=" <value:Expression> ";" <end:@R> =>
        GlobalDeclaration { name, value, span: Span::new(file_id, start, end) },
}
```

### Type Checking (`type_visitor.rs`)

Types are inferred from the RHS expression:

```rust
(self.type_for_expression(v, symtab, diagnostics), span)
```

## Design Decisions

### Syntax Choice

Use `: type` after the identifier, consistent with function parameters and properties:

```tapir
# Proposed syntax
var x: int = 5;
var ratio: fix = 3.14;
global SPEED: int = 10;

# Existing similar syntax
fn foo(x: int) { }       # Function parameters
property pos: int;       # Properties
```

### Optional vs Required

Annotations are **optional**. When omitted, behavior is unchanged (infer from RHS). This maintains backward compatibility and allows concise code when types are obvious.

### Multi-Variable Declarations

For declarations like `var a, b = 1, 2;`, each variable can optionally have its own annotation:

```tapir
var a: int, b: int = 1, 2;
var x: int, y = 1, 2;       # Mix annotated and inferred
var a: int, b: fix = 1, 2;  # Different types
```

### Strict Type Matching

Type annotations must exactly match the RHS expression type. There is no implicit coercion:

| Annotation | RHS Type | Result                     |
| ---------- | -------- | -------------------------- |
| `int`      | `int`    | OK                         |
| `fix`      | `fix`    | OK                         |
| `bool`     | `bool`   | OK                         |
| `fix`      | `int`    | Error - type mismatch      |
| `int`      | `fix`    | Error - type mismatch      |
| `bool`     | `int`    | Error - type mismatch      |

This keeps the type system simple and explicit. Future work may add explicit conversion functions like `round()` and `floor()` for fix-to-int conversion.

## Implementation Plan

### Phase 1: AST Changes - Unified TypedIdent

**File**: `compiler/src/ast.rs`

Create a unified struct for identifiers with optional type annotations. This replaces multiple similar patterns across the AST:

```rust
/// An identifier with an optional type annotation.
///
/// Used throughout the AST for any name that may have a type:
/// - Variable declarations: `var x: int = 5;` (type optional, inferred if absent)
/// - Global declarations: `global G: int = 10;` (type optional, inferred if absent)
/// - Function arguments: `fn foo(x: int)` (type required by grammar)
/// - Property declarations: `property p: int;` (type required by grammar)
///
/// Whether the type is required or optional is enforced by the grammar, not the AST.
#[derive(Clone, Debug, Serialize)]
pub struct TypedIdent<'input> {
    pub span: Span,
    pub ident: &'input str,
    pub ty: Option<TypeWithLocation>,
}

impl<'input> TypedIdent<'input> {
    /// Returns the type, panicking if not present.
    /// Use only when the grammar guarantees a type exists.
    pub fn ty_required(&self) -> &TypeWithLocation {
        self.ty.as_ref().expect("type required by grammar")
    }
}
```

#### Structures to Update

**VariableDeclaration** - uses `TypedIdent` with optional type:

```rust
pub enum StatementKind<'input> {
    VariableDeclaration {
        idents: Vec<TypedIdent<'input>>,
        values: Vec<Expression<'input>>,
    },
    // ...
}
```

**GlobalDeclaration** - uses `TypedIdent` with optional type:

```rust
pub struct GlobalDeclaration<'input> {
    pub name: TypedIdent<'input>,
    pub value: Expression<'input>,
    pub span: Span,
}
```

**FunctionArgument** - replaced by `TypedIdent` (type required by grammar):

```rust
// REMOVE the old FunctionArgument struct:
// pub struct FunctionArgument<'input> {
//     pub span: Span,
//     pub t: TypeWithLocation,
//     pub name: MaybeResolved<'input>,
// }

// REMOVE MaybeResolved - symbol resolution uses a side table instead:
// pub enum MaybeResolved<'input> {
//     Unresolved(&'input str),
//     Resolved(SymbolId),
// }

// Function now uses TypedIdent directly:
pub struct Function<'input> {
    pub name: &'input str,
    pub arguments: Vec<TypedIdent<'input>>,  // was Vec<FunctionArgument<'input>>
    pub return_type: FunctionReturn,
    // ...
}
```

**PropertyDeclaration** - uses `TypedIdent` (type required by grammar):

```rust
pub struct PropertyDeclaration<'input> {
    pub name: TypedIdent<'input>,  // was separate name: Ident and ty: TypeWithLocation
    pub span: Span,
}
```

#### Removing MaybeResolved

The old `FunctionArgument` used `MaybeResolved` to store symbol IDs after resolution. This mutated the AST during compilation, which is awkward. Instead, symbol resolution should store the mapping in a side table:

```rust
// In SymTab or a dedicated structure
pub struct ArgumentSymbols {
    /// Maps (function_name, argument_index) -> SymbolId
    symbols: HashMap<(&'input str, usize), SymbolId>,
}
```

This keeps the AST immutable after parsing.

### Phase 2: Grammar Changes

**File**: `compiler/src/grammar.lalrpop`

Add two grammar rules - one for optional types, one for required types:

```lalrpop
// Optional type annotation (for var, global)
TypedIdentifier: TypedIdent<'input> = {
    <start:@L> <ident:identifier> <end:@R> =>
        TypedIdent {
            span: Span::new(file_id, start, end),
            ident,
            ty: None
        },
    <start:@L> <ident:identifier> ":" <ty:Type> <end:@R> =>
        TypedIdent {
            span: Span::new(file_id, start, end),
            ident,
            ty: Some(ty)
        },
}

// Required type annotation (for function args, properties)
TypedIdentifierRequired: TypedIdent<'input> = {
    <start:@L> <ident:identifier> ":" <ty:Type> <end:@R> =>
        TypedIdent {
            span: Span::new(file_id, start, end),
            ident,
            ty: Some(ty)
        },
}
```

Update **variable declaration** to use `TypedIdentifier` (optional):

```lalrpop
StatementKind: StatementKind<'input> = {
    var <idents: CommaSeparated<TypedIdentifier>> "=" <values: CommaSeparated<Expression>> ";"
        => StatementKind::VariableDeclaration { <> },
    // ...
}
```

Update **global declaration** to use `TypedIdentifier` (optional):

```lalrpop
GlobalDecl: GlobalDeclaration<'input> = {
    <start:@L> global <name:TypedIdentifier> "=" <value:Expression> ";" <end:@R> =>
        GlobalDeclaration { name, value, span: Span::new(file_id, start, end) },
}
```

Update **function arguments** to use `TypedIdentifierRequired`:

```lalrpop
// Old rule (REMOVE):
// FunctionArgument: FunctionArgument<'input> =
//     <start: @L> <name: identifier> <end: @R> ":" <t: Type>
//         => FunctionArgument { span: Span::new(file_id, start, end), t, name: MaybeResolved::Unresolved(name) };

// New rule:
FunctionArgument: TypedIdent<'input> = <TypedIdentifierRequired>;
```

Update **property declarations** to use `TypedIdentifierRequired`:

```lalrpop
// Old rule:
// PropertyDecl: PropertyDeclaration<'input> = {
//     <start:@L> property <name:Identifier> ":" <ty:Type> ";" <end:@R> =>
//         PropertyDeclaration { name, ty, span: Span::new(file_id, start, end) },
// }

// New rule:
PropertyDecl: PropertyDeclaration<'input> = {
    <start:@L> property <name:TypedIdentifierRequired> ";" <end:@R> =>
        PropertyDeclaration { name, span: Span::new(file_id, start, end) },
}
```

### Phase 3: Symbol Table Updates

**File**: `compiler/src/compile/symtab_visitor.rs`

When processing variable declarations, extract any type annotation for later use by the type checker:

```rust
StatementKind::VariableDeclaration { idents, values } => {
    for (i, ident) in idents.iter().enumerate() {
        let symbol_id = self.symtab.new_symbol(ident.ident, ident.span);
        self.symbol_names.insert(ident.ident, symbol_id);
        statement_meta.push(symbol_id);

        // Store type annotation if present
        if let Some(ref ty) = ident.ty {
            self.symtab.set_annotated_type(symbol_id, ty.t);
        }
    }
    statement.meta.set(statement_meta);
}
```

Similarly for global declarations (in the top-level visitor):

```rust
TopLevelStatement::GlobalDeclaration(decl) => {
    let symbol_id = self.symtab.new_symbol(decl.name.ident, decl.name.span);
    // ... existing global handling ...

    // Store type annotation if present
    if let Some(ref ty) = decl.name.ty {
        self.symtab.set_annotated_type(symbol_id, ty.t);
    }
}
```

Add storage for annotated types in the symbol table:

```rust
pub struct SymTab<'input> {
    // ... existing fields ...

    /// Types explicitly annotated by the user (not inferred)
    annotated_types: HashMap<SymbolId, Type>,
}

impl<'input> SymTab<'input> {
    pub fn set_annotated_type(&mut self, symbol: SymbolId, ty: Type) {
        self.annotated_types.insert(symbol, ty);
    }

    pub fn get_annotated_type(&self, symbol: SymbolId) -> Option<Type> {
        self.annotated_types.get(&symbol).copied()
    }
}
```

### Phase 4: Type Checking Updates

**File**: `compiler/src/compile/type_visitor.rs`

Extract a helper function to validate type annotations:

```rust
fn check_type_annotation(
    &mut self,
    annotation: Option<&TypeWithLocation>,
    inferred_type: Type,
    span: Span,
    diagnostics: &mut Diagnostics,
) -> Type {
    if let Some(annotation) = annotation {
        let annotated = annotation.t;

        // Strict type matching - no implicit coercion
        let types_match = annotated == inferred_type
            || annotated == Type::Error
            || inferred_type == Type::Error;

        if !types_match {
            ErrorKind::TypeAnnotationMismatch {
                annotated,
                actual: inferred_type,
            }
            .at(span)
            .with_label(annotation.span, format!("expected {annotated}"))
            .emit(diagnostics);
            Type::Error
        } else {
            annotated
        }
    } else {
        // No annotation - use inferred type
        inferred_type
    }
}
```

Update `visit_variable_declaration` to use it:

```rust
fn visit_variable_declaration(
    &mut self,
    idents: &[TypedIdent],
    values: &[Expression],
    statement_meta: &[SymbolId],
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) {
    // ... existing value type computation ...

    for (i, (ident, &symbol_id)) in idents.iter().zip(statement_meta).enumerate() {
        let inferred_type = value_types.get(i).map(|(t, _)| *t).unwrap_or(Type::Error);

        let final_type = self.check_type_annotation(
            ident.ty.as_ref(),
            inferred_type,
            ident.span,
            diagnostics,
        );

        self.resolve_type_with_spans(symbol_id, final_type, ident.span, symtab, diagnostics);
    }
}
```

Update global declaration handling similarly:

```rust
fn visit_global_declaration(
    &mut self,
    decl: &GlobalDeclaration,
    symbol_id: SymbolId,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) {
    let inferred_type = self.type_for_expression(&decl.value, symtab, diagnostics);

    let final_type = self.check_type_annotation(
        decl.name.ty.as_ref(),
        inferred_type,
        decl.name.span,
        diagnostics,
    );

    self.resolve_type_with_spans(symbol_id, final_type, decl.name.span, symtab, diagnostics);
}
```

### Phase 5: Error Messages

**File**: `compiler/src/reporting.rs`

Add new error variant:

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Type annotation doesn't match the expression type
    TypeAnnotationMismatch {
        annotated: Type,
        actual: Type,
    },
}

impl ErrorKind {
    pub fn message(&self) -> String {
        match self {
            // ...
            ErrorKind::TypeAnnotationMismatch { annotated, actual } => {
                format!(
                    "type annotation `{annotated}` doesn't match expression type `{actual}`"
                )
            }
        }
    }
}
```

## Testing Strategy

### Parser Tests

```tapir
# Basic variable annotation
var x: int = 5;

# All types
var a: int = 1;
var b: fix = 1.5;
var c: bool = true;

# Multi-variable with annotations
var x: int, y: int = 1, 2;
var a: int, b: fix = 1, 2.0;

# Mixed annotated and inferred
var x: int, y = 1, 2;

# Global annotations
global SPEED: int = 10;
global GRAVITY: fix = 0.5;
global DEBUG: bool = false;
```

### Type Checking Tests

```tapir
# Valid: annotation matches RHS
var x: int = 5;         # OK
var y: fix = 3.14;      # OK
var z: bool = true;     # OK
global G: int = 10;     # OK

# Invalid: type mismatch (no implicit coercion)
var a: fix = 5;         # Error: type annotation `fix` doesn't match expression type `int`
var b: int = 3.14;      # Error: type annotation `int` doesn't match expression type `fix`
var c: bool = 5;        # Error: type annotation `bool` doesn't match expression type `int`
global H: fix = 10;     # Error: type annotation `fix` doesn't match expression type `int`
```

## Implementation Order

1. **Phase 1**: AST changes
   - Add `TypedIdent` struct
   - Update `VariableDeclaration`, `GlobalDeclaration` to use it
   - Update `Function.arguments` to use `Vec<TypedIdent>`
   - Update `PropertyDeclaration` to use `TypedIdent`
   - Remove `FunctionArgument` struct
   - Remove `MaybeResolved` enum

2. **Phase 2**: Grammar changes
   - Add `TypedIdentifier` (optional type) and `TypedIdentifierRequired` rules
   - Update all declaration rules to use appropriate variant

3. **Phase 3**: Symbol table
   - Update symbol resolution for function arguments (use side table instead of `MaybeResolved`)
   - Add `annotated_types` storage for optional type annotations

4. **Phase 4**: Type checking
   - Add `check_type_annotation` helper
   - Update variable and global declaration handling
   - Verify function argument and property handling still works (types always present)

5. **Phase 5**: Error messages - add `TypeAnnotationMismatch` variant

Each phase should include relevant tests before moving to the next.

## Edge Cases

### Annotation on Multi-Return Function

```tapir
var a: int, b: int = some_function();  # function returns (int, int)
```

The annotations must match the function's return types.

### Annotation with Inferred RHS Type

```tapir
var x: fix = y + z;  # y and z must be fix for this to compile
```

The RHS expression type must match the annotation exactly.

### Duplicate Variable with Different Annotation

```tapir
var x: int = 5;
var x: fix = 3.14;  # Error: variable already declared (existing error)
```

The existing duplicate variable error takes precedence.

## Future Extensions

- **Explicit conversion functions**: `round(x)` and `floor(x)` for fix-to-int conversion
- **Vector type annotations**: `var v: int2 = [1, 2];` once vectors are implemented
- **Const declarations**: `const PI: fix = 3.14159;` with required type annotation
