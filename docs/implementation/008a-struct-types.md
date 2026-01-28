# Struct Types (Part 1 of 3)

## Overview

This document covers the foundation for user-defined struct types: the type system, parsing, registration, and constructor functions. After completing this part, you can define structs and type-check constructor calls, but can't yet use struct values (field access, assignment, etc.).

See also:
- [008b-struct-operations.md](008b-struct-operations.md) - Field access, assignment, function params
- [008c-struct-rust-integration.md](008c-struct-rust-integration.md) - Properties, macro updates

## Motivation

Games often need to group related values together. Positions, velocities, rectangles, and other compound data are fundamental. Currently, users must manage components separately:

```tapir
property pos_x: int;
property pos_y: int;

pos_x = pos_x + vel_x;
pos_y = pos_y + vel_y;
```

With structs:

```tapir
struct Point { x: int, y: int }

property pos: Point;

pos.x = pos.x + vel.x;
pos.y = pos.y + vel.y;
```

This is more readable, less error-prone, and better organizes related data.

## Design Decisions

### Nominal Typing

Struct types are nominal, not structural. Two structs with identical fields are distinct types:

```tapir
struct Point { x: int, y: int }
struct Size { x: int, y: int }

var p: Point = Point(1, 2);
var s: Size = p;  // ERROR: type mismatch
```

### Scalarization at IR (Struct Flattening)

All struct types are purely a frontend abstraction. At the IR phase, every struct becomes N scalar values (one per field):

- `Point { x: int, y: int }` becomes two `int` values
- Nested structs flatten recursively

**Advantages:**

- No bytecode changes required
- No VM changes required
- Existing optimizations apply to each field
- Register allocation works naturally

**Trade-offs:**

- Cannot exploit SIMD operations (not relevant for this VM)
- Each struct variable uses N symbol IDs (one per field)

### Struct Syntax

**Declaration (top-level only):**

```tapir
struct Point { x: int, y: int }
struct Rect { origin: Point, size: Point }
```

**Construction (implicit constructor function):**

```tapir
var p = Point(1, 2);           // positional arguments in field order
var r = Rect(Point(0, 0), Point(10, 20));
```

Each struct declaration implicitly creates a constructor function with the same name. Arguments are positional, matching field declaration order.

### Nested Structs

Structs can contain other structs as fields:

```tapir
struct Line { start: Point, end: Point }
```

This scalarizes recursively - `Line` becomes 4 scalar values (`start.x`, `start.y`, `end.x`, `end.y`).

## Type System Changes

### The Problem

The current `Type` enum is `Copy`:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type { Int, Fix, Bool, Error }
```

Struct types need to reference their definition (field names, field types), which can't be stored inline in a `Copy` type.

### Solution: StructId and StructRegistry

Introduce `StructId` as an index into a registry:

```rust
/// Unique identifier for a struct type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct StructId(pub u32);

/// Definition of a struct type
#[derive(Clone, Debug, Serialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Registry of all struct definitions
#[derive(Default, Debug, Serialize)]
pub struct StructRegistry {
    structs: Vec<StructDef>,
}

impl StructRegistry {
    pub fn register(&mut self, def: StructDef) -> StructId {
        let id = StructId(self.structs.len() as u32);
        self.structs.push(def);
        id
    }

    pub fn get(&self, id: StructId) -> &StructDef {
        &self.structs[id.0 as usize]
    }
}
```

Update the `Type` enum:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default, Serialize)]
pub enum Type {
    Int,
    Fix,
    Bool,
    Struct(StructId),  // NEW

    #[default]
    Error,
}

impl Type {
    /// Parse a builtin type name. Returns None for non-builtin names.
    /// Used by type resolution and struct registration (to prevent shadowing).
    pub fn parse_builtin(name: &str) -> Option<Type> {
        match name {
            "int" => Some(Type::Int),
            "fix" => Some(Type::Fix),
            "bool" => Some(Type::Bool),
            _ => None,
        }
    }

    /// Returns whether this is a struct type
    pub fn is_struct(self) -> bool {
        matches!(self, Type::Struct(_))
    }

    /// Returns the StructId if this is a struct type
    pub fn as_struct(self) -> Option<StructId> {
        match self {
            Type::Struct(id) => Some(id),
            _ => None,
        }
    }
}
```

The `StructRegistry` is stored alongside the symbol table and passed through compilation phases.

### Type Display

Displaying a struct type requires access to the registry:

```rust
impl Type {
    pub fn display<'a>(&self, registry: &'a StructRegistry) -> impl Display + 'a {
        struct TypeDisplay<'a> {
            ty: Type,
            registry: &'a StructRegistry,
        }

        impl Display for TypeDisplay<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.ty {
                    Type::Int => write!(f, "int"),
                    Type::Fix => write!(f, "fix"),
                    Type::Bool => write!(f, "bool"),
                    Type::Struct(id) => write!(f, "{}", self.registry.get(id).name),
                    Type::Error => write!(f, "unknown"),
                }
            }
        }

        TypeDisplay { ty: *self, registry }
    }
}
```

## Implementation Plan

Each phase leaves the project in a compilable, testable state.

### Phase 1: Type System Foundation

**Files**: `compiler/src/types.rs`

Add the core types without connecting them to anything yet:

```rust
/// Unique identifier for a struct type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct StructId(pub u32);

#[derive(Clone, Debug, Serialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Default, Debug, Serialize)]
pub struct StructRegistry {
    structs: Vec<StructDef>,
}

impl StructRegistry {
    pub fn register(&mut self, def: StructDef) -> StructId { ... }
    pub fn get(&self, id: StructId) -> &StructDef { ... }
    pub fn get_mut(&mut self, id: StructId) -> &mut StructDef { ... }
}
```

Add `Type::Struct` variant and update ALL existing match statements to handle it. For now, most handlers return `Type::Error` or panic with "structs not yet supported":

```rust
pub enum Type {
    Int, Fix, Bool,
    Struct(StructId),  // NEW
    Error,
}
```

**Tests**:

- Unit tests for `StructRegistry`: register, get, get_mut
- Unit tests for `Type` helper methods: `is_struct()`, `as_struct()`
- Unit tests for `Type::parse_builtin`: returns `Some` for "int"/"fix"/"bool", `None` for others
- Verify all existing tests still pass

**Compilable**: Yes - existing code handles new variant (even if just to error)

---

### Phase 2: Lexer & Parser for Struct Declarations

**Files**: `compiler/src/tokens.rs`, `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`

Add struct declaration syntax only (no field access yet):

```rust
// tokens.rs
#[token("struct")]
KeywordStruct,

// ast.rs
#[derive(Clone, Debug, Serialize)]
pub struct StructDeclaration<'input> {
    pub name: Ident<'input>,  // Ident includes span for precise error pointing
    pub fields: Vec<StructFieldDecl<'input>>,
    pub span: Span,           // Span of entire declaration
}

// For fields, we reuse TypedIdent but the grammar enforces the type is present.
// TypedIdent.ty will always be Some(...) for struct fields.
pub type StructFieldDecl<'input> = TypedIdent<'input>;

// Add to Script
pub struct_declarations: Vec<StructDeclaration<'input>>,
```

Using `Ident` for the struct name gives us a precise span for error messages like "the struct `Point` defined here" without pointing at the entire definition. Reusing `TypedIdent` for fields keeps consistency with function arguments (which also use `TypedIdent`).

```lalrpop
StructDeclaration: StructDeclaration<'input> = {
    <start:@L> "struct" <name:Ident> "{" <fields:Comma<StructField>> "}" <end:@R> => ...
};
```

Struct declarations are parsed and stored in `Script`, but not processed further yet.

**Tests**:

- Parser snapshot tests: valid struct declarations parse correctly
- Parser error tests: malformed struct syntax produces errors
- Existing tests still pass (structs are parsed but ignored)

**Compilable**: Yes - struct_declarations collected but unused

---

### Phase 3: Struct Registration

**Files**: `compiler/src/compile/struct_visitor.rs` (new), `compiler/src/compile/compile.rs`

Create the struct registration pass that populates `StructRegistry`:

```rust
/// First pass: Register all struct names and allocate StructIds.
/// Fields are left empty - we just need the names to exist so structs
/// can reference each other (e.g., `struct Rect { origin: Point, ... }`).
///
/// Checks that struct names don't shadow builtin types or other structs.
pub fn register_structs<'input>(
    script: &Script<'input>,
    registry: &mut StructRegistry,
    diagnostics: &mut Diagnostics,
) -> HashMap<&'input str, StructId> {
    let mut struct_names = HashMap::new();

    for decl in &script.struct_declarations {
        let name = decl.name.ident;

        // Check for shadowing builtin types (uses Type::parse_builtin from Phase 1)
        if Type::parse_builtin(name).is_some() {
            ErrorKind::StructShadowsBuiltinType { name: name.to_string() }
                .at(decl.name.span)
                .emit(diagnostics);
            continue;
        }

        // Check for duplicate struct names
        if struct_names.contains_key(name) {
            ErrorKind::DuplicateStructName { name: name.to_string() }
                .at(decl.name.span)
                .emit(diagnostics);
            continue;
        }

        // Register the struct
        let def = StructDef {
            name: name.to_string(),
            fields: vec![], // Populated in second pass
            span: decl.span,
        };
        let id = registry.register(def);
        struct_names.insert(name, id);
    }

    struct_names
}

/// Second pass: Now that all struct names are registered, resolve field types.
/// This converts AST type annotations (which may be struct names like "Point")
/// into actual Type values (Type::Struct(id)). Also detects recursive structs.
pub fn resolve_struct_fields<'input>(
    script: &Script<'input>,
    registry: &mut StructRegistry,
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) { ... }
```

The two-pass approach is needed because struct A can reference struct B and vice versa in their field types. We need all struct names registered before we can resolve any field types.

Wire into compilation pipeline early (before symtab), but don't use the registry anywhere else yet.

**Tests**:

- Unit tests for registration: structs get unique IDs
- Unit tests for field resolution: field types resolved correctly
- Unit tests for field resolution where field types are defined after the field
- Error tests: duplicate struct names, unknown field types
- Error test: `struct int { x: int }` - can't shadow builtin type
- Error test: duplicate struct field names `struct Foo { x: int, x: int }`
- Existing tests still pass

**Compilable**: Yes - registry populated but not used downstream

---

### Phase 4: Named Types in Grammar

**Files**: `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`, `compiler/src/tokens.rs`, `compiler/src/compile/type_visitor.rs`

Currently the grammar has `int`, `fix`, `bool` as keywords that produce specific type variants. Instead, treat all type annotations as identifiers and resolve them in the type visitor.

**Remove type keywords from lexer** (`tokens.rs`):

Remove `KeywordInt`, `KeywordFix`, `KeywordBool` tokens (or repurpose them only for literals if needed elsewhere).

**Simplify grammar** (`grammar.lalrpop`):

```lalrpop
// All types are now just identifiers - resolved later
Type: TypeWithLocation = {
    <start:@L> <name:Ident> <end:@R> => TypeWithLocation {
        t: ParsedType::Named(name),
        span: Span::new(file_id, start, end),
    },
};
```

**Type resolution** (`type_visitor.rs`):

The type visitor resolves type names to actual `Type` values, using `Type::parse_builtin` from Phase 1:

```rust
fn resolve_type(name: &str, struct_names: &HashMap<&str, StructId>, diagnostics: &mut Diagnostics, span: Span) -> Type {
    // Check builtin types first
    if let Some(builtin) = Type::parse_builtin(name) {
        return builtin;
    }

    // Then check user-defined structs
    if let Some(&id) = struct_names.get(name) {
        return Type::Struct(id);
    }

    // Unknown type
    ErrorKind::UnknownType { name: name.to_string() }
        .at(span)
        .emit(diagnostics);
    Type::Error
}
```

This gives uniform treatment to all types - builtins and user-defined structs go through the same resolution path.

**Note on namespaces**: Type names and value names are separate namespaces. `var int = 3;` is allowed (confusing but harmless) because the variable `int` doesn't conflict with the type `int` - they're looked up in different contexts.

This allows function signatures with struct types even before constructors work:

```tapir
fn foo(p: Point) -> int { return 0; }
```

**Tests**:

- `fn foo(p: Point) -> int { return 0; }` - struct type in function signature
- `fn bar() -> Point { ... }` - struct as return type (body will error, but signature parses)
- `var x: int = 3;` - builtin types still work
- Unknown type names produce errors
- Existing tests still pass

**Compilable**: Yes - struct types recognized in signatures but can't construct values yet

---

### Phase 5: Constructor Functions

**Files**: `compiler/src/compile/symtab_visitor.rs`, `compiler/src/compile/type_visitor.rs`

Register an implicit constructor function for each struct (note: `ResolvedFunctionId` will already have `Builtin` variant from 010-builtins.md):

```rust
// Extend ResolvedFunctionId (Builtin variant already exists from 010-builtins.md)
pub enum ResolvedFunctionId {
    Internal(FunctionId),
    External(ExternalFunctionId),
    Builtin(BuiltinFunctionId),
    StructConstructor(StructId),  // NEW
}
```

Type checking for constructor calls validates argument count and types.

For now, IR lowering for constructors can emit a placeholder or error - we'll implement scalarization in 008b.

**Tests**:

- `var p = Point(1, 2);` type-checks correctly (type inferred as `Point`)
- `var p: Point = Point(1, 2);` - explicit type annotation works
- Wrong argument count/types produce errors
- Existing tests still pass

**Compilable**: Yes - constructors type-check but IR lowering incomplete

---

## Implementation Order Summary

| Phase | What's Added                | Can Test                     |
| ----- | --------------------------- | ---------------------------- |
| 1     | Type system types           | StructRegistry unit tests    |
| 2     | Parsing struct declarations | Parser tests                 |
| 3     | Struct registration         | Registration tests           |
| 4     | Named types in grammar      | Struct types in signatures   |
| 5     | Constructor functions       | Constructor type-check tests |

## What's Next

After completing 008a, you can:
- Define structs with `struct Point { x: int, y: int }`
- Use struct types in function signatures
- Type-check constructor calls like `Point(1, 2)`

But you cannot yet:
- Actually execute code that uses struct constructors (IR lowering incomplete)
- Access fields (`p.x`)
- Assign to fields (`p.x = 10`)
- Use struct properties

Continue to [008b-struct-operations.md](008b-struct-operations.md) for field access, assignment, and function parameters.
