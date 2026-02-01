# How Structs Are Compiled

This document explains the internal implementation of struct compilation in the tapir-script compiler. It's intended for developers working on the compiler itself.

## Overview

Structs are a **frontend-only abstraction**. By the time code reaches the VM, all structs have been "scalarized" - flattened into individual scalar values. The VM has no concept of structs; it only sees `int`, `fix`, and `bool` values.

This design has significant implications:

- No bytecode changes were needed to support structs
- No VM changes were needed
- Existing optimizations apply to each scalar field
- Register allocation works naturally

The cost is complexity in the compiler, which must track how struct fields map to their underlying storage.

## The Three Storage Classes

Struct values can live in three different storage classes, each with different access patterns:

| Storage Class | Declaration            | Access Instructions     | Example                  |
| ------------- | ---------------------- | ----------------------- | ------------------------ |
| **Local**     | `var p: Point;`        | `Move`                  | Function-local variables |
| **Property**  | `property pos: Point;` | `GetProp`/`StoreProp`   | Rust-visible state       |
| **Global**    | `global g: Point;`     | `GetGlobal`/`SetGlobal` | Script-wide state        |

Each storage class uses different bytecode instructions and requires different handling during IR lowering.

## Core Data Structures

### StructLayout

`StructLayout` is the unified representation of how a struct's fields are stored:

```rust
pub struct StructLayout {
    pub struct_id: StructId,      // Which struct type this is
    pub fields: Vec<FieldAccessor>, // One entry per field in declaration order
}
```

### FieldAccessor

`FieldAccessor` encodes how to access a single field:

```rust
pub enum FieldAccessor {
    Local(SymbolId),                           // Use Move instruction
    Property(usize),                           // Use GetProp/StoreProp with this index
    Global(GlobalId),                          // Use GetGlobal/SetGlobal with this index
    Nested(Option<SymbolId>, Box<StructLayout>), // Recurse into nested struct
}
```

The `Nested` variant contains another `StructLayout` for struct-typed fields. The optional `SymbolId` is present only for local storage (needed for field symbol lookup).

### Example: How a Struct is Represented

Given this tapir-script code:

```tapir
struct Point { x: int, y: int }
struct Rect { origin: Point, size: Point }

var r: Rect;        # Local
property bounds: Rect;  # Property
global g: Rect;     # Global
```

The `Rect` struct layout for each storage class:

**Local (`var r: Rect`):**

```
StructLayout {
    struct_id: Rect,
    fields: [
        Nested(Some(r.origin), StructLayout {
            struct_id: Point,
            fields: [Local(r.origin.x), Local(r.origin.y)]
        }),
        Nested(Some(r.size), StructLayout {
            struct_id: Point,
            fields: [Local(r.size.x), Local(r.size.y)]
        })
    ]
}
```

**Property (`property bounds: Rect`):**

```
StructLayout {
    struct_id: Rect,
    fields: [
        Nested(None, StructLayout {
            struct_id: Point,
            fields: [Property(0), Property(1)]  # bounds.origin.x, bounds.origin.y
        }),
        Nested(None, StructLayout {
            struct_id: Point,
            fields: [Property(2), Property(3)]  # bounds.size.x, bounds.size.y
        })
    ]
}
```

**Global (`global g: Rect`):**

```
StructLayout {
    struct_id: Rect,
    fields: [
        Nested(None, StructLayout {
            struct_id: Point,
            fields: [Global(0), Global(1)]  # g.origin.x, g.origin.y
        }),
        Nested(None, StructLayout {
            struct_id: Point,
            fields: [Global(2), Global(3)]  # g.size.x, g.size.y
        })
    ]
}
```

## Compilation Pipeline

### Phase 1: Struct Registration (`struct_visitor.rs`)

Before symbol resolution, all struct declarations are processed:

1. **First pass**: Register all struct names, allocating `StructId`s
2. **Second pass**: Resolve field types (now that all struct names are known)

This two-pass approach allows structs to reference each other:

```tapir
struct Node { value: int, next: NodePtr }  # NodePtr declared later
struct NodePtr { node: Node }              # References Node
```

### Phase 2: Symbol Resolution (`symtab_visitor.rs`)

During symbol resolution, struct-typed variables are expanded:

**For local variables (`var p: Point`):**

- Creates symbols for each scalar field: `p.x`, `p.y`
- Builds a `StructLayout` with `Local` accessors
- Layout is stored in `symtab.struct_layouts`

**For properties (`property pos: Point`):**

- Creates individual property entries: `pos.x`, `pos.y`
- Creates a "property base" symbol for `pos` (allows `pos.x = 10` syntax)
- Builds a `StructLayout` with `Property` accessors
- Registered via `register_struct_property_base()` which atomically updates:
  - `property_base_struct_ids` - maps symbol to struct type
  - `property_base_symbols` - maps name to symbol
  - `struct_layouts` - stores the layout
  - `named_struct_layouts` - maps name to symbol for layout lookup

**For globals (`global g: Point`):**

- Creates global entries for each scalar field
- Builds a `StructLayout` with `Global` accessors

### Phase 3: Type Checking (`type_visitor.rs`)

The type visitor:

- Validates field access (`p.x` - does `x` exist on the type of `p`?)
- Records `FieldAccessInfo` metadata on each field access expression
- Records `FieldAssignmentInfo` metadata on assignments to field paths
- Pre-populates the type table with property base types during construction

Key type checking metadata:

```rust
// Stored on FieldAccess expressions
pub struct FieldAccessInfo {
    pub base_struct_id: StructId,
    pub field_index: usize,
}

// Stored on Assignment statements with field targets
pub struct FieldAssignmentInfo(pub Vec<Option<(StructId, Vec<usize>)>>);
```

### Phase 4: IR Lowering (`ir/lowering.rs`)

IR lowering translates AST to TapIR, handling struct operations:

#### Field Access

When lowering `p.x` where `p` is a struct:

1. Retrieve `FieldAccessInfo` from expression metadata
2. Look up the layout for the base (`p`)
3. Navigate to the field using the index
4. Emit appropriate instruction based on accessor type:
   - `Local(sym)` → `Move { target, source: sym }`
   - `Property(idx)` → `GetProp { target, prop_index: idx }`
   - `Global(id)` → `GetGlobal { target, global_index: id.0 }`
   - `Nested(...)` → Copy all leaf fields

#### Field Assignment

When lowering `p.x = 10`:

1. Retrieve `FieldAssignmentInfo` from statement metadata
2. Look up the layout for the base
3. Navigate to the target field
4. Emit appropriate instruction:
   - `Local(sym)` → `Move { target: sym, source: value }`
   - `Property(idx)` → `StoreProp { prop_index: idx, value }`
   - `Global(id)` → `SetGlobal { global_index: id.0, value }`
   - `Nested(...)` → Copy all leaf fields

#### Struct Copying

When assigning one struct to another (`p = q`), all leaf fields must be copied. The `copy_struct_layout()` function handles this:

```rust
fn copy_struct_layout(&mut self, src: &StructLayout, dst: &StructLayout, symtab: &mut SymTab) {
    for (src_field, dst_field) in src.fields.iter().zip(&dst.fields) {
        self.copy_field_accessor(src_field, dst_field, symtab);
    }
}
```

This works for any combination of storage classes (Local→Property, Global→Local, etc.).

#### Constructor Calls

When lowering `Point(1, 2)`:

1. Create a temporary layout for the result
2. Evaluate each argument
3. Copy argument values to corresponding fields in the temp
4. The temp's leaf symbols become the "value" of the constructor

## Property Bases

Property bases are a special concept for struct-typed properties. Given:

```tapir
property pos: Point;
pos.x = 10;
```

The name `pos` needs to be resolvable as a variable, even though individual properties are `pos.x` and `pos.y`. A "property base" symbol is created for `pos` that:

1. Has type `Struct(Point)`
2. Is tracked in `property_base_struct_ids` and `property_base_symbols`
3. Has an associated `StructLayout` for field navigation
4. Is pre-populated in the type table during `TypeVisitor::new()`

When `pos.x` is encountered:

1. `pos` resolves to the property base symbol
2. Type checking sees it as `Struct(Point)` and validates `.x`
3. IR lowering looks up the property layout and navigates to field 0
4. Emits `StoreProp { prop_index: 0, value }` for the assignment

## Key Invariants

1. **Storage class consistency**: A `StructLayout` for properties contains only `Property` and `Nested` accessors (never `Local` or `Global`). Similarly for other storage classes.

2. **Field order**: Fields in `StructLayout.fields` are always in declaration order, matching the struct definition.

3. **Leaf traversal order**: When collecting leaf accessors, traversal is depth-first in field declaration order. This order must be consistent everywhere (arguments, returns, copying).

4. **Atomic registration**: Property bases must be registered via `register_struct_property_base()` which updates all four related data structures atomically.

## Debugging Tips

### Finding a struct's layout

```rust
// All lookups use symbol IDs
symtab.get_struct_layout(symbol_id)

// For globals, get the symbol ID from the global info
let symbol_id = symtab.get_global_by_name("pos")?.id.to_symbol_id();
symtab.get_struct_layout(symbol_id)

// For property bases, get the symbol ID from the property base
let symbol_id = symtab.get_property_base_symbol("pos")?;
symtab.get_struct_layout(symbol_id)
```

### Checking if a symbol is a property base

```rust
symtab.is_property_base(symbol_id)
// or
symtab.get_property_base_struct_id(symbol_id).is_some()
```

### Tracing field access

Field access lowering follows this path:

1. `blocks_for_expression()` matches `FieldAccess { base, field }`
2. Retrieves `FieldAccessInfo` from `expr.meta`
3. Checks if base is a property base → `get_struct_layout(base_symbol)`
4. Checks if base is a global → `get_struct_layout(base_symbol)`
5. Otherwise → evaluates base, `ensure_local_layout(target, struct_id)`

All layout lookups use symbol IDs, never strings.

### Common Issues

**"Unknown type for variable 'pos'"**

- Property base wasn't registered in type table
- Check that `register_struct_property_base()` was called
- Verify `TypeVisitor::new()` pre-populates property base types

**Wrong field being accessed**

- Check field indices in `FieldAccessInfo`
- Verify layout field order matches struct definition
- Ensure `navigate()` is using correct path

**Struct copy missing fields**

- Check that `copy_struct_layout()` iterates all fields
- Verify both layouts have same `struct_id`
- Check for nested struct handling

## Files Reference

| File                | Responsibility                                    |
| ------------------- | ------------------------------------------------- |
| `types.rs`          | `StructId`, `StructDef`, `StructRegistry`         |
| `struct_visitor.rs` | Struct registration and field resolution          |
| `symtab_visitor.rs` | `StructLayout`, `FieldAccessor`, symbol expansion |
| `type_visitor.rs`   | Field access validation, `FieldAccessInfo`        |
| `ir/lowering.rs`    | Struct IR lowering, `copy_struct_layout()`        |
