# Struct Rust Integration (Part 3 of 3)

## Overview

This document covers integrating structs with the Rust host: struct properties, macro updates for `From`/`Into` tuple conversion, and error message polish. After completing this part, structs are fully integrated with Rust.

**Prerequisites**:
- [008a-struct-types.md](008a-struct-types.md) must be completed first
- [008b-struct-operations.md](008b-struct-operations.md) must be completed first

## Property Declarations

```tapir
struct Point { x: int, y: int }
property position: Point;
```

In the Rust struct, struct properties use tuple conversion via `From` and `Into` traits:

```rust
use agb_fixnum::Vector2D;

#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    position: Vector2D<i32>,  // must impl From<(i32, i32)> and Into<(i32, i32)>
}
```

The macro generates code like:

- Getting: `let (x, y) = self.position.into();`
- Setting: `self.position = Vector2D::from((x, y));`

Field order in the tuple matches declaration order in the tapir struct.

## Implementation Plan

Each phase leaves the project in a compilable, testable state.

### Phase 12: Struct Properties

**Files**: `compiler/src/compile/symtab_visitor.rs`, `compiler/src/compile/ir/lowering.rs`

Struct properties expand to consecutive property indices:

```rust
// property pos: Point; becomes indices 0 (pos.x) and 1 (pos.y)

fn expand_property_fields(
    path: &str,
    struct_id: StructId,
    current_index: &mut usize,
    registry: &StructRegistry,
    properties: &mut Vec<Property>,
) {
    let def = registry.get(struct_id);

    for field in &def.fields {
        let field_path = format!("{}.{}", path, field.name);

        match field.ty {
            Type::Struct(nested_id) => {
                // Recurse for nested structs
                expand_property_fields(
                    &field_path,
                    nested_id,
                    current_index,
                    registry,
                    properties,
                );
            }
            scalar_type => {
                properties.push(Property {
                    ty: scalar_type,
                    index: *current_index,
                    name: field_path,
                    span: field.span,
                });
                *current_index += 1;
            }
        }
    }
}
```

**IR lowering** for GetProp/SetProp with struct fields uses the expanded indices:

```rust
// Reading a struct property field:
// pos.x -> GetProp with index 0
// pos.y -> GetProp with index 1

// The property lookup finds the expanded property by path
let prop = symtab.get_property_by_path("pos.x");
self.current_block.push(TapIr::GetProp {
    target: target_symbol,
    prop_index: prop.index,
});
```

**Tests**:

- Property with struct type expands correctly to consecutive indices
- Can read struct property fields: `var x = pos.x;`
- Can write struct property fields: `pos.x = 10;`
- Nested struct properties work
- IR snapshot tests
- Existing tests still pass

**Compilable**: Yes - struct properties work within tapir-script

---

### Phase 13: Macro Updates

**Files**: `tapir-script-macros-core/src/lib.rs`

Generate `From`/`Into` tuple conversions for struct properties.

The macro needs to:
1. Detect which properties are struct types (from the compiled script metadata)
2. Generate tuple conversion code for those properties

**Getting struct property fields**:

```rust
// For property `pos: Point` with fields x, y at indices 0, 1
// Generate getter arms:
0 => {
    let (x, _): (i32, i32) = self.position.clone().into();
    x
}
1 => {
    let (_, y): (i32, i32) = self.position.clone().into();
    y
}
```

**Setting struct property fields**:

Setting is more complex because we need to reconstruct the full tuple. The simplest approach is to get the current value, modify one field, and set it back:

```rust
// For setting index 0 (pos.x):
0 => {
    let (_, y): (i32, i32) = self.position.clone().into();
    self.position = From::from((value, y));
}
// For setting index 1 (pos.y):
1 => {
    let (x, _): (i32, i32) = self.position.clone().into();
    self.position = From::from((x, value));
}
```

**Type information**: The macro needs to know:
- Which properties are structs
- How many fields each struct has
- The scalar type of each field

This information should be included in the compiled script metadata that the macro reads.

**Tests**:

- Integration test with Rust struct using `Vector2D<i32>`
- Test get/set roundtrip for struct properties
- Test nested struct properties if supported
- Existing macro tests still pass

**Compilable**: Yes - full Rust integration

---

### Phase 14: Error Messages & Polish

**Files**: `compiler/src/reporting.rs`

Add polished error messages for all struct-related errors:

```rust
pub enum ErrorKind {
    // From 008a
    DuplicateStructName { name: String },
    StructShadowsBuiltinType { name: String },  // e.g., `struct int { ... }`
    UnknownType { name: String },
    DuplicateStructField { struct_name: String, field_name: String },

    // From 008b
    FieldAccessOnNonStruct { ty: Type },
    UnknownField { ty: Type, field: String },

    // Additional
    RecursiveStruct { name: String },  // struct Foo { f: Foo }
}
```

**Error message examples**:

```
error: struct name `int` shadows builtin type
  --> script.tapir:1:8
   |
 1 | struct int { x: int }
   |        ^^^ cannot use builtin type name

error: unknown field `z` on type `Point`
  --> script.tapir:5:13
   |
 5 |     var x = p.z;
   |             ^^^ Point has fields: x, y

error: field access on non-struct type `int`
  --> script.tapir:3:13
   |
 3 |     var y = x.foo;
   |             ^^^^^ int is not a struct
```

**Tests**:

- Error message snapshot tests for each error kind
- Verify error spans point to the right location
- Verify helpful suggestions are included (e.g., listing available fields)

**Compilable**: Yes - complete feature

---

## Implementation Order Summary

| Phase | What's Added        | Can Test            |
| ----- | ------------------- | ------------------- |
| 12    | Struct properties   | Property tests      |
| 13    | Macro updates       | Integration tests   |
| 14    | Error messages      | Error snapshot tests|

## Complete Feature Summary

After completing all three parts (008a, 008b, 008c), structs are fully implemented:

- **Define**: `struct Point { x: int, y: int }`
- **Construct**: `var p = Point(1, 2);`
- **Access**: `var x = p.x;`
- **Assign**: `p.x = 10;`
- **Copy**: `var q = p;`
- **Functions**: `fn foo(p: Point) -> Point { ... }`
- **Properties**: `property pos: Point;` with Rust `Vector2D<i32>`
- **Nested**: `struct Rect { origin: Point, size: Point }`

## Future Extensions

### Builtin Structs

For `int2` and `fix2`, add a `builtin struct` syntax:

```tapir
builtin struct int2 { x: int, y: int }
builtin struct fix2 { x: fix, y: fix }
```

Builtin structs:

- Are defined in the prelude
- Get special constructor syntax: `[1, 2]` creates `int2`, `[1.0, 2.0]` creates `fix2`
- May have special compiler treatment for optimization

Implementation: Add `is_builtin: bool` to `StructDef`, add `[expr, expr]` syntax that creates builtin vector types based on component type inference.

### Local Structs

Allow struct declarations inside functions:

```tapir
fn example() {
    struct Local { a: int, b: int }
    var x = Local(1, 2);
}
```

These would be hoisted to top-level with a generated unique name (like `__example_Local_0`), inaccessible from outside the function.

Note: Local functions (non-lambda) would use the same hoisting mechanism.

### Struct Methods

Allow methods on structs using type-prefixed syntax:

```tapir
struct Point { x: int, y: int }

fn Point.length_squared(self) -> int {
    return self.x * self.x + self.y * self.y;
}

fn Point.add(self, other: Point) -> Point {
    return Point(self.x + other.x, self.y + other.y);
}

// Called as:
var p = Point(3, 4);
var len_sq = p.length_squared();
var sum = p.add(Point(1, 1));
```

The `self` parameter is explicit and must be the first parameter. The type prefix (`Point.`) associates the function with the struct without requiring methods inside the struct body or impl blocks.

### Operator Overloading for Structs

With operator overloading (separate feature), structs could define `+`, `-`, etc.:

```tapir
struct Point { x: int, y: int }

operator +(a: Point, b: Point) -> Point {
    return Point(a.x + b.x, a.y + b.y);
}
```
