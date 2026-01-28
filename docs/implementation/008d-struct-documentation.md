# Struct Documentation (Part 4 of 4)

## Overview

Update the language reference documentation to include structs.

**Prerequisites**:
- [008a-struct-types.md](008a-struct-types.md)
- [008b-struct-operations.md](008b-struct-operations.md)
- [008c-struct-rust-integration.md](008c-struct-rust-integration.md)

## Implementation

**Files**: `docs/tapir-reference.md`

Add a new section documenting structs:

### Structs

```tapir
struct Point { x: int, y: int }
struct Rect { origin: Point, size: Point }
```

Structs are composite types with named fields. They are declared at the top level and create an implicit constructor function.

**Construction:**

```tapir
var p = Point(1, 2);
var r = Rect(Point(0, 0), Point(10, 20));
```

**Field access:**

```tapir
var x = p.x;
p.y = 10;
r.origin.x = 5;
```

**Assignment:**

```tapir
var q = p;  // copies all fields
```

**Function parameters and returns:**

```tapir
fn add_points(a: Point, b: Point) -> Point {
    return Point(a.x + b.x, a.y + b.y);
}
```

**Properties:**

```tapir
property pos: Point;

fn update() {
    pos.x = pos.x + 1;
}
```

In Rust, struct properties must implement `From<(T, T, ...)>` and `Into<(T, T, ...)>`:

```rust
#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    pos: Vector2D<i32>,  // Vector2D implements From/Into for (i32, i32)
}
```

### Type System Section

Update the type system section to include `struct` types alongside `int`, `fix`, and `bool`.

### Restrictions

- Structs can only be declared at top level
- Struct names cannot shadow builtin types (`int`, `fix`, `bool`)
- Struct fields must have explicit types
- No recursive structs (a struct cannot contain itself)

---

## Tests

- Verify examples in documentation compile and run correctly
- Consider adding the documentation examples as integration tests

## Compilable

Yes - documentation only, no code changes.
