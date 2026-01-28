use tapir_script::TapirScript;

/// A simple 2D vector for testing struct properties.
/// Implements From/Into for (i32, i32) tuple conversion.
#[derive(Clone, Debug, PartialEq)]
struct Vector2D {
    x: i32,
    y: i32,
}

impl From<(i32, i32)> for Vector2D {
    fn from((x, y): (i32, i32)) -> Self {
        Vector2D { x, y }
    }
}

impl From<Vector2D> for (i32, i32) {
    fn from(v: Vector2D) -> (i32, i32) {
        (v.x, v.y)
    }
}

#[derive(TapirScript)]
#[tapir("tests/struct_property.tapir")]
struct StructPropertyTest {
    pos: Vector2D,
}

#[test]
fn test_struct_property_basic() {
    let mut script = StructPropertyTest {
        pos: Vector2D { x: 0, y: 0 },
    }
    .script();

    script.run();

    // Script sets pos to (10, 20) then doubles to (20, 40)
    assert_eq!(script.properties.pos.x, 20);
    assert_eq!(script.properties.pos.y, 40);
}

#[test]
fn test_struct_property_initial_values() {
    let mut script = StructPropertyTest {
        pos: Vector2D { x: 5, y: 10 },
    }
    .script();

    // The script always sets pos.x = 10, pos.y = 20 first,
    // so initial values are overwritten
    script.run();

    assert_eq!(script.properties.pos.x, 20);
    assert_eq!(script.properties.pos.y, 40);
}

/// A rectangle composed of two Vector2D points.
/// Implements From/Into for flat tuples: (i32, i32, i32, i32)
/// Field order: origin.x, origin.y, size.x, size.y
#[derive(Clone, Debug, PartialEq)]
struct Rectangle {
    origin: Vector2D,
    size: Vector2D,
}

impl From<(i32, i32, i32, i32)> for Rectangle {
    fn from((ox, oy, sx, sy): (i32, i32, i32, i32)) -> Self {
        Rectangle {
            origin: Vector2D { x: ox, y: oy },
            size: Vector2D { x: sx, y: sy },
        }
    }
}

impl From<Rectangle> for (i32, i32, i32, i32) {
    fn from(r: Rectangle) -> (i32, i32, i32, i32) {
        (r.origin.x, r.origin.y, r.size.x, r.size.y)
    }
}

#[derive(TapirScript)]
#[tapir("tests/nested_struct_property.tapir")]
struct NestedStructPropertyTest {
    bounds: Rectangle,
}

#[test]
fn test_nested_struct_property() {
    let mut script = NestedStructPropertyTest {
        bounds: Rectangle {
            origin: Vector2D { x: 0, y: 0 },
            size: Vector2D { x: 0, y: 0 },
        },
    }
    .script();

    script.run();

    assert_eq!(script.properties.bounds.origin.x, 10);
    assert_eq!(script.properties.bounds.origin.y, 20);
    assert_eq!(script.properties.bounds.size.x, 100);
    assert_eq!(script.properties.bounds.size.y, 200);
}
