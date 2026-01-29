use tapir_script::{ConvertBetweenTapir, TapirScript};

/// A simple 2D vector for testing struct properties.
#[derive(Clone, Debug, PartialEq, ConvertBetweenTapir)]
struct Vector2D {
    x: i32,
    y: i32,
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
/// ConvertBetweenTapir recursively converts nested structs.
#[derive(Clone, Debug, PartialEq, ConvertBetweenTapir)]
struct Rectangle {
    origin: Vector2D,
    size: Vector2D,
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
