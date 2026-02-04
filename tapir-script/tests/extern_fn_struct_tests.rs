use tapir_script::{ConvertBetweenTapir, TapirScript};

/// A 2D point for testing extern functions with struct parameters.
#[derive(Clone, Debug, PartialEq, ConvertBetweenTapir)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(TapirScript)]
#[tapir("tests/extern_fn_struct.tapir")]
struct ExternFnStructTest {
    result: Point,
    distance_result: i32,
}

impl ExternFnStructTest {
    fn add_points(&mut self, a: Point, b: Point) -> Point {
        Point {
            x: a.x + b.x,
            y: a.y + b.y,
        }
    }

    fn scale_point(&mut self, p: Point, scale: i32) -> Point {
        Point {
            x: p.x * scale,
            y: p.y * scale,
        }
    }

    fn get_distance(&mut self, p: Point) -> i32 {
        // Simple Manhattan distance from origin
        p.x.abs() + p.y.abs()
    }
}

#[test]
fn test_extern_fn_add_points() {
    let mut script = ExternFnStructTest {
        result: Point { x: 0, y: 0 },
        distance_result: 0,
    }
    .script();

    script.run();

    // p1 = (10, 20), p2 = (5, 15), sum = (15, 35)
    assert_eq!(script.properties.result.x, 15);
    assert_eq!(script.properties.result.y, 35);
}

#[test]
fn test_extern_fn_scale_point() {
    let mut script = ExternFnStructTest {
        result: Point { x: 0, y: 0 },
        distance_result: 0,
    }
    .script();

    script.run();

    // scaled = (10, 20) * 2 = (20, 40)
    // distance = 20 + 40 = 60
    assert_eq!(script.properties.distance_result, 60);
}
