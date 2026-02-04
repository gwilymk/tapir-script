use tapir_script::{ConvertBetweenTapir, TapirScript};

/// A 2D point for testing triggers with struct arguments.
#[derive(Clone, Debug, PartialEq, Eq, ConvertBetweenTapir)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(TapirScript)]
#[tapir("tests/trigger_struct.tapir", trigger_type = PointEvent)]
struct TriggerStructTest;

#[derive(PartialEq, Eq, Debug)]
enum PointEvent {
    PointMoved(Point),
    PointScaled(Point, i32),
    PointsAdded(Point, Point),
}

#[test]
fn test_trigger_with_struct() {
    use PointEvent::*;

    let mut script = TriggerStructTest.script();

    // First trigger: PointMoved(Point(10, 20))
    let events = script.run();
    assert_eq!(events, vec![PointMoved(Point { x: 10, y: 20 })]);

    // Second trigger: PointMoved(Point(5, 15))
    let events = script.run();
    assert_eq!(events, vec![PointMoved(Point { x: 5, y: 15 })]);

    // Third trigger: PointScaled(Point(10, 20), 3)
    let events = script.run();
    assert_eq!(events, vec![PointScaled(Point { x: 10, y: 20 }, 3)]);

    // Fourth trigger: PointsAdded(Point(10, 20), Point(1, 2))
    let events = script.run();
    assert_eq!(
        events,
        vec![PointsAdded(Point { x: 10, y: 20 }, Point { x: 1, y: 2 })]
    );

    assert!(!script.will_calling_run_do_anything());
}
