use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/wait_n.tapir")]
struct WaitNTest {
    result: i32,
}

#[test]
fn wait_n_tests() {
    let mut script = WaitNTest { result: 999 }.script();

    // Test wait 0 is no-op (frame 0)
    script.run();
    assert_eq!(
        script.properties.result, 0,
        "wait 0 should not advance frames"
    );

    // Test wait 1 same as wait (needs 1 extra frame)
    script.run(); // hits wait 1
    script.run(); // completes wait 1
    assert_eq!(script.properties.result, 1, "wait 1 should advance 1 frame");

    // Test wait 5 (needs 5 extra frames)
    for _ in 0..5 {
        script.run();
    }
    script.run(); // completes wait 5
    assert_eq!(
        script.properties.result, 5,
        "wait 5 should advance 5 frames"
    );

    // Test wait with variable n=3 (needs 3 extra frames)
    for _ in 0..3 {
        script.run();
    }
    script.run(); // completes wait n
    assert_eq!(
        script.properties.result, 3,
        "wait n (n=3) should advance 3 frames"
    );

    // Test wait with expression n+3 where n=2 (needs 5 extra frames)
    for _ in 0..5 {
        script.run();
    }
    script.run(); // completes wait n+3
    assert_eq!(
        script.properties.result, 5,
        "wait n+3 (n=2) should advance 5 frames"
    );

    // Test negative wait is no-op
    script.run();
    assert_eq!(
        script.properties.result, 0,
        "wait with negative value should not advance frames"
    );
}
