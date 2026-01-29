use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/builtin_methods.tapir")]
struct BuiltinMethodsTest {
    floor_result: i32,
    ceil_result: i32,
    round_result: i32,
}

#[test]
fn test_builtin_floor_method() {
    let mut script = BuiltinMethodsTest {
        floor_result: 0,
        ceil_result: 0,
        round_result: 0,
    }
    .script();
    script.run();

    // 3.7.floor() = 3
    assert_eq!(script.properties.floor_result, 3);
}

#[test]
fn test_builtin_ceil_method() {
    let mut script = BuiltinMethodsTest {
        floor_result: 0,
        ceil_result: 0,
        round_result: 0,
    }
    .script();
    script.run();

    // 3.2.ceil() = 4
    assert_eq!(script.properties.ceil_result, 4);
}

#[test]
fn test_builtin_round_method() {
    let mut script = BuiltinMethodsTest {
        floor_result: 0,
        ceil_result: 0,
        round_result: 0,
    }
    .script();
    script.run();

    // 3.5.round() = 4
    assert_eq!(script.properties.round_result, 4);
}
