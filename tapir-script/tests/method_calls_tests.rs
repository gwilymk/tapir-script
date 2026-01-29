use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/method_calls.tapir")]
struct MethodCallsTest {
    result: i32,
}

#[test]
fn test_chained_method_calls() {
    let mut script = MethodCallsTest { result: 0 }.script();
    script.run();

    // 5.double() = 10, 10.add(3) = 13, 13.double() = 26
    assert_eq!(script.properties.result, 26);
}
