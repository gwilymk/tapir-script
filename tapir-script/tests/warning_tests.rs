use tapir_script::TapirScript;

// This test is designed to trigger a compile-time warning for division by zero.
// The warning should be visible during compilation.
#[derive(TapirScript)]
#[tapir("tests/warning_test.tapir")]
struct WarningTest {
    result: i32,
}

#[test]
fn test_warning_script_compiles() {
    // This just verifies the script compiles (with warnings).
    // The actual warning is shown during macro expansion at compile time.
    let script = WarningTest { result: 0 }.script();
    drop(script);
}
