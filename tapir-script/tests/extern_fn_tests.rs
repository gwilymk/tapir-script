use agb_fixnum::num;
use tapir_script::{Fix, TapirScript};

#[derive(TapirScript)]
#[tapir("tests/extern_fn.tapir")]
struct ExternFnTest {
    result: i32,
    fix_result: Fix,
    constant: i32,
    threshold_passed: bool,

    // Track side effect calls (not declared in .tapir, so not accessible from script)
    side_effect_called_with: Option<i32>,
}

impl ExternFnTest {
    fn add_numbers(&mut self, a: i32, b: i32) -> i32 {
        dbg!(a, b);
        a + b
    }

    fn multiply_fix(&mut self, a: Fix, b: Fix) -> Fix {
        a * b
    }

    fn side_effect(&mut self, value: i32) {
        self.side_effect_called_with = Some(value);
    }

    fn get_constant(&mut self) -> i32 {
        42
    }

    fn check_threshold(&mut self, value: i32) -> bool {
        value > 5
    }
}

#[test]
fn test_extern_fn_add_numbers() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    assert_eq!(script.properties.result, 8); // 3 + 5
}

#[test]
fn test_extern_fn_multiply_fix() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    assert_eq!(script.properties.fix_result, num!(10.0)); // 2.5 * 4.0
}

#[test]
fn test_extern_fn_side_effect() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    assert_eq!(script.properties.side_effect_called_with, Some(42));
}

#[test]
fn test_extern_fn_get_constant() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    assert_eq!(script.properties.constant, 42);
}

#[test]
fn test_extern_fn_check_threshold() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    // result is 8, which is > 5, so threshold_passed should be true
    assert!(script.properties.threshold_passed);
}

#[test]
fn test_extern_fn_all_together() {
    let mut script = ExternFnTest {
        result: 0,
        fix_result: num!(0.0),
        constant: 0,
        threshold_passed: false,
        side_effect_called_with: None,
    }
    .script();

    script.run();

    assert_eq!(script.properties.result, 8);
    assert_eq!(script.properties.fix_result, num!(10.0));
    assert_eq!(script.properties.constant, 42);
    assert!(script.properties.threshold_passed);
    assert_eq!(script.properties.side_effect_called_with, Some(42));
}
