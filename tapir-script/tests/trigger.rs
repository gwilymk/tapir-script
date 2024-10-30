#![deny(unfulfilled_lint_expectations)]

use agb_fixnum::num;
use tapir_script::{Fix, TapirScript};

#[derive(TapirScript)]
#[tapir("tests/trigger.tapir", trigger_type = MyEventType)]
struct Triggers;

#[derive(PartialEq, Eq, Debug)]
enum MyEventType {
    IntKind(i32),
    FixKind(Fix),
    BoolKind(bool),
    TwoArguments(i32, i32),
    EmptyKind,

    #[expect(dead_code, reason = "This isn't referenced in the trigger.tapir code")]
    UnusedEventType,
}

#[test]
fn triggers() {
    use MyEventType::*;

    let expected_order: &[&[MyEventType]] = &[
        &[IntKind(4)],
        &[FixKind(num!(3.5))],
        &[BoolKind(true)],
        &[EmptyKind],
        &[
            IntKind(0),
            IntKind(1),
            IntKind(2),
            IntKind(3),
            IntKind(4),
            IntKind(5),
        ],
        &[TwoArguments(5, 7)],
    ];

    let mut script = Triggers.script();

    for (i, expected) in expected_order.iter().enumerate() {
        assert_eq!(&script.run(), expected, "Failed at {i}");
    }

    assert!(!script.will_calling_run_do_anything());
}
