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

    let mut i = 0;
    while script.will_calling_run_do_anything() {
        assert_eq!(script.run(), expected_order[i], "Failed at {i}");
        i += 1;
    }
}
