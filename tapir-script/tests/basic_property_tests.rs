use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/factorial.tapir")]
struct FactorialCalculation {
    io: i32,
}

#[derive(TapirScript)]
#[tapir("tests/empty.tapir")]
#[allow(dead_code)]
struct NoProperties;

#[derive(TapirScript)]
#[tapir("tests/empty.tapir")]
#[allow(dead_code)]
struct NumberProperties(i32);

#[derive(TapirScript)]
#[tapir("tests/booleans.tapir")]
struct ManyProperties {
    input1: i32,
    input2: i32,
    output: bool,
}

#[test]
fn test_factorial() {
    let mut script = FactorialCalculation { io: 10 }.script();
    script.run();

    assert_eq!(script.properties.io, 3628800);
}

#[test]
fn test_many_properties() {
    let mut script = ManyProperties {
        input1: 4,
        input2: 4,
        output: false,
    }
    .script();
    script.run();

    assert!(script.properties.output);
}
