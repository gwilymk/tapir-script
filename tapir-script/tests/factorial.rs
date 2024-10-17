use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/factorial.tapir")]
struct FactorialCalculation {
    #[tapir(int)]
    io: i32,
}

#[test]
fn test_factorial() {
    let mut script = FactorialCalculation { io: 10 }.script();
    script.run();

    assert_eq!(script.properties.io, 3628800);
}
