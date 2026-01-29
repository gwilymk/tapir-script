use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/struct_methods.tapir")]
struct StructMethodsTest {
    result: i32,
}

#[test]
fn test_struct_methods() {
    let mut script = StructMethodsTest { result: 0 }.script();
    script.run();

    // p1.scale(2) = Point(6, 8)
    // Point(6, 8).add(Point(1, 2)) = Point(7, 10)
    // Point(7, 10).sum() = 17
    assert_eq!(script.properties.result, 17);
}
