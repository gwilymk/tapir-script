use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("tests/basic_event.tapir")]
struct Events {
    int_prop: i32,
}

#[test]
fn events() {
    let mut events = Events { int_prop: 0 }.script();

    events.on_loopy(20);

    for i in 0..20 {
        events.run();
        assert_eq!(events.properties.int_prop, i);
    }

    events.run();
    assert!(!events.will_calling_run_do_anything());
}
