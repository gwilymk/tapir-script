use tapir_script::TapirScript;

pub enum EventsEvent {
    Loopy(i32),
}

#[derive(TapirScript)]
#[tapir("tests/basic_event.tapir", event_type = EventsEvent)]
struct Events {
    int_prop: i32,
}

#[test]
fn events() {
    let mut events = Events { int_prop: 0 }.script();

    events.send_event(EventsEvent::Loopy(20));

    for i in 0..20 {
        events.run();
        assert_eq!(events.properties.int_prop, i);
    }

    events.run();
    assert!(!events.will_calling_run_do_anything());
}
