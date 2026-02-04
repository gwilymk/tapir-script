use tapir_script::TapirScript;

pub enum SomePropertiesEvent {
    Foo(i32),
}

#[derive(TapirScript, Debug)]
#[tapir("examples/basic_properties.tapir", event_type = SomePropertiesEvent)]
struct SomeProperties {
    int_prop: i32,
}

fn main() {
    let mut script = SomeProperties { int_prop: 5 }.script();

    script.send_event(SomePropertiesEvent::Foo(120894));

    while script.will_calling_run_do_anything() {
        println!("{:?}", script.properties);

        script.run();
    }
}
