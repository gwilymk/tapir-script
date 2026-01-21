use tapir_script::TapirScript;

#[derive(TapirScript, Debug)]
#[tapir("examples/basic_properties.tapir")]
struct SomeProperties {
    int_prop: i32,
}

fn main() {
    let mut script = SomeProperties { int_prop: 5 }.script();

    script.on_foo(120894);

    while script.will_calling_run_do_anything() {
        println!("{:?}", script.properties);

        script.run();
    }
}
