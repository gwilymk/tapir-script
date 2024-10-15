// use tapir_script::TapirScript;

use tapir_script::TapirScript;

#[derive(TapirScript, Debug)]
#[tapir("examples/basic_properties.tapir")]
struct SomeProperties {
    #[tapir(int)]
    int_prop: i32,
}

// trait SomePropertiesEvents {
//     fn on_blah(&mut self, a: i32);
// }

// impl SomePropertiesEvents for vm::Script<SomeProperties> {
//     fn on_blah(&mut self, a: i32) {
//         unsafe { self.__private_trigger_event(vec![a], 0) }
//     }
// }

fn main() {
    let mut script = SomeProperties { int_prop: 5 }.script();

    while script.will_calling_run_do_anything() {
        script.run();
        println!("{:?}", script.properties);
    }
}
