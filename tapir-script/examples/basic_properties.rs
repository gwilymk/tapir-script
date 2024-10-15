// use tapir_script::TapirScript;

use vm::TapirScript;

// #[derive(TapirScript)]
// #[tapir("examples/basic_properties.tapir")]
struct SomeProperties {
    int_prop: i32,
}

impl TapirScript for SomeProperties {
    fn script(self) -> vm::Script<Self> {
        vm::Script::new(self, &[])
    }

    fn set_prop(&mut self, _index: u8, value: i32) {
        self.int_prop = value;
    }

    fn get_prop(&self, _index: u8) -> i32 {
        self.int_prop
    }
}

fn main() {}
