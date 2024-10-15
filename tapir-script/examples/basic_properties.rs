// use tapir_script::TapirScript;

use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("examples/basic_properties.tapir")]
struct SomeProperties {
    int_prop: i32,
}

trait SomePropertiesEvents {
    fn on_blah(&mut self, a: i32);
}

impl SomePropertiesEvents for vm::Script<SomeProperties> {
    fn on_blah(&mut self, a: i32) {
        unsafe { self.__private_trigger_event(vec![a], 0) }
    }
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

    type EventType = ();

    fn create_event(&self, index: u8, stack: &mut Vec<i32>) -> Self::EventType {}
}

mod foo {
    use vm::Script;

    use crate::{SomeProperties, SomePropertiesEvents};

    fn blah(t: &mut Script<SomeProperties>) {
        t.on_blah(1);
    }
}

fn main() {}
