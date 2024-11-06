#![deny(clippy::all)]
#![no_std]
extern crate alloc;

mod state;

use alloc::{vec, vec::Vec};
use state::{ObjectSafeProperties, ObjectSafePropertiesImpl, State};

struct Vm<'a> {
    bytecode: &'a [u16],
    states: Vec<State>,
}

impl<'a> Vm<'a> {
    pub fn new(bytecode: &'a [u16]) -> Self {
        Self {
            bytecode,
            states: vec![State::new(0, vec![])],
        }
    }

    fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
        let mut state_index = 0;
        while state_index < self.states.len() {
            match self.states[state_index].run_until_wait(self.bytecode, properties) {
                state::RunResult::Waiting => {
                    state_index += 1;
                }
                state::RunResult::Finished => {
                    self.states.swap_remove(state_index);
                }
                state::RunResult::Spawn(state) => {
                    self.states.push(*state);
                    // intentionally not increasing state_index to ensure that the spawning
                    // state continues to run.
                }
            }
        }
    }
}

/// # Safety
///
/// You should never implement this directly, and instead go through the derive macro
pub unsafe trait TapirScript {
    type EventType;

    fn script(self) -> Script<Self>
    where
        Self: Sized;

    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn create_event(&self, index: u8, stack: &mut Vec<i32>) -> Self::EventType;
}

pub struct Script<T: TapirScript> {
    vm: Vm<'static>,
    pub properties: T,
}

impl<T: TapirScript> Script<T> {
    pub fn new(properties: T, bytecode: &'static [u16]) -> Self {
        Self {
            vm: Vm::new(bytecode),
            properties,
        }
    }

    pub fn run(&mut self) -> Vec<T::EventType> {
        let mut object_safe_props = ObjectSafePropertiesImpl {
            properties: &mut self.properties,
            events: vec![],
        };

        self.vm.run_until_wait(&mut object_safe_props);

        object_safe_props.events
    }

    pub fn will_calling_run_do_anything(&self) -> bool {
        !self.vm.states.is_empty()
    }

    #[doc(hidden)]
    pub unsafe fn __private_trigger_event(&mut self, mut initial_stack: Vec<i32>, pc: usize) {
        initial_stack.push(0);

        self.vm.states.push(State::new(pc, initial_stack));
    }
}

#[cfg(test)]
mod test {
    extern crate std;

    use super::*;

    use std::fs;

    use alloc::string::ToString;
    use compiler::{CompileSettings, Property, Type};
    use insta::{assert_ron_snapshot, glob};
    use serde::Serialize;

    use crate::TapirScript;

    #[test]
    fn stack_snapshot_tests() {
        glob!("snapshot_tests", "stack/**/*.tapir", |path| {
            std::println!("{}", path.display());

            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
                enable_optimisations: true,
            };

            let bytecode = compiler::compile(path.file_name().unwrap(), &input, compiler_settings)
                .unwrap()
                .bytecode;

            let mut vm = Vm::new(&bytecode);
            let mut prop_object = PropObj { int_prop: 5 };

            let mut stack_at_waits = vec![];

            let mut max_iterations = 1000;

            while !vm.states.is_empty() && max_iterations >= 0 {
                let mut object_safe_props = ObjectSafePropertiesImpl {
                    properties: &mut prop_object,
                    events: vec![],
                };

                vm.run_until_wait(&mut object_safe_props);
                stack_at_waits.push((
                    vm.states
                        .iter()
                        .map(|state| state.stack().to_vec())
                        .collect::<Vec<_>>(),
                    prop_object.clone(),
                ));

                max_iterations -= 1;
            }

            if max_iterations == 0 {
                panic!("ran for over 1000 waits, something seems to have gone wrong...");
            }

            assert_ron_snapshot!(stack_at_waits);
        });
    }

    macro_rules! binop_test {
        ($($type: ident, $name:ident: ($code:tt, $expected:expr),)*) => {
            $(
                paste::paste! {
                    #[test]
                    fn [< binop_test_ $name >]() {
                        let compile_settings = CompileSettings {
                            properties: vec![Property {
                                ty: Type::$type,
                                index: 0,
                                name: "prop".to_string(),
                            }],
                            enable_optimisations: false,
                        };

                        let bytecode = compiler::compile(
                            concat!(stringify!($name), ".tapir"),
                            concat!("prop = ", $code, ";"),
                            compile_settings
                        ).unwrap().bytecode;

                        let mut vm = Vm::new(&bytecode);
                        let mut prop_object = PropObj {
                            int_prop: if Type::$type == Type::Int { 5 } else { 1 },
                        };

                        while !vm.states.is_empty() {
                            let mut object_safe_props = ObjectSafePropertiesImpl {
                                properties: &mut prop_object,
                                events: vec![],
                            };

                            vm.run_until_wait(&mut object_safe_props);
                        }

                        assert_eq!(prop_object.int_prop, $expected);
                    }
                }
            )*
        };
    }

    binop_test!(
        Int, addition: ("prop + 1", 6),
        Int, addition2: ("1 + prop", 6),
        Int, multiplication: ("prop * 2", 10),
        Int, multiplication2: ("2 * prop", 10),
        Int, subtraction: ("prop - 1", 4),
        Int, subtraction2: ("1 - prop", -4),
        Int, division: ("prop // 3", 1),
        Int, division2: ("15 // prop", 3),
        Int, modulo: ("15 %% prop", 0),
        Int, modulo2: ("16 %% prop", 1),
        Int, modulo3: ("prop %% 2", 1),
        Int, modulo4: ("prop %% (0 - 2)", 1),

        Bool, eqeq: ("prop == true", 1),
        Bool, eqeq2: ("prop == false", 0),
        Bool, eqeq3: ("prop == prop", 1),
        Bool, eqeq4: ("false == prop", 0),
        Bool, eqeq5: ("true == prop", 1),
        Bool, eqeq6: ("5 == 5", 1),

        Bool, neeq: ("true != true", 0),
        Bool, neeq2: ("true != false", 1),

        Bool, gt: ("10 > 10", 0),
        Bool, gt2: ("5 > 10", 0),
        Bool, gt3: ("15 > 10", 1),

        Bool, gteq: ("10 >= 10", 1),
        Bool, gteq2: ("5 >= 10", 0),
        Bool, gteq3: ("15 >= 10", 1),

        Bool, lt: ("10 < 10", 0),
        Bool, lt2: ("5 < 10", 1),
        Bool, lt3: ("15 < 10", 0),

        Bool, lteq: ("10 <= 10", 1),
        Bool, lteq2: ("5 <= 10", 1),
        Bool, lteq3: ("15 <= 10", 0),

        Int, then: ("5 then 8", 8),
        Int, then2: ("prop then 7", 7),
        Int, then3: ("7 then prop", 5),
    );

    #[derive(Serialize, Clone, Debug)]
    struct PropObj {
        int_prop: i32,
    }

    unsafe impl TapirScript for PropObj {
        fn set_prop(&mut self, index: u8, value: i32) {
            if index != 0 {
                panic!("Invalid index {index}");
            }

            self.int_prop = value;
        }

        fn get_prop(&self, index: u8) -> i32 {
            if index != 0 {
                panic!("Invalid index {index}");
            }

            self.int_prop
        }

        type EventType = ();

        fn script(self) -> Script<Self> {
            unimplemented!("Shouldn't create the script this way in the tests")
        }

        fn create_event(&self, _index: u8, _stack: &mut Vec<i32>) -> Self::EventType {}
    }
}
