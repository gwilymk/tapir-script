#![deny(clippy::all)]
#![no_std]
extern crate alloc;

mod state;

use alloc::{vec, vec::Vec};
use state::{ObjectSafeProperties, ObjectSafePropertiesImpl, State};

struct Vm<'a> {
    bytecode: &'a [u32],
    states: Vec<State>,
    frame: i32,
    globals: Vec<i32>,
}

impl<'a> Vm<'a> {
    pub fn new(bytecode: &'a [u32], initial_globals: &[i32]) -> Self {
        Self {
            bytecode,
            states: vec![State::new(0, vec![-1])],
            frame: 0,
            globals: initial_globals.to_vec(),
        }
    }

    fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
        let mut state_index = 0;
        while state_index < self.states.len() {
            match self.states[state_index].run_until_wait(
                self.bytecode,
                properties,
                self.frame,
                &mut self.globals,
            ) {
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

        self.frame += 1;
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

    fn create_event(&self, index: u8, args: &[i32]) -> Self::EventType;

    fn extern_call(&mut self, id: usize, stack: &mut Vec<i32>, first_arg: usize);
}

pub struct Script<T: TapirScript> {
    vm: Vm<'static>,
    pub properties: T,
}

impl<T: TapirScript> Script<T> {
    pub fn new(properties: T, bytecode: &'static [u32], initial_globals: &[i32]) -> Self {
        Self {
            vm: Vm::new(bytecode, initial_globals),
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
    pub unsafe fn __private_trigger_event(&mut self, initial_stack: Vec<i32>, pc: usize) {
        self.vm.states.push(State::new(pc, initial_stack));
    }
}

#[cfg(test)]
mod test {
    extern crate std;

    use super::*;

    use std::fs;

    use alloc::string::ToString;
    use compiler::CompileSettings;
    use insta::{assert_ron_snapshot, glob};
    use serde::Serialize;

    use crate::TapirScript;

    #[test]
    fn stack_snapshot_tests() {
        glob!("snapshot_tests", "stack/**/*.tapir", |path| {
            std::println!("{}", path.display());

            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                available_fields: Some(vec!["int_prop".to_string()]),
                enable_optimisations: true,
            };

            let compile_result =
                compiler::compile(path.file_name().unwrap(), &input, compiler_settings).unwrap();

            let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
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
                        let type_str = match stringify!($type) {
                            "Int" => "int",
                            "Bool" => "bool",
                            "Fix" => "fix",
                            _ => panic!("Unknown type"),
                        };

                        let compile_settings = CompileSettings {
                            available_fields: Some(vec!["prop".to_string()]),
                            enable_optimisations: false,
                        };

                        let source = std::format!("property prop: {};\nprop = {};", type_str, $code);
                        let compile_result = compiler::compile(
                            concat!(stringify!($name), ".tapir"),
                            &source,
                            compile_settings
                        ).unwrap();

                        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
                        // For Int tests, use 5; for Bool tests, use 1 (true)
                        let mut prop_object = PropObj {
                            int_prop: if type_str == "int" { 5 } else { 1 },
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

        fn create_event(&self, _index: u8, _args: &[i32]) -> Self::EventType {}

        fn extern_call(&mut self, _id: usize, _stack: &mut Vec<i32>, _first_arg: usize) {
            unimplemented!("Should never make an extern call")
        }
    }

    #[test]
    fn frame_starts_at_zero() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["int_prop".to_string()]),
            enable_optimisations: false,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nint_prop = frame;",
            compile_settings,
        )
        .unwrap()
        .bytecode;

        let mut vm = Vm::new(&bytecode, &[]);
        let mut prop_object = PropObj { int_prop: 999 };

        let mut object_safe_props = ObjectSafePropertiesImpl {
            properties: &mut prop_object,
            events: vec![],
        };

        vm.run_until_wait(&mut object_safe_props);

        assert_eq!(prop_object.int_prop, 0, "frame should be 0 on first run");
    }

    #[test]
    fn frame_increments_each_run() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["int_prop".to_string()]),
            enable_optimisations: false,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nint_prop = frame; wait; int_prop = frame; wait; int_prop = frame;",
            compile_settings,
        )
        .unwrap()
        .bytecode;

        let mut vm = Vm::new(&bytecode, &[]);
        let mut prop_object = PropObj { int_prop: 999 };

        // First run - frame should be 0
        {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }
        assert_eq!(prop_object.int_prop, 0, "frame should be 0 on first run");

        // Second run - frame should be 1
        {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }
        assert_eq!(prop_object.int_prop, 1, "frame should be 1 on second run");

        // Third run - frame should be 2
        {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }
        assert_eq!(prop_object.int_prop, 2, "frame should be 2 on third run");
    }

    #[test]
    fn frame_in_expression() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["int_prop".to_string()]),
            enable_optimisations: false,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nvar start = frame; wait; wait; int_prop = frame - start;",
            compile_settings,
        )
        .unwrap()
        .bytecode;

        let mut vm = Vm::new(&bytecode, &[]);
        let mut prop_object = PropObj { int_prop: 999 };

        // Run 3 times (to pass 2 waits and reach the assignment)
        for _ in 0..3 {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        assert_eq!(prop_object.int_prop, 2, "frame difference should be 2");
    }

    #[test]
    fn frame_in_loop_condition() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["int_prop".to_string()]),
            enable_optimisations: false,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nloop { if frame >= 5 { break; } wait; } int_prop = frame;",
            compile_settings,
        )
        .unwrap()
        .bytecode;

        let mut vm = Vm::new(&bytecode, &[]);
        let mut prop_object = PropObj { int_prop: 999 };

        // Run until the script completes
        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        assert_eq!(
            prop_object.int_prop, 5,
            "frame should be 5 after loop exits"
        );
    }

    #[test]
    fn builtin_sin_zero() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
        };

        let source = "builtin(0) fn sin(x: fix) -> fix;\nproperty prop: fix;\nprop = sin(0.0);";
        let compile_result =
            compiler::compile("builtin_sin.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // sin(0) should be 0
        assert_eq!(prop_object.int_prop, 0);
    }

    #[test]
    fn builtin_cos_zero() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
        };

        let source = "builtin(1) fn cos(x: fix) -> fix;\nproperty prop: fix;\nprop = cos(0.0);";
        let compile_result =
            compiler::compile("builtin_cos.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // cos(0) should be 1.0, which is 256 in fixed point (24.8)
        assert_eq!(prop_object.int_prop, 256);
    }

    #[test]
    fn builtin_sqrt_four() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
        };

        let source = "builtin(2) fn sqrt(x: fix) -> fix;\nproperty prop: fix;\nprop = sqrt(4.0);";
        let compile_result =
            compiler::compile("builtin_sqrt.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // sqrt(4) should be 2.0, which is 512 in fixed point (24.8)
        assert_eq!(prop_object.int_prop, 512);
    }

    #[test]
    fn builtin_frame_function() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
        };

        let source =
            "builtin(-1) fn frame() -> int;\nproperty prop: int;\nwait; wait; prop = frame();";
        let compile_result =
            compiler::compile("builtin_frame.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        // Run 3 times to execute: initial run (wait), frame 1 (wait), frame 2 (assign prop = frame())
        for _ in 0..3 {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // frame() should return 2 after two waits
        assert_eq!(prop_object.int_prop, 2);
    }

    #[test]
    fn builtin_floor() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
        };

        let source =
            "builtin(3) fn floor(x: fix) -> int;\nproperty prop: int;\nprop = floor(3.7);";
        let compile_result =
            compiler::compile("builtin_floor.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // floor(3.7) should be 3
        assert_eq!(prop_object.int_prop, 3);
    }
}
