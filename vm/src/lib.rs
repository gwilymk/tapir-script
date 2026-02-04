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
    /// Next task ID to assign (starts at 1, 0 is the empty sentinel)
    next_task_id: u32,
}

impl<'a> Vm<'a> {
    pub fn new(bytecode: &'a [u32], initial_globals: &[i32]) -> Self {
        Self {
            bytecode,
            states: vec![State::new(0, vec![-1])],
            frame: 0,
            globals: initial_globals.to_vec(),
            next_task_id: 1, // Start at 1, 0 is reserved as empty sentinel
        }
    }

    fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
        let mut state_index = 0;
        while state_index < self.states.len() {
            // Check task status
            match self.states[state_index].status {
                state::TaskStatus::Cancelled => {
                    self.states.swap_remove(state_index);
                    continue;
                }
                state::TaskStatus::Paused => {
                    state_index += 1;
                    continue;
                }
                state::TaskStatus::Running => {}
            }

            match self.states[state_index].run_until_wait(
                self.bytecode,
                properties,
                self.frame,
                &mut self.globals,
                &mut self.next_task_id,
            ) {
                state::RunResult::Waiting => {
                    state_index += 1;
                }
                state::RunResult::Finished => {
                    self.states.swap_remove(state_index);
                }
                state::RunResult::Spawn { state } => {
                    self.states.push(*state);
                    // intentionally not increasing state_index to ensure that the spawning
                    // state continues to run.
                }
                state::RunResult::Cancel(task_id) => {
                    // Find the state with matching task_id and mark it as cancelled
                    self.cancel_task(task_id);
                    // Continue running same state (don't increment)
                    // If we cancelled self, we'll catch it on next iteration
                }
                state::RunResult::Pause(task_id) => {
                    // Find the state with matching task_id and mark it as paused
                    self.pause_task(task_id);
                    // Continue running same state (don't increment)
                    // If we paused self, we'll catch it on next iteration
                }
                state::RunResult::Resume(task_id) => {
                    // Find the state with matching task_id and mark it as not paused
                    self.resume_task(task_id);
                    // Continue running same state (don't increment)
                }
            }
        }

        self.frame += 1;
    }

    /// Cancel a task by its task ID. Returns true if a task was found and cancelled.
    /// No-op if task_id is 0 (empty sentinel) or if no matching task is found.
    pub fn cancel_task(&mut self, task_id: i32) -> bool {
        if task_id <= 0 {
            return false; // 0 and negative IDs are invalid
        }
        let task_id = task_id as u32;
        for state in &mut self.states {
            if state.task_id == task_id {
                state.status = state::TaskStatus::Cancelled;
                return true;
            }
        }
        false // Task not found (already finished)
    }

    /// Pause a task by its task ID. Returns true if a task was found and paused.
    /// No-op if task_id is 0 (empty sentinel) or if no matching task is found.
    pub fn pause_task(&mut self, task_id: i32) -> bool {
        if task_id <= 0 {
            return false; // 0 and negative IDs are invalid
        }
        let task_id = task_id as u32;
        for state in &mut self.states {
            if state.task_id == task_id {
                state.status = state::TaskStatus::Paused;
                return true;
            }
        }
        false // Task not found (already finished)
    }

    /// Resume a task by its task ID. Returns true if a task was found and resumed.
    /// No-op if task_id is 0 (empty sentinel), if no matching task is found, or if the task is cancelled.
    pub fn resume_task(&mut self, task_id: i32) -> bool {
        if task_id <= 0 {
            return false; // 0 and negative IDs are invalid
        }
        let task_id = task_id as u32;
        for state in &mut self.states {
            if state.task_id == task_id {
                // Only resume if paused (don't resurrect cancelled tasks)
                if state.status == state::TaskStatus::Paused {
                    state.status = state::TaskStatus::Running;
                    return true;
                }
                return false;
            }
        }
        false // Task not found (already finished)
    }
}

/// Uninhabited type used as default EventType for scripts with no event handlers.
/// Since this enum has no variants, it's impossible to construct, preventing
/// accidental calls to send_event on scripts that don't handle events.
pub enum NoEventType {}

/// # Safety
///
/// You should never implement this directly, and instead go through the derive macro
pub unsafe trait TapirScript {
    /// Type for outgoing events (triggers) emitted by the script.
    type TriggerType;
    /// Type for incoming events sent to the script's event handlers.
    type EventType;

    fn script(self) -> Script<Self, Self::EventType>
    where
        Self: Sized;

    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn create_event(&self, index: u8, args: &[i32]) -> Self::TriggerType;

    fn extern_call(&mut self, id: usize, stack: &mut Vec<i32>, first_arg: usize);

    /// Convert an event enum variant to stack data and bytecode offset.
    /// Generated by macro - matches on event, writes args via ConvertBetweenTapir.
    /// Writes to the provided stack (which already has return placeholder) and returns pc.
    /// Returns None if the event variant has no handler (does nothing).
    fn handle_event(event: Self::EventType, stack: &mut Vec<i32>) -> Option<usize>;
}

pub struct Script<T: TapirScript, E = NoEventType> {
    vm: Vm<'static>,
    pub properties: T,
    _event_type: core::marker::PhantomData<E>,
}

impl<T: TapirScript<EventType = E>, E> Script<T, E> {
    pub fn new(properties: T, bytecode: &'static [u32], initial_globals: &[i32]) -> Self {
        Self {
            vm: Vm::new(bytecode, initial_globals),
            properties,
            _event_type: core::marker::PhantomData,
        }
    }

    pub fn run(&mut self) -> Vec<T::TriggerType> {
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

    /// Send an event to the script, triggering the corresponding event handler.
    /// Does nothing if the event variant has no handler.
    pub fn send_event(&mut self, event: E) {
        let mut stack = vec![-1]; // return placeholder
        if let Some(pc) = T::handle_event(event, &mut stack) {
            unsafe {
                self.__private_trigger_event(stack, pc);
            }
        }
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

    use std::{fs, path::Path};

    use alloc::string::{String, ToString};
    use compiler::CompileSettings;
    use insta::{assert_ron_snapshot, glob};
    use serde::Serialize;

    use crate::{NoEventType, TapirScript};

    #[derive(Serialize, Debug)]
    struct FrameResult {
        active_states: usize,
        int_prop: i32,
    }

    #[test]
    fn stack_snapshot_tests() {
        glob!("snapshot_tests", "stack/**/*.tapir", |path| {
            std::println!("{}", path.display());

            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                available_fields: Some(vec!["int_prop".to_string()]),
                enable_optimisations: true,
                enable_prelude: true,
                has_event_type: true,
            };

            let compile_result =
                compiler::compile(path.file_name().unwrap(), &input, compiler_settings).unwrap();

            let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
            let mut prop_object = PropObj { int_prop: 5 };

            let mut results = vec![];

            let mut max_iterations = 1000;

            while !vm.states.is_empty() && max_iterations >= 0 {
                let mut object_safe_props = ObjectSafePropertiesImpl {
                    properties: &mut prop_object,
                    events: vec![],
                };

                vm.run_until_wait(&mut object_safe_props);
                results.push(FrameResult {
                    active_states: vm.states.len(),
                    int_prop: prop_object.int_prop,
                });

                max_iterations -= 1;
            }

            if max_iterations == 0 {
                panic!("ran for over 1000 waits, something seems to have gone wrong...");
            }

            assert_ron_snapshot!(results);
        });
    }

    /// A minimal test object that only supports the `assert` extern function.
    /// Used for simple behavioral tests that verify correctness via assertions.
    struct AssertTestObj {
        assertion_count: usize,
    }

    unsafe impl TapirScript for AssertTestObj {
        fn set_prop(&mut self, index: u8, _value: i32) {
            panic!("Unexpected property set at index {index}");
        }

        fn get_prop(&self, index: u8) -> i32 {
            panic!("Unexpected property get at index {index}");
        }

        type TriggerType = ();
        type EventType = NoEventType;

        fn script(self) -> Script<Self, Self::EventType> {
            unimplemented!("Shouldn't create the script this way in the tests")
        }

        fn create_event(&self, _index: u8, _args: &[i32]) -> Self::TriggerType {}

        fn extern_call(&mut self, id: usize, stack: &mut Vec<i32>, first_arg: usize) {
            match id {
                0 => {
                    // assert(value: bool)
                    let value = stack[first_arg];
                    if value == 0 {
                        panic!(
                            "Assertion #{} failed (0-indexed, {} passed before this)",
                            self.assertion_count, self.assertion_count
                        );
                    }
                    self.assertion_count += 1;
                }
                _ => panic!("Unknown extern function id: {id}"),
            }
        }

        fn handle_event(event: Self::EventType, _stack: &mut Vec<i32>) -> Option<usize> {
            match event {} // empty match on uninhabited type
        }
    }

    fn run_assert_test(path: &Path, enable_optimisations: bool) -> Option<String> {
        let input = fs::read_to_string(path).unwrap();

        let compiler_settings = CompileSettings {
            available_fields: Some(vec![]),
            enable_optimisations,
            enable_prelude: true,
            has_event_type: true,
        };

        let compile_result =
            match compiler::compile(path.file_name().unwrap(), &input, compiler_settings) {
                Ok(result) => result,
                Err(mut diagnostic) => {
                    std::eprintln!("{}", diagnostic.pretty_string(true));
                    panic!("Failed to compile: {}", path.display());
                }
            };

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
            let mut test_obj = AssertTestObj { assertion_count: 0 };

            let mut max_iterations = 1000;

            while !vm.states.is_empty() && max_iterations >= 0 {
                let mut object_safe_props = ObjectSafePropertiesImpl {
                    properties: &mut test_obj,
                    events: vec![],
                };

                vm.run_until_wait(&mut object_safe_props);
                max_iterations -= 1;
            }

            if max_iterations == 0 {
                panic!("ran for over 1000 waits, something seems to have gone wrong...");
            }
        }));

        if let Err(e) = result {
            let msg: std::string::String = if let Some(s) = e.downcast_ref::<&str>() {
                (*s).into()
            } else if let Some(s) = e.downcast_ref::<std::string::String>() {
                s.clone()
            } else {
                "unknown error".into()
            };

            return Some(msg);
        }

        None
    }

    #[test]
    fn assert_tests() {
        let mut failures = vec![];

        glob!("snapshot_tests", "assert/**/*.tapir", |path| {
            if let Some(error) = run_assert_test(path, true) {
                failures.push((path.display().to_string(), error, true));
            }

            if let Some(error) = run_assert_test(path, false) {
                failures.push((path.display().to_string(), error, false));
            }
        });

        if !failures.is_empty() {
            for (path, msg, optimisations) in failures {
                std::println!(
                    "'{path}' had error `{msg}` with optimisations {}",
                    if optimisations { "enabled" } else { "disabled" }
                );
            }

            panic!("Tests failed");
        }
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
                            enable_prelude: true,
                            has_event_type: true,
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

        type TriggerType = ();
        type EventType = NoEventType;

        fn script(self) -> Script<Self, Self::EventType> {
            unimplemented!("Shouldn't create the script this way in the tests")
        }

        fn create_event(&self, _index: u8, _args: &[i32]) -> Self::TriggerType {}

        fn extern_call(&mut self, _id: usize, _stack: &mut Vec<i32>, _first_arg: usize) {
            unimplemented!("Should never make an extern call")
        }

        fn handle_event(event: Self::EventType, _stack: &mut Vec<i32>) -> Option<usize> {
            match event {} // empty match on uninhabited type
        }
    }

    #[test]
    fn frame_starts_at_zero() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["int_prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nint_prop = frame();",
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
            enable_prelude: true,
            has_event_type: true,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nint_prop = frame(); wait; int_prop = frame(); wait; int_prop = frame();",
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
            enable_prelude: true,
            has_event_type: true,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nvar start = frame(); wait; wait; int_prop = frame() - start;",
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
            enable_prelude: true,
            has_event_type: true,
        };

        let bytecode = compiler::compile(
            "frame_test.tapir",
            "property int_prop: int;\nloop { if frame() >= 5 { break; } wait; } int_prop = frame();",
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
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: fix;\nprop = sin(0.0);";
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
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: fix;\nprop = cos(0.0);";
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
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: fix;\nprop = sqrt(4.0);";
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
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: int;\nwait; wait; prop = frame();";
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
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: int;\nvar x: fix = 3.7;\nprop = x.floor();";
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

        // x.floor() where x = 3.7 should be 3
        assert_eq!(prop_object.int_prop, 3);
    }

    #[test]
    fn builtin_ceil() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: int;\nvar x: fix = 3.2;\nprop = x.ceil();";
        let compile_result =
            compiler::compile("builtin_ceil.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // x.ceil() where x = 3.2 should be 4
        assert_eq!(prop_object.int_prop, 4);
    }

    #[test]
    fn builtin_round() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = "property prop: int;\nvar x: fix = 3.6;\nprop = x.round();";
        let compile_result =
            compiler::compile("builtin_round.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // x.round() where x = 3.6 should be 4
        assert_eq!(prop_object.int_prop, 4);
    }

    #[test]
    fn user_defined_method_on_int() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = r#"
            property prop: int;
            fn int.double(self) -> int {
                return self + self;
            }
            var x: int = 7;
            prop = x.double();
        "#;
        let compile_result =
            compiler::compile("method_int.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // 7.double() should be 14
        assert_eq!(prop_object.int_prop, 14);
    }

    #[test]
    fn user_defined_method_with_args() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = r#"
            property prop: int;
            fn int.add(self, other: int) -> int {
                return self + other;
            }
            var x: int = 5;
            prop = x.add(3);
        "#;
        let compile_result =
            compiler::compile("method_with_args.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // 5.add(3) should be 8
        assert_eq!(prop_object.int_prop, 8);
    }

    #[test]
    fn chained_method_calls() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = r#"
            property prop: int;
            fn int.double(self) -> int {
                return self + self;
            }
            fn int.add(self, n: int) -> int {
                return self + n;
            }
            var x: int = 3;
            prop = x.double().add(1).double();
        "#;
        let compile_result =
            compiler::compile("chained_methods.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // 3.double() = 6, 6.add(1) = 7, 7.double() = 14
        assert_eq!(prop_object.int_prop, 14);
    }

    #[test]
    fn method_on_struct() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = r#"
            property prop: int;
            struct Point { x: int, y: int }
            fn Point.sum(self) -> int {
                return self.x + self.y;
            }
            var p = Point(3, 4);
            prop = p.sum();
        "#;
        let compile_result =
            compiler::compile("method_struct.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Point(3, 4).sum() should be 7
        assert_eq!(prop_object.int_prop, 7);
    }

    #[test]
    fn method_returning_struct() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        let source = r#"
            property prop: int;
            struct Point { x: int, y: int }
            fn Point.scale(self, factor: int) -> Point {
                return Point(self.x * factor, self.y * factor);
            }
            var p = Point(2, 3);
            var scaled = p.scale(3);
            prop = scaled.x + scaled.y;
        "#;
        let compile_result =
            compiler::compile("method_return_struct.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Point(2, 3).scale(3) = Point(6, 9), sum = 15
        assert_eq!(prop_object.int_prop, 15);
    }

    #[test]
    fn wait_zero_is_noop() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait 0 should not advance the frame
        let source = "property prop: int;\nvar f1 = frame();\nwait 0;\nprop = frame() - f1;";
        let compile_result =
            compiler::compile("wait_zero.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 0 because wait 0 is a no-op
        assert_eq!(prop_object.int_prop, 0);
    }

    #[test]
    fn wait_one_same_as_wait() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait 1 should behave the same as wait
        let source = "property prop: int;\nvar f1 = frame();\nwait 1;\nprop = frame() - f1;";
        let compile_result = compiler::compile("wait_one.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        // Run until completion
        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 1
        assert_eq!(prop_object.int_prop, 1);
    }

    #[test]
    fn wait_n_waits_multiple_frames() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait 5 should wait 5 frames
        let source = "property prop: int;\nvar start = frame();\nwait 5;\nprop = frame() - start;";
        let compile_result = compiler::compile("wait_n.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        // Run until completion
        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 5
        assert_eq!(prop_object.int_prop, 5);
    }

    #[test]
    fn wait_negative_is_noop() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait with negative value should be no-op
        let source =
            "property prop: int;\nvar start = frame();\nwait 0 - 10;\nprop = frame() - start;";
        let compile_result =
            compiler::compile("wait_negative.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 0 because negative wait is no-op
        assert_eq!(prop_object.int_prop, 0);
    }

    #[test]
    fn wait_with_variable() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait with variable
        let source =
            "property prop: int;\nvar n = 3;\nvar start = frame();\nwait n;\nprop = frame() - start;";
        let compile_result = compiler::compile("wait_var.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 3
        assert_eq!(prop_object.int_prop, 3);
    }

    #[test]
    fn wait_with_expression() {
        let compile_settings = CompileSettings {
            available_fields: Some(vec!["prop".to_string()]),
            enable_optimisations: false,
            enable_prelude: true,
            has_event_type: true,
        };

        // wait with expression
        let source =
            "property prop: int;\nvar n = 2;\nvar start = frame();\nwait n + 3;\nprop = frame() - start;";
        let compile_result =
            compiler::compile("wait_expr.tapir", source, compile_settings).unwrap();

        let mut vm = Vm::new(&compile_result.bytecode, &compile_result.globals);
        let mut prop_object = PropObj { int_prop: 999 };

        while !vm.states.is_empty() {
            let mut object_safe_props = ObjectSafePropertiesImpl {
                properties: &mut prop_object,
                events: vec![],
            };
            vm.run_until_wait(&mut object_safe_props);
        }

        // Frame difference should be 5 (2 + 3)
        assert_eq!(prop_object.int_prop, 5);
    }
}
