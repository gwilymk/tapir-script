#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;

use alloc::vec;

struct Vm<'a> {
    bytecode: &'a [u16],
    states: Vec<State>,
}

struct State {
    pc: usize,
    stack: Vec<i32>,
}

impl<'a> Vm<'a> {
    pub fn new(bytecode: &'a [u16]) -> Self {
        Self {
            bytecode,
            states: vec![State::at(0)],
        }
    }

    fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
        for state_index in (0..self.states.len()).rev() {
            if self.states[state_index].run_until_wait(self.bytecode, properties)
                == VmState::Finished
            {
                self.states.swap_remove(state_index);
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

trait ObjectSafeProperties {
    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn add_event(&mut self, index: u8, stack: &mut Vec<i32>);
}

struct ObjectSafePropertiesImpl<'a, T, U>
where
    T: TapirScript<EventType = U>,
{
    properties: &'a mut T,
    events: Vec<U>,
}

impl<'a, T, U> ObjectSafeProperties for ObjectSafePropertiesImpl<'a, T, U>
where
    T: TapirScript<EventType = U>,
{
    fn set_prop(&mut self, index: u8, value: i32) {
        self.properties.set_prop(index, value);
    }

    fn get_prop(&self, index: u8) -> i32 {
        self.properties.get_prop(index)
    }

    fn add_event(&mut self, index: u8, stack: &mut Vec<i32>) {
        self.events.push(self.properties.create_event(index, stack));
    }
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

    #[doc(hidden)]
    pub unsafe fn __private_trigger_event(&mut self, mut initial_stack: Vec<i32>, pc: usize) {
        initial_stack.push(0);

        self.vm.states.push(State {
            pc,
            stack: initial_stack,
        });
    }
}

impl State {
    fn run_until_wait(
        &mut self,
        bytecode: &[u16],
        properties: &mut dyn ObjectSafeProperties,
    ) -> VmState {
        loop {
            let Some(instr) = bytecode.get(self.pc) else {
                return VmState::Finished;
            };

            let parsed =
                bytecode::Instruction::n((instr >> 8) as u8).expect("Invalid bytecode instruction");

            let arg = instr & 0xff;

            self.pc += 1;

            match parsed {
                bytecode::Instruction::Push8 => {
                    self.stack.push(arg as i8 as i32);
                }
                bytecode::Instruction::Push24 => todo!(),
                bytecode::Instruction::Dup => {
                    self.stack
                        .push(self.stack[self.stack.len() - arg as usize - 1]);
                }
                bytecode::Instruction::Drop => {
                    let desired_size = self.stack.len() - arg as usize;
                    self.stack.truncate(desired_size);
                }
                bytecode::Instruction::GetProp => {
                    self.stack.push(properties.get_prop(arg as u8));
                }
                bytecode::Instruction::SetProp => {
                    properties.set_prop(arg as u8, self.stack.pop().expect("Stack underflow"));
                }
                bytecode::Instruction::Nop => {}
                bytecode::Instruction::Wait => {
                    return VmState::Waiting;
                }
                bytecode::Instruction::Move => {
                    let move_location = self.stack.len() - arg as usize - 1;
                    self.stack[move_location] = self.stack.pop().expect("Stack underflow");
                }
                bytecode::Instruction::MathsOp => {
                    let op = bytecode::MathsOp::n(arg as u8).expect("Invalid maths op");
                    let rhs = self.stack.pop().expect("Stack underflow");
                    let lhs = self.stack.pop().expect("Stack underflow");

                    let result = match op {
                        bytecode::MathsOp::Add => lhs + rhs,
                        bytecode::MathsOp::Sub => lhs - rhs,
                        bytecode::MathsOp::Mul => lhs * rhs,
                        bytecode::MathsOp::RealMod => lhs.rem_euclid(rhs),
                        bytecode::MathsOp::RealDiv => lhs / rhs,
                        bytecode::MathsOp::EqEq => (lhs == rhs).into(),
                        bytecode::MathsOp::NeEq => (lhs != rhs).into(),
                        bytecode::MathsOp::Gt => (lhs > rhs).into(),
                        bytecode::MathsOp::GtEq => (lhs >= rhs).into(),
                        bytecode::MathsOp::Lt => (lhs < rhs).into(),
                        bytecode::MathsOp::LtEq => (lhs <= rhs).into(),
                    };

                    self.stack.push(result);
                }
                bytecode::Instruction::JumpIfFalse => {
                    let target_for_jump = bytecode[self.pc];
                    self.pc += 1;

                    if *self.stack.last().expect("Stack underflow") == 0 {
                        self.pc = target_for_jump as usize;
                    }
                }
                bytecode::Instruction::Jump => {
                    let target_for_jump = bytecode[self.pc];
                    self.pc = target_for_jump as usize;
                }
                bytecode::Instruction::Call => {
                    let target_for_jump = bytecode[self.pc];
                    self.stack.push((self.pc + 1) as i32);

                    self.pc = target_for_jump as usize;
                }
                bytecode::Instruction::Return => {
                    let args = arg;
                    let [rets, shift] = bytecode[self.pc].to_be_bytes();

                    let args = args as usize;
                    let rets = rets as usize;
                    let shift = shift as usize;

                    if self.stack.len() == shift {
                        return VmState::Finished;
                    }

                    let new_pc = self.stack[self.stack.len() - shift - 1];
                    // extra -1 to cover the new program counter
                    let copy_range = (self.stack.len() - rets)..;
                    let copy_dest = self.stack.len() - args - shift - 1;

                    self.stack.copy_within(copy_range, copy_dest);
                    self.stack
                        .truncate(self.stack.len() - args - shift + rets - 1);

                    self.pc = new_pc as usize;
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VmState {
    Waiting,
    Finished,
}

impl State {
    fn at(pc: usize) -> Self {
        Self { pc, stack: vec![] }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::fs;

    use compiler::{CompileSettings, Property, Type};
    use insta::{assert_ron_snapshot, glob};
    use serde::Serialize;

    use crate::TapirScript;

    #[test]
    fn stack_snapshot_tests() {
        glob!("snapshot_tests", "stack/**/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };

            let bytecode =
                compiler::compile(path.file_name().unwrap(), &input, compiler_settings).unwrap();

            let mut vm = Vm::new(&bytecode);
            let mut prop_object = PropObj { int_prop: 5 };

            let mut stack_at_waits = vec![];

            let mut max_iterations = 1000;

            while !vm.states.is_empty() && max_iterations >= 0 {
                let object_safe_props = ObjectSafePropertiesImpl {
                    properties: &mut prop_object,
                    events: vec![],
                };

                vm.run_until_wait(&mut object_safe_props);
                stack_at_waits.push((
                    vm.states
                        .iter()
                        .map(|state| state.stack.clone())
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
                        };

                        let bytecode = compiler::compile(
                            concat!(stringify!($name), ".tapir"),
                            concat!("prop = ", $code, ";"),
                            compile_settings
                        ).unwrap();

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
    );

    #[derive(Serialize, Clone, Debug)]
    struct PropObj {
        int_prop: i32,
    }

    impl TapirScript for PropObj {
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
