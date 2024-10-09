#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;

use alloc::vec;

pub struct Vm<'a> {
    bytecode: &'a [u16],
    states: Vec<State>,
}

pub trait VmProperties {
    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;
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

    pub fn step(&mut self, properties: &mut dyn VmProperties) {
        for state_index in (0..self.states.len()).rev() {
            if self.states[state_index].step(self.bytecode, properties) == VmState::Finished {
                self.states.swap_remove(state_index);
            }
        }
    }
}

impl State {
    fn step(&mut self, bytecode: &[u16], properties: &mut dyn VmProperties) -> VmState {
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
                    let lhs = self.stack.pop().expect("Stack underflow");
                    let rhs = self.stack.pop().expect("Stack underflow");

                    let result = match op {
                        bytecode::MathsOp::Add => lhs + rhs,
                        bytecode::MathsOp::Sub => lhs - rhs,
                        bytecode::MathsOp::Mul => lhs * rhs,
                        bytecode::MathsOp::RealMod => lhs.rem_euclid(rhs),
                        bytecode::MathsOp::RealDiv => lhs / rhs,
                        bytecode::MathsOp::EqEq => (lhs == rhs).into(),
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
                bytecode::Instruction::Call => todo!(),
                bytecode::Instruction::Return => todo!(),
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

    use crate::VmProperties;

    #[test]
    fn stack_snapshot_tests() {
        glob!("snapshot_tests", "stack/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };

            let bytecode = compiler::compile(&input, compiler_settings).unwrap();

            let mut vm = Vm::new(&bytecode);
            let mut prop_object = PropObj { int_prop: 5 };

            let mut stack_at_waits = vec![];

            while !vm.states.is_empty() {
                vm.step(&mut prop_object);
                stack_at_waits.push((
                    vm.states
                        .iter()
                        .map(|state| state.stack.clone())
                        .collect::<Vec<_>>(),
                    prop_object.clone(),
                ));
            }

            assert_ron_snapshot!(stack_at_waits);
        });
    }

    #[derive(Serialize, Clone)]
    struct PropObj {
        int_prop: i32,
    }

    impl VmProperties for PropObj {
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
    }
}
