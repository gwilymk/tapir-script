use crate::TapirScript;

use agb_fixnum::Num;
use alloc::{boxed::Box, vec::Vec};

#[derive(Debug)]
pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
}

const SPAWN_FINISHED: i32 = u16::MAX as i32;

pub(crate) enum RunResult {
    Waiting,
    Finished,
    Spawn(Box<State>),
}

impl State {
    pub(crate) fn new(pc: usize, stack: Vec<i32>) -> Self {
        Self { pc, stack }
    }

    #[cfg(test)]
    pub(crate) fn stack(&self) -> &[i32] {
        &self.stack
    }

    pub(crate) fn run_until_wait(
        &mut self,
        bytecode: &[u16],
        properties: &mut dyn ObjectSafeProperties,
    ) -> RunResult {
        loop {
            let Some(instr) = bytecode.get(self.pc) else {
                return RunResult::Finished;
            };

            let parsed =
                bytecode::Instruction::n((instr >> 8) as u8).expect("Invalid bytecode instruction");

            let arg = instr & 0xff;

            extern crate std;
            std::println!(
                "{} {parsed:?} {arg} {:?}",
                self.pc,
                bytecode.get(self.pc + 1)
            );

            self.pc += 1;

            match parsed {
                bytecode::Instruction::Push8 => {
                    self.stack.push(arg as i8 as i32);
                }
                bytecode::Instruction::Push24 => todo!(),
                bytecode::Instruction::Push32 => {
                    let first = bytecode[self.pc].to_le_bytes();
                    let second = bytecode[self.pc + 1].to_le_bytes();
                    self.pc += 2;
                    let value = i32::from_le_bytes([first[0], first[1], second[0], second[1]]);
                    self.stack.push(value);
                }
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
                    return RunResult::Waiting;
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
                        bytecode::MathsOp::RealDiv => lhs / rhs, // FIXME: div_floor
                        bytecode::MathsOp::EqEq => (lhs == rhs).into(),
                        bytecode::MathsOp::NeEq => (lhs != rhs).into(),
                        bytecode::MathsOp::Gt => (lhs > rhs).into(),
                        bytecode::MathsOp::GtEq => (lhs >= rhs).into(),
                        bytecode::MathsOp::Lt => (lhs < rhs).into(),
                        bytecode::MathsOp::LtEq => (lhs <= rhs).into(),
                        bytecode::MathsOp::FixMul => {
                            (Num::<i32, 8>::from_raw(lhs) * Num::<i32, 8>::from_raw(rhs)).to_raw()
                        }
                        bytecode::MathsOp::FixDiv => {
                            (Num::<i32, 8>::from_raw(lhs) / Num::<i32, 8>::from_raw(rhs)).to_raw()
                        }
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
                bytecode::Instruction::Spawn => {
                    let target_for_spawn = bytecode[self.pc];
                    self.pc += 1;

                    let mut new_stack = self.stack.split_off(self.stack.len() - arg as usize);
                    new_stack.push(SPAWN_FINISHED);

                    return RunResult::Spawn(Box::new(Self::new(
                        target_for_spawn as usize,
                        new_stack,
                    )));
                }
                bytecode::Instruction::Return => {
                    let args = arg;
                    let [rets, shift] = bytecode[self.pc].to_be_bytes();

                    let args = args as usize;
                    let rets = rets as usize;
                    let shift = shift as usize;

                    if self.stack.len() == shift {
                        return RunResult::Finished;
                    }

                    let new_pc = self.stack[self.stack.len() - shift - 1];

                    if new_pc == SPAWN_FINISHED {
                        // this is the end of a spawned function or event
                        return RunResult::Finished;
                    }

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

pub(crate) trait ObjectSafeProperties {
    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn add_event(&mut self, index: u8, stack: &mut Vec<i32>);
}

pub(crate) struct ObjectSafePropertiesImpl<'a, T, U>
where
    T: TapirScript<EventType = U>,
{
    pub properties: &'a mut T,
    pub events: Vec<U>,
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
