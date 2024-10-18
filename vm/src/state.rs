use crate::{TapirScript, VmState};

use alloc::vec::Vec;

pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
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
