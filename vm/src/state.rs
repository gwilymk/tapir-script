use crate::TapirScript;

use agb_fixnum::Num;
use alloc::{boxed::Box, vec::Vec};

type Fix = Num<i32, 8>;

#[derive(Debug)]
pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
    stack_offset: usize,
}

pub(crate) enum RunResult {
    Waiting,
    Finished,
    Spawn(Box<State>),
}

impl State {
    pub(crate) fn new(pc: usize, stack: Vec<i32>) -> Self {
        Self {
            pc,
            stack,
            stack_offset: 0,
        }
    }

    #[cfg(test)]
    pub(crate) fn stack(&self) -> &[i32] {
        &self.stack
    }

    pub(crate) fn run_until_wait(
        &mut self,
        bytecode: &[u32],
        properties: &mut dyn ObjectSafeProperties,
    ) -> RunResult {
        loop {
            let Some(&instr) = bytecode.get(self.pc) else {
                return RunResult::Finished;
            };

            let opcode = bytecode::opcode(instr).expect("Invalid bytecode instruction");

            self.pc += 1;

            use bytecode::Opcode as O;

            macro_rules! type1 {
                ($target:ident) => {
                    type1!($target, _a)
                };
                ($target:ident, $a:ident) => {
                    type1!($target, $a, _b)
                };
                ($target:ident, $a:ident, $b:ident) => {
                    let bytecode::Type1 {
                        target: $target,
                        a: $a,
                        b: $b,
                        ..
                    } = bytecode::Type1::decode(instr);
                };
            }

            macro_rules! binop {
                ($a:ident, $b:ident, $op:expr) => {{
                    type1!(target, a, b);
                    let $a = self.get_reg(a);
                    let $b = self.get_reg(b);
                    self.set_reg(target, $op);
                }};
            }

            match opcode {
                O::Mov => {
                    type1!(target, a);
                    self.set_reg(target, self.get_reg(a));
                }

                O::Add => binop!(a, b, a + b),
                O::Sub => binop!(a, b, a - b),
                O::Mul => binop!(a, b, a * b),
                O::RealMod => binop!(a, b, a.rem_euclid(b)),
                O::RealDiv => binop!(a, b, a / b), // FIXME: div_floor
                O::EqEq => binop!(a, b, (a == b).into()),
                O::NeEq => binop!(a, b, (a != b).into()),
                O::Gt => binop!(a, b, (a > b).into()),
                O::GtEq => binop!(a, b, (a >= b).into()),
                O::Lt => binop!(a, b, (a < b).into()),
                O::LtEq => binop!(a, b, (a <= b).into()),
                O::FixMul => binop!(a, b, (Fix::from_raw(a) * Fix::from_raw(b)).to_raw()),
                O::FixDiv => binop!(a, b, (Fix::from_raw(a) / Fix::from_raw(b)).to_raw()),

                O::GetProp => {
                    type1!(target, prop_index);
                    self.set_reg(target, properties.get_prop(prop_index));
                }
                O::SetProp => {
                    type1!(value, prop_index);
                    properties.set_prop(prop_index, self.get_reg(value));
                }

                O::Call => {
                    type1!(first_arg);
                    let reg_value = (first_arg as u32) << 24 | (self.pc as u32);
                    self.set_reg(first_arg, reg_value as i32);
                    self.stack_offset += first_arg as usize;
                }
                O::Spawn => {
                    type1!(first_arg, num_args);
                    let mut new_stack = Vec::with_capacity(num_args as usize + 1);
                    new_stack.push(-1);

                    let stack_to_copy_start = self.stack_offset + usize::from(first_arg + 1);
                    let stack_to_copy_end = stack_to_copy_start + usize::from(num_args);
                    new_stack.extend(&self.stack[stack_to_copy_start..stack_to_copy_end]);

                    let new_thread_pc = self.pc;
                    self.pc += 1; // skip over the jump instruction

                    return RunResult::Spawn(Box::new(Self::new(new_thread_pc, new_stack)));
                }
                O::Trigger => {
                    type1!(id, first_arg);

                    let stack_to_copy_start = self.stack_offset + usize::from(first_arg + 1);
                    properties.add_event(id, &self.stack[stack_to_copy_start..]);
                }

                O::JumpIf => {
                    type1!(test);

                    let test_value = self.get_reg(test);
                    if test_value == 0 {
                        self.pc += 1;
                    }
                }

                O::Ret => {
                    let value = self.get_reg(0);
                    if value == -1 {
                        return RunResult::Finished;
                    }

                    let value = value as u32;
                    let offset = value >> 24;
                    self.stack_offset -= offset as usize;

                    let new_pc = value & 0x00FF_FFFF;
                    self.pc = new_pc as usize + 1; // skip the jump instruction
                }
                O::Wait => {
                    return RunResult::Waiting;
                }

                O::LoadConstant => {
                    type1!(target);
                    let constant = bytecode[self.pc];
                    self.pc += 1;

                    self.set_reg(target, constant as i32);
                }

                O::Jump => {
                    let target = instr & 0x00FF_FFFF;
                    self.pc = target as usize;
                }
            }
        }
    }

    fn set_reg(&mut self, reg: u8, value: i32) {
        *self.reg(reg) = value;
    }

    fn get_reg(&self, reg: u8) -> i32 {
        let reg_id = self.reg_id(reg);
        self.stack.get(reg_id).copied().unwrap_or(0)
    }

    fn reg_id(&self, reg: u8) -> usize {
        self.stack_offset + reg as usize
    }

    fn reg(&mut self, reg: u8) -> &mut i32 {
        let reg_id = self.reg_id(reg);
        self.stack.resize(self.stack.len().max(reg_id + 1), 0);

        &mut self.stack[reg_id]
    }
}

pub(crate) trait ObjectSafeProperties {
    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn add_event(&mut self, index: u8, args: &[i32]);
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

    fn add_event(&mut self, index: u8, args: &[i32]) {
        self.events.push(self.properties.create_event(index, args));
    }
}
