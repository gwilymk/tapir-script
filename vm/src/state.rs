use crate::TapirScript;

use agb_fixnum::Num;
use alloc::{boxed::Box, vec::Vec};

type Fix = Num<i32, 8>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TaskStatus {
    Running,
    Paused,
    Cancelled,
}

#[derive(Debug)]
pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
    stack_offset: usize,
    /// Task ID for this thread (0 for main thread or threads without handles)
    pub task_id: u32,
    /// Current status of this task
    pub status: TaskStatus,
    /// Remaining frames to wait (0 = not waiting)
    wait_remaining: i32,
}

pub(crate) enum RunResult {
    Waiting,
    Finished,
    /// Spawn a new state (task ID already stored in spawner's target register)
    Spawn {
        state: Box<State>,
    },
    /// Cancel a task by its task ID
    Cancel(i32),
    /// Pause a task by its task ID
    Pause(i32),
    /// Resume a task by its task ID
    Resume(i32),
}

impl State {
    pub(crate) fn new(pc: usize, stack: Vec<i32>) -> Self {
        Self {
            pc,
            stack,
            stack_offset: 0,
            task_id: 0,
            status: TaskStatus::Running,
            wait_remaining: 0,
        }
    }

    pub(crate) fn new_with_task_id(pc: usize, stack: Vec<i32>, task_id: u32) -> Self {
        Self {
            pc,
            stack,
            stack_offset: 0,
            task_id,
            status: TaskStatus::Running,
            wait_remaining: 0,
        }
    }

    pub(crate) fn run_until_wait(
        &mut self,
        bytecode: &[u32],
        properties: &mut dyn ObjectSafeProperties,
        frame: i32,
        globals: &mut [i32],
        next_task_id: &mut u32,
    ) -> RunResult {
        // Check if still waiting from a previous WaitN
        if self.wait_remaining > 0 {
            self.wait_remaining -= 1;
            return RunResult::Waiting;
        }

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
                O::Div => binop!(a, b, a.div_euclid(b)),
                O::Mod => binop!(a, b, a.rem_euclid(b)),
                O::RealMod => binop!(a, b, a % b),
                O::RealDiv => binop!(a, b, a / b),
                O::EqEq => binop!(a, b, (a == b).into()),
                O::NeEq => binop!(a, b, (a != b).into()),
                O::Gt => binop!(a, b, (a > b).into()),
                O::GtEq => binop!(a, b, (a >= b).into()),
                O::Lt => binop!(a, b, (a < b).into()),
                O::LtEq => binop!(a, b, (a <= b).into()),
                O::FixMul => binop!(a, b, (Fix::from_raw(a) * Fix::from_raw(b)).to_raw()),
                O::FixDiv => binop!(a, b, (Fix::from_raw(a) / Fix::from_raw(b)).to_raw()),

                O::Shl => binop!(a, b, a << (b & 31)),
                O::Shr => binop!(a, b, a >> (b & 31)),
                O::BitAnd => binop!(a, b, a & b),
                O::BitOr => binop!(a, b, a | b),

                O::AddI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) + imm as i32);
                }
                O::SubI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) - imm as i32);
                }
                O::MulI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) * imm as i32);
                }
                O::DivI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a).div_euclid(imm as i32));
                }
                O::FixAddI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) + ((imm as i32) << 4));
                }
                O::FixSubI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) - ((imm as i32) << 4));
                }
                O::FixMulI => {
                    type1!(target, a, imm);
                    self.set_reg(target, (self.get_reg(a) * imm as i32) >> 4);
                }
                O::FixDivI => {
                    type1!(target, a, imm);
                    self.set_reg(target, (self.get_reg(a) << 4) / imm as i32);
                }
                O::ShlI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) << (imm & 31));
                }
                O::ShrI => {
                    type1!(target, a, imm);
                    self.set_reg(target, self.get_reg(a) >> (imm & 31));
                }

                O::Neg => {
                    type1!(target, operand);
                    self.set_reg(target, -self.get_reg(operand));
                }
                O::Not => {
                    type1!(target, operand);
                    self.set_reg(target, (self.get_reg(operand) == 0) as i32);
                }
                O::BitNot => {
                    type1!(target, operand);
                    self.set_reg(target, !self.get_reg(operand));
                }

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
                O::ExternCall => {
                    type1!(extern_id, first_arg);
                    properties.extern_call(
                        usize::from(extern_id),
                        &mut self.stack,
                        self.stack_offset + usize::from(first_arg),
                    );
                }
                O::Spawn => {
                    type1!(target, first_arg, num_args);
                    let mut new_stack = Vec::with_capacity(num_args as usize + 1);
                    new_stack.push(-1);

                    if num_args > 0 {
                        let stack_to_copy_start = self.stack_offset + usize::from(first_arg + 1);
                        let stack_to_copy_end = stack_to_copy_start + usize::from(num_args);
                        new_stack.extend(&self.stack[stack_to_copy_start..stack_to_copy_end]);
                    }

                    let new_thread_pc = self.pc;
                    self.pc += 1; // skip over the jump instruction

                    // Assign task ID
                    let task_id = *next_task_id;
                    *next_task_id += 1;

                    // Store task ID in the target register
                    self.set_reg(target, task_id as i32);

                    return RunResult::Spawn {
                        state: Box::new(Self::new_with_task_id(new_thread_pc, new_stack, task_id)),
                    };
                }
                O::Trigger => {
                    type1!(id, first_arg);

                    let stack_to_copy_start = self.stack_offset + usize::from(first_arg + 1);
                    let args = self.stack.get(stack_to_copy_start..).unwrap_or(&[]);
                    properties.add_event(id, args);
                }

                O::JumpIf => {
                    type1!(test);

                    let test_value = self.get_reg(test);
                    if test_value == 0 {
                        self.pc += 1;
                    }
                }

                O::JumpIfNot => {
                    type1!(test);

                    let test_value = self.get_reg(test);
                    if test_value != 0 {
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
                    type1!(frames_reg, has_frames);

                    if has_frames == 0 {
                        // Simple wait 1 frame (original behavior)
                        return RunResult::Waiting;
                    }

                    // Wait N frames
                    let frames = self.get_reg(frames_reg);

                    if frames <= 0 {
                        // wait 0 or negative = no-op, continue execution
                    } else if frames == 1 {
                        // wait 1 = same as regular wait
                        return RunResult::Waiting;
                    } else {
                        // wait N where N > 1: wait this frame, then N-1 more frames
                        self.wait_remaining = frames - 1;
                        return RunResult::Waiting;
                    }
                }

                O::LoadConstant => {
                    type1!(target);
                    let constant = bytecode[self.pc];
                    self.pc += 1;

                    self.set_reg(target, constant as i32);
                }
                O::LoadI => {
                    let t2 = bytecode::Type2::decode(instr);
                    self.set_reg(t2.reg, t2.imm as i32);
                }
                O::SetPropI => {
                    let t2 = bytecode::Type2::decode(instr);
                    properties.set_prop(t2.reg, t2.imm as i32);
                }
                O::SetGlobalI => {
                    let t2 = bytecode::Type2::decode(instr);
                    globals[t2.reg as usize] = t2.imm as i32;
                }
                O::CallBuiltin => {
                    type1!(target, builtin_id, first_arg);
                    let builtin_id = builtin_id as i8 as i16;

                    // Special case: builtin ID -2 is task.cancel()
                    if builtin_id == -2 {
                        let task_id = self.get_reg(first_arg);
                        return RunResult::Cancel(task_id);
                    }

                    // Special case: builtin ID -3 is task.pause()
                    if builtin_id == -3 {
                        let task_id = self.get_reg(first_arg);
                        return RunResult::Pause(task_id);
                    }

                    // Special case: builtin ID -4 is task.resume()
                    if builtin_id == -4 {
                        let task_id = self.get_reg(first_arg);
                        return RunResult::Resume(task_id);
                    }

                    let args_start = self.stack_offset + usize::from(first_arg);
                    let args = self.stack.get(args_start..).unwrap_or(&[]);

                    let result = if builtin_id >= 0 {
                        builtins::execute_pure(builtin_id, args)
                            .unwrap_or_else(|| panic!("Unknown pure builtin ID {builtin_id}"))
                            .unwrap_or_else(|e| panic!("Builtin error: {e:?}"))
                    } else {
                        let ctx = builtins::Context { frame };
                        builtins::execute_impure(builtin_id, args, &ctx)
                            .unwrap_or_else(|| panic!("Unknown impure builtin ID {builtin_id}"))
                    };

                    self.set_reg(target, result);
                }
                O::GetGlobal => {
                    type1!(target, global_index);
                    self.set_reg(target, globals[global_index as usize]);
                }
                O::SetGlobal => {
                    type1!(value, global_index);
                    globals[global_index as usize] = self.get_reg(value);
                }

                O::StackAlloc => {
                    type1!(size);
                    let needed = self.stack_offset + size as usize;
                    if self.stack.len() < needed {
                        self.stack.resize(needed, 0);
                    }
                }

                O::Jump => {
                    let target = instr >> 8;
                    self.pc = target as usize;
                }
            }
        }
    }

    fn set_reg(&mut self, reg: u8, value: i32) {
        let reg_id = self.reg_id(reg);
        unsafe {
            *self.stack.get_unchecked_mut(reg_id) = value;
        }
    }

    fn get_reg(&self, reg: u8) -> i32 {
        let reg_id = self.reg_id(reg);
        unsafe { *self.stack.get_unchecked(reg_id) }
    }

    fn reg_id(&self, reg: u8) -> usize {
        self.stack_offset + reg as usize
    }
}

pub(crate) trait ObjectSafeProperties {
    fn set_prop(&mut self, index: u8, value: i32);
    fn get_prop(&self, index: u8) -> i32;

    fn add_event(&mut self, index: u8, args: &[i32]);

    fn extern_call(&mut self, id: usize, stack: &mut [i32], first_arg: usize);
}

pub(crate) struct ObjectSafePropertiesImpl<'a, T, U>
where
    T: TapirScript<TriggerType = U>,
{
    pub properties: &'a mut T,
    pub events: Vec<U>,
}

impl<'a, T, U> ObjectSafeProperties for ObjectSafePropertiesImpl<'a, T, U>
where
    T: TapirScript<TriggerType = U>,
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

    fn extern_call(&mut self, id: usize, stack: &mut [i32], first_arg: usize) {
        self.properties.extern_call(id, stack, first_arg);
    }
}
