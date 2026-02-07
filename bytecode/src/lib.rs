#![no_std]
#![deny(clippy::all)]

use enumn::N;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Opcode {
    // Type 1
    Mov,
    ///< `mov a, b` => `b = a`
    // Binops
    Add,
    ///< `add a, b, c` => `a = b + c`
    Sub,
    Mul,
    Div,
    Mod,
    RealMod,
    RealDiv,

    EqEq,
    NeEq,
    Gt,
    GtEq,
    Lt,
    LtEq,

    FixMul,
    FixDiv,

    Shl,
    Shr,
    BitAnd,
    BitOr,

    // Immediate binops (b field is immediate value, not a register)
    AddI,
    SubI,
    MulI,
    DivI,
    FixAddI,
    FixSubI,
    FixMulI,
    FixDivI,
    ShlI,
    ShrI,

    // Unary ops
    Neg,
    Not,
    BitNot,

    GetProp,
    SetProp,
    GetGlobal,
    SetGlobal,

    Call,
    ExternCall,
    CallBuiltin,
    Spawn,
    Trigger,

    JumpIf,

    Ret,
    Wait,

    LoadConstant,

    // Type 3
    Jump,
}

pub fn opcode(encoded: u32) -> Option<Opcode> {
    Opcode::n((encoded & 0xFF) as u8)
}

#[derive(Clone, Copy)]
pub struct Type1 {
    opcode: Opcode,

    pub target: u8,
    pub a: u8,
    pub b: u8,
}

impl Type1 {
    const fn new0(opcode: Opcode) -> Self {
        Self::new1(opcode, 0)
    }

    const fn new1(opcode: Opcode, target: u8) -> Self {
        Self::new2(opcode, target, 0)
    }

    const fn new2(opcode: Opcode, target: u8, a: u8) -> Self {
        Self::new3(opcode, target, a, 0)
    }

    const fn new3(opcode: Opcode, target: u8, a: u8, b: u8) -> Self {
        Self {
            opcode,
            target,
            a,
            b,
        }
    }

    pub const fn opcode(self) -> Opcode {
        self.opcode
    }

    pub const fn encode(self) -> u32 {
        u32::from_le_bytes([self.opcode as u8, self.target, self.a, self.b])
    }

    pub fn decode(encoded: u32) -> Self {
        let [opcode, target, a, b] = encoded.to_le_bytes();
        Self {
            opcode: Opcode::n(opcode).expect("Invalid encoded"),
            target,
            a,
            b,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Type3 {
    opcode: Opcode,

    pub value: u32,
}

impl Type3 {
    pub const fn opcode(self) -> Opcode {
        self.opcode
    }

    pub const fn encode(self) -> u32 {
        assert!(self.value < (1 << 24), "Value too large to be encoded");
        (self.value << 8) | (self.opcode as u32)
    }

    pub fn decode(encoded: u32) -> Self {
        let opcode = (encoded & 0xFF) as u8;
        let value = encoded >> 8;
        Self {
            opcode: Opcode::n(opcode).expect("Invalid encoded"),
            value,
        }
    }
}

/// Creation functions for the various opcodes
impl Type1 {
    pub const fn ret() -> Self {
        Self::new0(Opcode::Ret)
    }

    pub const fn wait() -> Self {
        Self::new0(Opcode::Wait)
    }

    /// Wait N frames (read count from register).
    /// Format: target = frames register, a = 1 (has_frames flag), b = unused
    pub const fn wait_n(frames_reg: u8) -> Self {
        Self::new2(Opcode::Wait, frames_reg, 1)
    }

    pub const fn mov(target: u8, source: u8) -> Self {
        Self::new2(Opcode::Mov, target, source)
    }

    pub const fn binop(opcode: Opcode, target: u8, lhs: u8, rhs: u8) -> Self {
        assert!(
            opcode as u8 >= Opcode::Add as u8 && opcode as u8 <= Opcode::BitOr as u8,
            "Invalid opcode for binary operator",
        );

        Self::new3(opcode, target, lhs, rhs)
    }

    /// Immediate binary operation: target = a op imm
    /// The b field contains an immediate value, not a register index.
    pub const fn binop_imm(opcode: Opcode, target: u8, lhs: u8, imm: u8) -> Self {
        assert!(
            opcode as u8 >= Opcode::AddI as u8 && opcode as u8 <= Opcode::ShrI as u8,
            "Invalid opcode for immediate binary operator",
        );

        Self::new3(opcode, target, lhs, imm)
    }

    pub const fn unaryop(opcode: Opcode, target: u8, operand: u8) -> Self {
        assert!(
            opcode as u8 >= Opcode::Neg as u8 && opcode as u8 <= Opcode::BitNot as u8,
            "Invalid opcode for unary operator",
        );

        Self::new2(opcode, target, operand)
    }

    pub const fn jump_if(target: u8) -> Self {
        Self::new1(Opcode::JumpIf, target)
    }

    pub const fn call(first_arg: u8) -> Self {
        Self::new1(Opcode::Call, first_arg)
    }

    pub const fn extern_call(extern_id: u8, first_arg: u8) -> Self {
        Self::new2(Opcode::ExternCall, extern_id, first_arg)
    }

    /// Call a builtin function.
    /// Format: target = register to store result, builtin_id = i8 as u8, first_arg = first argument register
    /// Positive IDs are pure (constant foldable), negative IDs are impure (need runtime state)
    pub const fn call_builtin(target: u8, builtin_id: i8, first_arg: u8) -> Self {
        Self::new3(Opcode::CallBuiltin, target, builtin_id as u8, first_arg)
    }

    /// Spawn a new task. Returns the task ID in the target register.
    /// Format: target = register to store task ID, first_arg = first argument register, num_args = number of arguments
    pub const fn spawn(target: u8, first_arg: u8, num_args: u8) -> Self {
        Self::new3(Opcode::Spawn, target, first_arg, num_args)
    }

    pub const fn trigger(id: u8, first_arg: u8) -> Self {
        Self::new2(Opcode::Trigger, id, first_arg)
    }

    pub const fn constant(target: u8) -> Self {
        Self::new1(Opcode::LoadConstant, target)
    }

    pub const fn get_prop(target: u8, prop_index: u8) -> Self {
        Self::new2(Opcode::GetProp, target, prop_index)
    }

    pub const fn set_prop(value: u8, prop_index: u8) -> Self {
        Self::new2(Opcode::SetProp, value, prop_index)
    }

    pub const fn get_global(target: u8, global_index: u8) -> Self {
        Self::new2(Opcode::GetGlobal, target, global_index)
    }

    pub const fn set_global(value: u8, global_index: u8) -> Self {
        Self::new2(Opcode::SetGlobal, value, global_index)
    }
}

impl Type3 {
    pub const fn jump(target: u32) -> Self {
        Self {
            opcode: Opcode::Jump,
            value: target,
        }
    }

    pub const fn invalid_jump() -> Self {
        Self::jump((1 << 24) - 1)
    }
}
