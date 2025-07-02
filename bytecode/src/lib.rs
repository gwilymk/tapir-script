#![no_std]
#![deny(clippy::all)]

use enumn::N;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Opcode {
    // Type 1
    Mov,

    Add,
    Sub,
    Mul,
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

    GetProp,
    SetProp,

    Call,
    Spawn,
    Trigger,

    JumpIf,

    Ret,
    Wait,

    // Type 2
    LoadConstant,

    // Type 3
    Jump,
}

pub fn opcode(encoded: u32) -> Option<Opcode> {
    Opcode::n(encoded.to_be_bytes()[0])
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
        u32::from_be_bytes([self.opcode as u8, self.target, self.a, self.b])
    }

    pub fn decode(encoded: u32) -> Self {
        let [opcode, target, a, b] = encoded.to_be_bytes();
        Self {
            opcode: Opcode::n(opcode).expect("Invalid encoded"),
            target,
            a,
            b,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Type2 {
    opcode: Opcode,

    pub target: u8,
    pub value: u16,
}

impl Type2 {
    const fn new(opcode: Opcode, target: u8, value: u16) -> Self {
        Self {
            opcode,
            target,
            value,
        }
    }

    pub const fn opcode(self) -> Opcode {
        self.opcode
    }

    pub const fn encode(self) -> u32 {
        let value = self.value.to_be_bytes();
        u32::from_be_bytes([self.opcode as u8, self.target, value[0], value[1]])
    }

    pub fn decode(encoded: u32) -> Self {
        let [opcode, target, value0, value1] = encoded.to_be_bytes();
        let value = u16::from_be_bytes([value0, value1]);
        Self {
            opcode: Opcode::n(opcode).expect("Invalid encoded"),
            target,
            value,
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
        let value = self.value.to_be_bytes();
        assert!(value[0] == 0, "Value too large to be encoded");
        u32::from_be_bytes([self.opcode as u8, value[1], value[2], value[3]])
    }

    pub fn decode(encoded: u32) -> Self {
        let [opcode, value1, value2, value3] = encoded.to_be_bytes();
        let value = u32::from_be_bytes([0, value1, value2, value3]);
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

    pub const fn mov(target: u8, source: u8) -> Self {
        Self::new2(Opcode::Mov, target, source)
    }

    pub const fn binop(opcode: Opcode, target: u8, lhs: u8, rhs: u8) -> Self {
        assert!(
            opcode as u8 >= Opcode::Add as u8 && opcode as u8 <= Opcode::FixDiv as u8,
            "Invalid opcode for binary operator",
        );

        Self::new3(opcode, target, lhs, rhs)
    }

    pub const fn jump_if(target: u8) -> Self {
        Self::new1(Opcode::JumpIf, target)
    }
}

impl Type2 {
    pub const fn constant(target: u8, value: u16) -> Self {
        Self::new(Opcode::LoadConstant, target, value)
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
