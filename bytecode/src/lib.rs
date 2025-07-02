#![no_std]
#![deny(clippy::all)]

use enumn::N;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Opcode {
    // Type 1
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

    Return,

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
    pub const fn ret() -> Self {
        Self {
            opcode: Opcode::Return,
            target: 0,
            a: 0,
            b: 0,
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
    pub const fn jump(target: u32) -> Self {
        Self {
            opcode: Opcode::Jump,
            value: target,
        }
    }

    pub const fn invalid_jump() -> Self {
        Self::jump((1 << 24) - 1)
    }

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
