#![no_std]
#![deny(clippy::all)]

use enumn::N;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Instruction {
    Push8,
    Push32,
    Dup,
    Drop,
    GetProp,
    SetProp,
    Nop,
    Wait,
    Move,
    MathsOp,
    JumpIfFalse,
    Jump,
    Call,
    Return,
    Spawn,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum MathsOp {
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
}
