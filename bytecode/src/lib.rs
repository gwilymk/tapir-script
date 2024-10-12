#![no_std]

use enumn::N;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, N)]
pub enum Instruction {
    Push8,
    Push24,
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
}
