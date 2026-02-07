use std::slice;

use crate::ast::SymbolId;

use super::{BlockExitInstr, Operand, StoreValue, TapIr};

pub(super) enum SymbolIter<'a> {
    None,
    One(Option<SymbolId>),
    Two(Option<SymbolId>, Option<SymbolId>),
    Many(slice::Iter<'a, SymbolId>),
}

impl<'a> SymbolIter<'a> {
    pub fn new_source(instr: &'a TapIr) -> Self {
        match instr {
            TapIr::Move { source, .. }
            | TapIr::UnaryOp {
                operand: source, ..
            } => Self::One(Some(*source)),
            TapIr::StoreProp { value, .. } | TapIr::SetGlobal { value, .. } => match value {
                StoreValue::Symbol(sym) => Self::One(Some(*sym)),
                StoreValue::Immediate(_) => Self::None,
            },
            TapIr::BinOp { lhs, rhs, .. } => match rhs {
                Operand::Symbol(rhs) => Self::Two(Some(*lhs), Some(*rhs)),
                Operand::Immediate(_) | Operand::ShiftedImmediate(_) => Self::One(Some(*lhs)),
            },
            TapIr::Call { args, .. }
            | TapIr::CallExternal { args, .. }
            | TapIr::CallBuiltin { args, .. }
            | TapIr::Trigger { args, .. }
            | TapIr::Spawn { args, .. } => Self::Many(args.iter()),
            TapIr::Wait { frames: Some(f) } => Self::One(Some(*f)),
            TapIr::Wait { frames: None } => Self::None,
            _ => Self::None,
        }
    }

    pub fn new_target(instr: &'a TapIr) -> Self {
        match instr {
            TapIr::Constant(target, _)
            | TapIr::Move { target, .. }
            | TapIr::GetProp { target, .. }
            | TapIr::GetGlobal { target, .. }
            | TapIr::BinOp { target, .. }
            | TapIr::UnaryOp { target, .. }
            | TapIr::CallBuiltin { target, .. }
            | TapIr::Spawn { target, .. } => Self::One(Some(*target)),
            TapIr::Call { target, .. } | TapIr::CallExternal { target, .. } => {
                Self::Many(target.iter())
            }
            _ => Self::None,
        }
    }

    pub fn new_source_exit(instr: &'a BlockExitInstr) -> Self {
        match instr {
            BlockExitInstr::JumpToBlock(_) => Self::None,
            BlockExitInstr::ConditionalJump { test, .. } => Self::One(Some(*test)),
            BlockExitInstr::Return(symbol_ids) => Self::Many(symbol_ids.iter()),
        }
    }
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SymbolIter::None => None,
            SymbolIter::One(symbol_id) => symbol_id.take(),
            SymbolIter::Two(symbol_id1, symbol_id2) => {
                symbol_id1.take().or_else(|| symbol_id2.take())
            }
            SymbolIter::Many(iter) => iter.next().copied(),
        }
    }
}

pub(super) enum SymbolIterMut<'a> {
    None,
    One(Option<&'a mut SymbolId>),
    Two(Option<&'a mut SymbolId>, Option<&'a mut SymbolId>),
    Many(slice::IterMut<'a, SymbolId>),
}

impl<'a> SymbolIterMut<'a> {
    pub fn new_source(instr: &'a mut TapIr) -> Self {
        match instr {
            TapIr::Move { source, .. }
            | TapIr::UnaryOp {
                operand: source, ..
            } => Self::One(Some(source)),
            TapIr::StoreProp { value, .. } | TapIr::SetGlobal { value, .. } => match value {
                StoreValue::Symbol(sym) => Self::One(Some(sym)),
                StoreValue::Immediate(_) => Self::None,
            },
            TapIr::BinOp { lhs, rhs, .. } => match rhs {
                Operand::Symbol(rhs) => Self::Two(Some(lhs), Some(rhs)),
                Operand::Immediate(_) | Operand::ShiftedImmediate(_) => Self::One(Some(lhs)),
            },
            TapIr::Call { args, .. }
            | TapIr::CallExternal { args, .. }
            | TapIr::CallBuiltin { args, .. }
            | TapIr::Trigger { args, .. }
            | TapIr::Spawn { args, .. } => Self::Many(args.iter_mut()),
            TapIr::Wait { frames: Some(f) } => Self::One(Some(f)),
            TapIr::Wait { frames: None } => Self::None,
            _ => Self::None,
        }
    }

    pub fn new_target(instr: &'a mut TapIr) -> Self {
        match instr {
            TapIr::Constant(target, _)
            | TapIr::Move { target, .. }
            | TapIr::GetProp { target, .. }
            | TapIr::GetGlobal { target, .. }
            | TapIr::BinOp { target, .. }
            | TapIr::UnaryOp { target, .. }
            | TapIr::CallBuiltin { target, .. }
            | TapIr::Spawn { target, .. } => Self::One(Some(target)),
            TapIr::Call { target, .. } | TapIr::CallExternal { target, .. } => {
                Self::Many(target.iter_mut())
            }
            _ => Self::None,
        }
    }

    pub fn new_source_exit(instr: &'a mut BlockExitInstr) -> Self {
        match instr {
            BlockExitInstr::JumpToBlock(_) => Self::None,
            BlockExitInstr::ConditionalJump { test, .. } => Self::One(Some(test)),
            BlockExitInstr::Return(symbol_ids) => Self::Many(symbol_ids.iter_mut()),
        }
    }
}

impl<'a> Iterator for SymbolIterMut<'a> {
    type Item = &'a mut SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SymbolIterMut::None => None,
            SymbolIterMut::One(symbol_id) => symbol_id.take(),
            SymbolIterMut::Two(symbol_id1, symbol_id2) => {
                symbol_id1.take().or_else(|| symbol_id2.take())
            }
            SymbolIterMut::Many(many) => many.next(),
        }
    }
}
