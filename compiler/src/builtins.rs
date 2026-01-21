use crate::{Type, ast::SymbolId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BuiltinVariable {
    Frame,
}

impl BuiltinVariable {
    const RESERVED_BIT: usize = 1 << (usize::BITS - 1);

    pub const fn symbol_id(self) -> SymbolId {
        SymbolId(Self::RESERVED_BIT | (self as usize))
    }

    pub fn from_symbol_id(id: SymbolId) -> Option<Self> {
        if id.0 & Self::RESERVED_BIT == 0 {
            return None;
        }

        match id.0 & !Self::RESERVED_BIT {
            0 => Some(BuiltinVariable::Frame),
            otherwise => unreachable!("Unknown builtin variable {otherwise}"),
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "frame" => Some(BuiltinVariable::Frame),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn name(self) -> &'static str {
        match self {
            BuiltinVariable::Frame => "frame",
        }
    }

    pub const fn ty(self) -> Type {
        match self {
            BuiltinVariable::Frame => Type::Int,
        }
    }

    pub const fn id(self) -> u8 {
        self as u8
    }
}
