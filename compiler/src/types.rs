use std::fmt::{self, Display};

use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default, Serialize)]
pub enum Type {
    Int,
    Fix,
    Bool,

    #[default]
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Fix => write!(f, "fix"),
            Type::Bool => write!(f, "bool"),
            Type::Error => write!(f, "unknown"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub args: Vec<Type>,
    pub rets: Vec<Type>,
}
