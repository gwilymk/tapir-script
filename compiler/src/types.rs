#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Fix,
    Bool,
}

pub struct FunctionType {
    pub args: Vec<Type>,
    pub rets: Vec<Type>,
}
