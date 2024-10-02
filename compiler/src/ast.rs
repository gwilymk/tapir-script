use crate::tokens::{FileId, LexicalError, LexicalErrorKind, Span};

use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub struct Statement<'input> {
    pub span: Span,
    pub kind: StatementKind<'input>,
}

#[derive(Clone, Debug, Serialize)]
pub enum StatementKind<'input> {
    VariableDeclaration {
        ident: &'input str,
        value: Box<Expression<'input>>,
    },
    Assignment {
        ident: &'input str,
        value: Box<Expression<'input>>,
    },
    Wait,
}

impl<'input> StatementKind<'input> {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Statement<'input> {
        Statement {
            span: Span::new(file_id, start, end),
            kind: self,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Expression<'input> {
    pub span: Span,
    pub kind: ExpressionKind<'input>,
}

impl<'input> From<LexicalError> for Box<Expression<'input>> {
    fn from(value: LexicalError) -> Self {
        Box::new(Expression {
            span: value.span,
            kind: ExpressionKind::Error(value.kind),
        })
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum ExpressionKind<'input> {
    Integer(i32),
    Variable(&'input str),
    BinaryOperation {
        lhs: Box<Expression<'input>>,
        operator: BinaryOperator,
        rhs: Box<Expression<'input>>,
    },
    Error(LexicalErrorKind),
}

impl<'input> ExpressionKind<'input> {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Box<Expression<'input>> {
        Box::new(Expression {
            kind: self,
            span: Span::new(file_id, start, end),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    RealDiv,
    RealMod,
}
