use std::mem;

use crate::{
    tokens::{FileId, LexicalError, Span},
    Message,
};

use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub struct Statement<'input> {
    pub span: Span,
    pub kind: StatementKind<'input>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub enum StatementKind<'input> {
    Error(Message),
    VariableDeclaration {
        ident: &'input str,
        value: Box<Expression<'input>>,
    },
    Assignment {
        ident: &'input str,
        value: Box<Expression<'input>>,
    },
    Wait,
    #[default]
    Nop,
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
            kind: ExpressionKind::Error(Message::from(value)),
        })
    }
}

#[derive(Clone, Default, Debug, Serialize)]
pub enum ExpressionKind<'input> {
    Integer(i32),
    Variable(&'input str),
    BinaryOperation {
        lhs: Box<Expression<'input>>,
        operator: BinaryOperator,
        rhs: Box<Expression<'input>>,
    },
    Error(Message),
    #[default]
    Nop,
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

pub type VResult<T> = Result<T, Message>;

pub trait Visitable<T> {
    fn visit(&mut self, v: &mut dyn Visitor<T>) -> VResult<T>;
}

pub trait Visitor<T>
where
    T: Default,
{
    fn start_statement(&mut self, _statement: &mut Statement<'_>) -> VResult<()> {
        Ok(())
    }

    fn visit_statement_error<'input>(&mut self, error: Message) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::Error(error))
    }

    fn visit_variable_declaration<'input>(
        &mut self,
        ident: &'input str,
        value: Box<Expression<'input>>,
    ) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::VariableDeclaration { ident, value })
    }

    fn visit_assignment<'input>(
        &mut self,
        ident: &'input str,
        value: Box<Expression<'input>>,
    ) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::Assignment { ident, value })
    }

    fn visit_wait<'input>(&mut self) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::Wait)
    }

    fn end_statement(&mut self, _statement: &mut Statement<'_>) -> VResult<T> {
        Ok(T::default())
    }

    fn start_expression(&mut self, _expression: &mut Expression<'_>) -> VResult<()> {
        Ok(())
    }

    fn end_expression(&mut self, _expression: &mut Expression<'_>) -> VResult<T> {
        Ok(T::default())
    }

    fn visit_integer<'input>(&mut self, i: i32) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Integer(i))
    }

    fn visit_variable<'input>(&mut self, ident: &'input str) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Variable(ident))
    }

    fn visit_binop<'input>(
        &mut self,
        mut lhs: Box<Expression<'input>>,
        operator: BinaryOperator,
        mut rhs: Box<Expression<'input>>,
    ) -> VResult<ExpressionKind<'input>> {
        lhs.visit(self)?;
        rhs.visit(self)?;

        Ok(ExpressionKind::BinaryOperation { lhs, operator, rhs })
    }

    fn visit_expression_error<'input>(
        &mut self,
        error: Message,
    ) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Error(error))
    }
}

impl<'input, T> Visitable<T> for Statement<'input>
where
    T: Default,
{
    fn visit(&mut self, v: &mut dyn Visitor<T>) -> VResult<T> {
        v.start_statement(self)?;

        self.kind = match mem::take(&mut self.kind) {
            StatementKind::Error(message) => v.visit_statement_error(message)?,
            StatementKind::VariableDeclaration { ident, value } => {
                v.visit_variable_declaration(ident, value)?
            }
            StatementKind::Assignment { ident, value } => v.visit_assignment(ident, value)?,
            StatementKind::Wait => v.visit_wait()?,
            StatementKind::Nop => StatementKind::Nop,
        };

        v.end_statement(self)
    }
}

impl<'input, T> Visitable<T> for Vec<Statement<'input>>
where
    T: Default,
{
    fn visit(&mut self, v: &mut dyn Visitor<T>) -> VResult<T> {
        for statement in self {
            statement.visit(v)?;
        }

        Ok(T::default())
    }
}

impl<'input, T> Visitable<T> for Expression<'input>
where
    T: Default,
{
    fn visit(&mut self, v: &mut dyn Visitor<T>) -> VResult<T> {
        v.start_expression(self)?;

        self.kind = match mem::take(&mut self.kind) {
            ExpressionKind::Integer(i) => v.visit_integer(i)?,
            ExpressionKind::Variable(ident) => v.visit_variable(ident)?,
            ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                v.visit_binop(lhs, operator, rhs)?
            }
            ExpressionKind::Error(message) => v.visit_expression_error(message)?,
            ExpressionKind::Nop => ExpressionKind::Nop,
        };

        v.end_expression(self)
    }
}
