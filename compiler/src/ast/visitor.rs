use std::mem;

use crate::{tokens::Span, Message};

use super::{BinaryOperator, Expression, ExpressionKind, Statement, StatementKind, SymbolId};

pub type VResult<T> = Result<T, Message>;

pub trait Visitable<T: Default> {
    fn visit(&mut self, v: &mut impl Visitor<T>) -> VResult<T>;
}

pub trait Visitor<T>
where
    T: Default,
    Self: Sized,
{
    fn visit_statement_error<'input>(&mut self, error: Message) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::Error(error))
    }

    fn visit_variable_declaration<'input>(
        &mut self,
        ident: &'input str,
        mut value: Box<Expression<'input>>,
        _span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;

        Ok(StatementKind::VariableDeclaration { ident, value })
    }

    fn visit_assignment<'input>(
        &mut self,
        ident: &'input str,
        mut value: Box<Expression<'input>>,
        _span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;

        Ok(StatementKind::Assignment { ident, value })
    }

    fn visit_wait<'input>(&mut self, _span: Span) -> VResult<StatementKind<'input>> {
        Ok(StatementKind::Wait)
    }

    fn visit_integer<'input>(&mut self, i: i32, _span: Span) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Integer(i))
    }

    fn visit_variable<'input>(
        &mut self,
        ident: &'input str,
        _span: Span,
    ) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Variable(ident))
    }

    fn visit_binop<'input>(
        &mut self,
        mut lhs: Box<Expression<'input>>,
        operator: BinaryOperator,
        mut rhs: Box<Expression<'input>>,
        _span: Span,
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

    fn visit_symbol_assign<'input>(
        &mut self,
        ident: SymbolId,
        mut value: Box<Expression<'input>>,
        _span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;
        Ok(StatementKind::SymbolAssign { ident, value })
    }

    fn visit_symbol_declare<'input>(
        &mut self,
        ident: SymbolId,
        mut value: Box<Expression<'input>>,
        _span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;
        Ok(StatementKind::SymbolDeclare { ident, value })
    }

    fn visit_symbol<'input>(
        &mut self,
        ident: SymbolId,
        _span: Span,
    ) -> VResult<ExpressionKind<'input>> {
        Ok(ExpressionKind::Symbol(ident))
    }
}

impl<'input, T> Visitable<T> for Statement<'input>
where
    T: Default,
{
    fn visit(&mut self, v: &mut impl Visitor<T>) -> VResult<T> {
        self.kind = match mem::take(&mut self.kind) {
            StatementKind::Error(message) => v.visit_statement_error(message)?,
            StatementKind::VariableDeclaration { ident, value } => {
                v.visit_variable_declaration(ident, value, self.span)?
            }
            StatementKind::Assignment { ident, value } => {
                v.visit_assignment(ident, value, self.span)?
            }
            StatementKind::Wait => v.visit_wait(self.span)?,
            StatementKind::Nop => StatementKind::Nop,

            StatementKind::SymbolAssign { ident, value } => {
                v.visit_symbol_assign(ident, value, self.span)?
            }
            StatementKind::SymbolDeclare { ident, value } => {
                v.visit_symbol_declare(ident, value, self.span)?
            }
        };

        Ok(T::default())
    }
}

impl<'input, T> Visitable<T> for Vec<Statement<'input>>
where
    T: Default,
{
    fn visit(&mut self, v: &mut impl Visitor<T>) -> VResult<T> {
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
    fn visit(&mut self, v: &mut impl Visitor<T>) -> VResult<T> {
        self.kind = match mem::take(&mut self.kind) {
            ExpressionKind::Integer(i) => v.visit_integer(i, self.span)?,
            ExpressionKind::Variable(ident) => v.visit_variable(ident, self.span)?,
            ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                v.visit_binop(lhs, operator, rhs, self.span)?
            }
            ExpressionKind::Error(message) => v.visit_expression_error(message)?,
            ExpressionKind::Nop => ExpressionKind::Nop,

            ExpressionKind::Symbol(symbol_id) => v.visit_symbol(symbol_id, self.span)?,
        };

        Ok(T::default())
    }
}
