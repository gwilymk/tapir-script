use std::fmt::{Display, Write};

use super::{Expression, ExpressionKind, Function, Statement, StatementKind};

pub(super) fn pretty_print(function: &Function, output: &mut dyn Write) -> std::fmt::Result {
    if function.name == "@toplevel" {
        if !function.meta.is_empty() {
            writeln!(output, "# @toplevel: {:?}\n", function.meta)?;
        }
        pretty_print_statements(&function.statements, output, Indent(0))?;
    } else {
        writeln!(output)?;
        if !function.meta.is_empty() {
            writeln!(output, "# {:?}", function.meta)?;
        }

        write!(
            output,
            "{}fn {}(",
            if function.modifiers.is_event_handler.is_some() {
                "event "
            } else {
                ""
            },
            function.name
        )?;

        for argument in &function.arguments {
            write!(output, "{},", argument.t.t)?;
        }

        write!(output, ")")?;

        if !function.return_types.types.is_empty() {
            write!(output, " -> (")?;
            for return_type in &function.return_types.types {
                write!(output, "{},", return_type.t)?;
            }
            write!(output, ")")?;
        }

        writeln!(output, " {{")?;

        pretty_print_statements(&function.statements, output, Indent(1))?;
        writeln!(output, "}}")?;
    }

    Ok(())
}

fn pretty_print_statements(
    statements: &[Statement],
    output: &mut dyn Write,
    indent: Indent,
) -> std::fmt::Result {
    for statement in statements {
        if statement.meta.is_empty() {
        } else {
            writeln!(output, "{indent}# {:?}", statement.meta)?;
        }

        write!(output, "{indent}")?;
        match &statement.kind {
            StatementKind::Error => write!(output, "ERROR;")?,
            StatementKind::VariableDeclaration { ident, value } => {
                write!(output, "var {ident} = ")?;
                pretty_print_expr(value, output, indent.increase())?;
                write!(output, ";")?;
            }
            StatementKind::Assignment { ident, value } => {
                write!(output, "{ident} = ")?;
                pretty_print_expr(value, output, indent.increase())?;
                write!(output, ";")?;
            }
            StatementKind::Wait => write!(output, "wait;")?,
            StatementKind::Block { block } => {
                writeln!(output, "{{")?;
                pretty_print_statements(block, output, indent.increase())?;
                write!(output, "{indent}}}")?;
            }
            StatementKind::Continue => write!(output, "continue;")?,
            StatementKind::Break => write!(output, "break;")?,
            StatementKind::Nop => write!(output, "# nop")?,
            StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                write!(output, "if ")?;
                pretty_print_expr(condition, output, indent.increase())?;
                writeln!(output, " {{")?;
                pretty_print_statements(true_block, output, indent.increase())?;

                if !false_block.is_empty() {
                    writeln!(output, "{indent}}} else {{")?;
                    pretty_print_statements(false_block, output, indent.increase())?;
                }
                write!(output, "{indent}}}")?;
            }
            StatementKind::Loop { block } => {
                writeln!(output, "loop {{")?;
                pretty_print_statements(block, output, indent.increase())?;
                write!(output, "{indent}}}")?;
            }
            StatementKind::Call { name, arguments } => {
                write!(output, "{name}(")?;
                for argument in arguments {
                    pretty_print_expr(argument, output, indent.increase())?;
                    write!(output, ",")?;
                }
                write!(output, ");")?;
            }
            StatementKind::Spawn { name, arguments } => {
                write!(output, "spawn {name}(")?;
                for argument in arguments {
                    pretty_print_expr(argument, output, indent.increase())?;
                    write!(output, ",")?;
                }
                write!(output, ");")?;
            }
            StatementKind::Return { values } => {
                write!(output, "return (")?;
                for value in values {
                    pretty_print_expr(value, output, indent.increase())?;
                    write!(output, ",")?;
                }
                write!(output, ");")?;
            }
            StatementKind::Trigger { name, arguments } => {
                write!(output, "trigger {name}(")?;
                for arg in arguments {
                    pretty_print_expr(arg, output, indent.increase())?;
                    write!(output, ",")?;
                }
                write!(output, ");")?;
            }
        }

        writeln!(output)?;
    }

    Ok(())
}

fn pretty_print_expr(
    expression: &Expression,
    output: &mut dyn Write,
    mut indent: Indent,
) -> std::fmt::Result {
    if !expression.meta.is_empty() {
        indent = indent.increase();
        write!(output, "\n{indent}")?;
    }

    match &expression.kind {
        ExpressionKind::Integer(int) => write!(output, "{int}")?,
        ExpressionKind::Fix(num) => write!(output, "{num}")?,
        ExpressionKind::Bool(boool) => write!(output, "{boool}")?,
        ExpressionKind::Variable(variable) => write!(output, "{variable}")?,
        ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
            write!(output, "(")?;
            pretty_print_expr(lhs, output, indent)?;
            write!(
                output,
                " {} ",
                match operator {
                    super::BinaryOperator::Add => "+",
                    super::BinaryOperator::Sub => "-",
                    super::BinaryOperator::Mul => "*",
                    super::BinaryOperator::Div => "/",
                    super::BinaryOperator::Mod => "%",
                    super::BinaryOperator::RealDiv => "//",
                    super::BinaryOperator::RealMod => "%%",
                    super::BinaryOperator::FixMul => "*",
                    super::BinaryOperator::FixDiv => "/",
                    super::BinaryOperator::EqEq => "==",
                    super::BinaryOperator::NeEq => "!=",
                    super::BinaryOperator::Gt => ">",
                    super::BinaryOperator::GtEq => ">=",
                    super::BinaryOperator::Lt => "<",
                    super::BinaryOperator::LtEq => "<=",
                    super::BinaryOperator::Then => "then",
                }
            )?;
            pretty_print_expr(rhs, output, indent)?;
            write!(output, ")")?;
        }
        ExpressionKind::Error => write!(output, "ERROR")?,
        ExpressionKind::Nop => write!(output, "NOP")?,
        ExpressionKind::Call { name, arguments } => {
            write!(output, "{name}(")?;
            for argument in arguments {
                pretty_print_expr(argument, output, indent.increase())?;
                write!(output, ",")?;
            }
            write!(output, ")")?;
        }
    }

    if !expression.meta.is_empty() {
        write!(output, " # {:?}\n{indent}", expression.meta)?;
    }

    Ok(())
}

#[derive(Clone, Copy)]
struct Indent(usize);
impl Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "    ")?
        }

        Ok(())
    }
}

impl Indent {
    pub fn increase(self) -> Self {
        Self(self.0 + 1)
    }
}
