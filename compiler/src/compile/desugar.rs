use crate::ast::*;

mod spawn_block;

use spawn_block::SpawnBlockDesugarer;

/// A desugaring pass that transforms the AST before symbol resolution.
///
/// Implement this trait for each desugaring transformation. The default
/// methods recurse into children via `walk_*` free functions — override
/// only the methods relevant to your transformation.
trait DesugarPass<'input>: Sized {
    fn visit_expression(&mut self, expr: &mut Expression<'input>)
    where
        Self: Sized,
    {
        walk_expression(self, expr);
    }

    fn visit_statement(&mut self, stmt: &mut Statement<'input>)
    where
        Self: Sized,
    {
        walk_statement(self, stmt);
    }

    /// Called after traversal to inject any new top-level items (e.g. synthetic functions).
    fn finish(self, _script: &mut Script<'input>) {}
}

/// Recurse into the children of an expression, calling `pass.visit_expression()`
/// or `pass.visit_statement()` on each child. The mutual recursion with
/// `DesugarPass::visit_expression` (which calls `walk_expression` by default)
/// ensures the pass fires at every level.
fn walk_expression<'input>(pass: &mut impl DesugarPass<'input>, expr: &mut Expression<'input>) {
    match &mut expr.kind {
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            pass.visit_expression(lhs);
            pass.visit_expression(rhs);
        }
        ExpressionKind::UnaryOperation { operand, .. } => {
            pass.visit_expression(operand);
        }
        ExpressionKind::Call { arguments, .. } => {
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        ExpressionKind::Spawn { arguments, .. } => {
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        ExpressionKind::SpawnBlock { body } => {
            for stmt in body {
                pass.visit_statement(stmt);
            }
        }
        ExpressionKind::FieldAccess { base, .. } => {
            pass.visit_expression(base);
        }
        ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            pass.visit_expression(receiver);
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        // Leaves — nothing to recurse into
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Variable(_)
        | ExpressionKind::Nop
        | ExpressionKind::Error => {}
    }
}

/// Recurse into the children of a statement.
fn walk_statement<'input>(pass: &mut impl DesugarPass<'input>, stmt: &mut Statement<'input>) {
    match &mut stmt.kind {
        StatementKind::Expression { expression } => pass.visit_expression(expression),
        StatementKind::VariableDeclaration { values, .. } => {
            for val in values {
                pass.visit_expression(val);
            }
        }
        StatementKind::Assignment { targets, values } => {
            for target in targets.iter_mut() {
                pass.visit_expression(target);
            }
            for val in values {
                pass.visit_expression(val);
            }
        }
        StatementKind::Wait { frames } => {
            if let Some(expr) = frames {
                pass.visit_expression(expr);
            }
        }
        StatementKind::If {
            condition,
            true_block,
            false_block,
        } => {
            pass.visit_expression(condition);
            for s in true_block {
                pass.visit_statement(s);
            }
            for s in false_block {
                pass.visit_statement(s);
            }
        }
        StatementKind::Loop { block } | StatementKind::Block { block } => {
            for s in block {
                pass.visit_statement(s);
            }
        }
        StatementKind::Return { values } => {
            for val in values {
                pass.visit_expression(val);
            }
        }
        StatementKind::Trigger { arguments, .. } => {
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        StatementKind::Continue
        | StatementKind::Break
        | StatementKind::Nop
        | StatementKind::Error => {}
    }
}

/// Run a desugaring pass over a script. Walks all function bodies,
/// then calls `finish` to inject any synthetic items.
fn run_desugar_pass<'input>(mut pass: impl DesugarPass<'input>, script: &mut Script<'input>) {
    for function in &mut script.functions {
        for stmt in &mut function.statements {
            pass.visit_statement(stmt);
        }
    }
    pass.finish(script);
}

/// Run all desugaring passes over a script.
pub fn run(script: &mut Script<'_>) {
    run_desugar_pass(SpawnBlockDesugarer::new(), script);
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, glob};

    use crate::{grammar, lexer::Lexer, tokens::FileId};

    use super::*;

    #[test]
    fn desugar_snapshot_tests() {
        glob!("snapshot_tests", "desugar/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let file_id = FileId::new(0);
            let mut lexer = Lexer::new(&input, file_id);
            let parser = grammar::ScriptParser::new();

            let mut diagnostics =
                crate::reporting::Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser
                .parse(file_id, &mut diagnostics, lexer.iter())
                .unwrap();

            run(&mut script);

            assert_ron_snapshot!(script, {
                ".**.span" => "[span]",
                ".**.meta" => "[meta]",
            });
        });
    }
}
