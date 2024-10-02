use std::{fs, iter};

use insta::{assert_ron_snapshot, assert_snapshot, glob};

use crate::{
    ast::{self, Expression},
    grammar,
    lexer::Lexer,
    tokens::FileId,
    DiagnosticCache, Message,
};

#[test]
fn snapshot_success() {
    glob!("snapshot_tests", "grammar/*.tapir", |path| {
        let input = fs::read_to_string(path).unwrap();

        let lexer = Lexer::new(&input, FileId::new(0));
        let parser = grammar::ScriptParser::new();

        let ast = parser.parse(FileId::new(0), lexer).unwrap();

        assert_ron_snapshot!(ast, {
            ".**.span" => "[span]",
        });
    });
}

#[test]
fn snapshot_failures() {
    glob!("snapshot_tests", "grammar/errors/*.tapir", |path| {
        let input = fs::read_to_string(path).unwrap();

        let file_id = FileId::new(0);

        let lexer = Lexer::new(&input, file_id);
        let parser = grammar::ScriptParser::new();

        let errors = match parser.parse(file_id, lexer) {
            Ok(ast) => collect_errors(&ast),
            Err(e) => vec![Message::from_lalrpop(e, file_id)],
        };

        let mut output = Vec::new();
        let mut diagnostics_cache = DiagnosticCache::new(iter::once((
            file_id,
            (path.to_string_lossy().to_string(), input.clone()),
        )));

        for error in errors {
            error
                .write_diagnostic(&mut output, &mut diagnostics_cache, false)
                .unwrap();
        }

        let error_str = String::from_utf8_lossy(&output);

        assert_snapshot!(error_str);
    });
}

fn collect_errors(statements: &[ast::Statement<'_>]) -> Vec<Message> {
    let mut errors = vec![];

    for statement in statements {
        match &statement.kind {
            ast::StatementKind::Error(message) => {
                errors.push(message.clone());
            }
            ast::StatementKind::VariableDeclaration { value, .. } => {
                gather_errors_in_expression(value, &mut errors);
            }
            ast::StatementKind::Assignment { value, .. } => {
                gather_errors_in_expression(value, &mut errors);
            }
            ast::StatementKind::Wait => {}
        }
    }

    fn gather_errors_in_expression(expression: &Expression<'_>, errors: &mut Vec<Message>) {
        match &expression.kind {
            ast::ExpressionKind::Integer(_) | ast::ExpressionKind::Variable(_) => {}
            ast::ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
                gather_errors_in_expression(lhs, errors);
                gather_errors_in_expression(rhs, errors);
            }
            ast::ExpressionKind::Error(message) => {
                errors.push(message.clone());
            }
        }
    }

    errors
}
