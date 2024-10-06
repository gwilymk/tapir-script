use std::{fs, iter};

use insta::{assert_ron_snapshot, assert_snapshot, glob};

use crate::{
    ast::{Expression, ExpressionKind, Statement, StatementKind},
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
            Ok(ast) => {
                let mut error_visitor = ErrorCollector::default();
                error_visitor.visit(&ast);
                error_visitor.errors
            }
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

#[derive(Default)]
pub(crate) struct ErrorCollector {
    pub errors: Vec<Message>,
}

impl ErrorCollector {
    pub fn visit(&mut self, ast: &[Statement]) {
        for statement in ast {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Error(e) => {
                self.errors.push(e.clone());
            }
            StatementKind::VariableDeclaration { value: expr, .. }
            | StatementKind::Assignment { value: expr, .. }
            | StatementKind::SymbolDeclare { value: expr, .. }
            | StatementKind::SymbolAssign { value: expr, .. } => self.visit_expr(expr),
            StatementKind::Wait | StatementKind::Nop => {}
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::Integer(_)
            | ExpressionKind::Fix(_)
            | ExpressionKind::Variable(_)
            | ExpressionKind::Nop
            | ExpressionKind::Symbol(_) => {}
            ExpressionKind::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExpressionKind::Error(message) => self.errors.push(message.clone()),
        }
    }
}
