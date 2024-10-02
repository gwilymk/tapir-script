use std::{fs, iter};

use insta::{assert_ron_snapshot, assert_snapshot, glob};

use crate::{
    ast::{ExpressionKind, StatementKind, VResult, Visitable, Visitor},
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
            Ok(mut ast) => {
                let mut error_visitor = ErrorVisitor::default();
                ast.visit(&mut error_visitor).unwrap();
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
pub(crate) struct ErrorVisitor {
    pub errors: Vec<Message>,
}

impl Visitor<()> for ErrorVisitor {
    fn visit_statement_error<'input>(&mut self, error: Message) -> VResult<StatementKind<'input>> {
        self.errors.push(error.clone());
        Ok(StatementKind::Error(error))
    }

    fn visit_expression_error<'input>(
        &mut self,
        error: Message,
    ) -> VResult<ExpressionKind<'input>> {
        self.errors.push(error.clone());
        Ok(ExpressionKind::Error(error))
    }
}
