use std::fs;

use insta::{assert_ron_snapshot, assert_snapshot, glob};

use crate::{grammar, lexer::Lexer, reporting::Diagnostics, tokens::FileId};

#[test]
fn snapshot_success() {
    glob!("snapshot_tests", "grammar/*.tapir", |path| {
        let input = fs::read_to_string(path).unwrap();

        let file_id = FileId::new(0);
        let lexer = Lexer::new(&input, file_id);
        let parser = grammar::ScriptParser::new();

        let mut diagnostics = Diagnostics::new(file_id, path, &input);

        let ast = parser
            .parse(FileId::new(0), &mut diagnostics, lexer)
            .unwrap();

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

        let mut diagnostics = Diagnostics::new(file_id, path, &input);

        match parser.parse(file_id, &mut diagnostics, lexer) {
            Ok(_) => {}
            Err(e) => {
                diagnostics.add_lalrpop(e, file_id);
            }
        }

        assert_snapshot!(diagnostics.pretty_string(false));
    });
}
