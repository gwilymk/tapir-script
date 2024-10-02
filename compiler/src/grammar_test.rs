use std::fs;

use insta::{assert_ron_snapshot, glob};

use crate::{grammar, lexer::Lexer, tokens::FileId};

#[test]
fn snapshot_tests() {
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
