use std::{fs, iter};

use insta::{assert_ron_snapshot, assert_snapshot, glob};

use crate::{grammar, lexer::Lexer, reporting::Diagnostics, tokens::FileId, DiagnosticCache};

#[test]
fn snapshot_success() {
    glob!("snapshot_tests", "grammar/*.tapir", |path| {
        let input = fs::read_to_string(path).unwrap();

        let lexer = Lexer::new(&input, FileId::new(0));
        let parser = grammar::ScriptParser::new();

        let mut diagnostics = Diagnostics::new();

        let ast = parser
            .parse(FileId::new(0), &mut diagnostics, lexer)
            .unwrap();

        assert!(!diagnostics.has_any());

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

        let mut diagnostics = Diagnostics::new();

        match parser.parse(file_id, &mut diagnostics, lexer) {
            Ok(_) => {}
            Err(e) => {
                diagnostics.add_lalrpop(e, file_id);
            }
        }

        let mut output = Vec::new();
        let mut diagnostics_cache = DiagnosticCache::new(iter::once((
            file_id,
            (path.to_string_lossy().to_string(), input.clone()),
        )));

        diagnostics
            .write(&mut output, &mut diagnostics_cache, false)
            .unwrap();

        let error_str = String::from_utf8_lossy(&output);

        assert_snapshot!(error_str);
    });
}
