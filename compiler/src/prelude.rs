use std::path::Path;

use crate::{ast::Script, grammar, lexer::Lexer, reporting::Diagnostics, tokens::FileId};

const PRELUDE_SOURCE: &str = include_str!("../stdlib/prelude.tapir");
const PRELUDE_FILENAME: &str = "stdlib/prelude.tapir";

pub const PRELUDE_FILE_ID: FileId = FileId::new(0);
pub const USER_FILE_ID: FileId = FileId::new(1);

/// Parse user code with the prelude merged in.
///
/// Returns the merged AST on success, or None if parsing failed
/// (errors will be added to diagnostics).
pub fn parse_with_prelude<'input>(
    _user_filename: impl AsRef<Path>,
    user_input: &'input str,
    diagnostics: &mut Diagnostics,
) -> Option<Script<'input>> {
    // Add prelude source to diagnostics cache for error reporting
    diagnostics.add_file(PRELUDE_FILE_ID, PRELUDE_FILENAME, PRELUDE_SOURCE);

    let parser = grammar::ScriptParser::new();

    // Parse prelude
    let prelude_lexer = Lexer::new(PRELUDE_SOURCE, PRELUDE_FILE_ID);
    let prelude_ast = match parser.parse(PRELUDE_FILE_ID, diagnostics, prelude_lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, PRELUDE_FILE_ID);
            return None;
        }
    };

    // Parse user code
    let user_lexer = Lexer::new(user_input, USER_FILE_ID);
    let mut user_ast = match parser.parse(USER_FILE_ID, diagnostics, user_lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, USER_FILE_ID);
            return None;
        }
    };

    // Merge prelude into user AST
    user_ast.merge_from(prelude_ast);

    Some(user_ast)
}
