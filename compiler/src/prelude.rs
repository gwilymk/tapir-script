use std::path::Path;

use crate::{ast::Script, grammar, lexer::Lexer, reporting::Diagnostics, tokens::FileId};

const PRELUDE_SOURCE: &str = include_str!("../stdlib/prelude.tapir");
const PRELUDE_FILENAME: &str = "stdlib/prelude.tapir";

pub const PRELUDE_FILE_ID: FileId = FileId::new(0);
pub const USER_FILE_ID: FileId = FileId::new(1);

/// Parse user code with the prelude merged in.
///
/// When `enable_prelude` is true, the bundled prelude is parsed and merged with user code.
/// When `enable_prelude` is false, the user file is treated as the prelude itself
/// (useful when editing the prelude file directly).
///
/// Returns the merged AST on success, or None if parsing failed
/// (errors will be added to diagnostics).
pub fn parse_with_prelude<'input>(
    _user_filename: impl AsRef<Path>,
    user_input: &'input str,
    diagnostics: &mut Diagnostics,
    enable_prelude: bool,
) -> Option<Script<'input>> {
    let parser = grammar::ScriptParser::new();

    if enable_prelude {
        // Normal mode: parse bundled prelude and merge with user code
        diagnostics.add_file(PRELUDE_FILE_ID, PRELUDE_FILENAME, PRELUDE_SOURCE);

        // Parse prelude
        let mut prelude_lexer = Lexer::new(PRELUDE_SOURCE, PRELUDE_FILE_ID);
        let prelude_ast = match parser.parse(PRELUDE_FILE_ID, diagnostics, prelude_lexer.iter()) {
            Ok(ast) => ast,
            Err(e) => {
                diagnostics.add_lalrpop(e, PRELUDE_FILE_ID);
                return None;
            }
        };

        // Parse user code
        let mut user_lexer = Lexer::new(user_input, USER_FILE_ID);
        let mut user_ast = match parser.parse(USER_FILE_ID, diagnostics, user_lexer.iter()) {
            Ok(ast) => ast,
            Err(e) => {
                diagnostics.add_lalrpop(e, USER_FILE_ID);
                return None;
            }
        };

        // Merge prelude into user AST
        user_ast.merge_from(prelude_ast);
        Some(user_ast)
    } else {
        // Prelude mode: treat the user file as the prelude itself
        // Use PRELUDE_FILE_ID so builtin declarations are allowed
        let mut user_lexer = Lexer::new(user_input, PRELUDE_FILE_ID);
        match parser.parse(PRELUDE_FILE_ID, diagnostics, user_lexer.iter()) {
            Ok(ast) => Some(ast),
            Err(e) => {
                diagnostics.add_lalrpop(e, PRELUDE_FILE_ID);
                None
            }
        }
    }
}
