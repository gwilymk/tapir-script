#![deny(clippy::all)]
use lalrpop_util::lalrpop_mod;

use lexer::Lexer;
use tokens::FileId;

mod ast;
mod compile;
mod lexer;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use compile::{CompileSettings, Property};
pub use reporting::{format::DiagnosticCache, Message};
pub use types::Type;

pub fn compile(input: &str, compile_settings: CompileSettings) -> Result<Vec<u16>, Message> {
    let file_id = FileId::new(0);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let ast = parser
        .parse(file_id, lexer)
        .map_err(|e| Message::from_lalrpop(e, file_id))?;

    let bytecode = compile::compile(ast, &compile_settings)?;

    Ok(bytecode.compile())
}
