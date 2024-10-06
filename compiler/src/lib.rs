#![deny(clippy::all)]
use compile::CompileSettings;
use lalrpop_util::lalrpop_mod;

mod ast;
mod compile;
mod lexer;
mod reporting;
mod tokens;
mod types;

lalrpop_mod!(grammar);

#[cfg(test)]
mod grammar_test;

pub use reporting::{format::DiagnosticCache, Message};

use lexer::Lexer;
use tokens::FileId;

pub fn compile(
    input: &str,
    fname: &str,
    compile_settings: CompileSettings,
) -> Result<Vec<u8>, Message> {
    let file_id = FileId::new(0);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let ast = parser
        .parse(file_id, lexer)
        .map_err(|e| Message::from_lalrpop(e, file_id))?;

    let bytecode = compile::compile(ast, &compile_settings)?;

    Ok(bytecode.compile())
}
