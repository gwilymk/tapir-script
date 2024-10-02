#![deny(clippy::all)]
use lalrpop_util::lalrpop_mod;

mod ast;
mod lexer;
mod tokens;

lalrpop_mod!(grammar);
