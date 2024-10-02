use lalrpop_util::lalrpop_mod;

mod ast;
mod tokens;

lalrpop_mod!(pub grammar);
