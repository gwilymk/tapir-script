use std::num::ParseIntError;

use logos::Logos;
use num_traits::ParseFloatError;
use serde::Serialize;

#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct FileId(usize);

impl FileId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub(crate) file_id: FileId,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Span {
    pub fn new(file_id: FileId, start: usize, end: usize) -> Self {
        assert!(start <= end, "{} was not <= {}", start, end);
        Self {
            file_id,
            start,
            end,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct LexicalError {
    pub kind: LexicalErrorKind,
    pub span: Span,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize)]
#[allow(clippy::enum_variant_names)]
pub enum LexicalErrorKind {
    InvalidNumber(#[serde(skip)] ParseIntError),
    InvalidFix,
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalErrorKind {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidNumber(value)
    }
}

impl From<ParseFloatError> for LexicalErrorKind {
    fn from(_: ParseFloatError) -> Self {
        Self::InvalidFix
    }
}

impl LexicalErrorKind {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> LexicalError {
        LexicalError {
            kind: self,
            span: Span::new(file_id, start, end),
        }
    }
}

#[derive(Logos, Clone, Debug, PartialEq, Serialize)]
#[logos(skip r"[ \t\n\f\r]+", skip r"#.*\n?", error = LexicalErrorKind)]
pub enum Token<'input> {
    #[token("wait")]
    KeywordWait,
    #[token("var")]
    KeywordVar,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice())]
    Identifier(&'input str),
    #[regex("-?[0-9]+", |lex| lex.slice())]
    Integer(&'input str),
    #[regex("-?[0-9]+\\.[0-9]*", |lex| lex.slice())]
    Fix(&'input str),

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("%")]
    OperatorMod,
    #[token("//")]
    OperatorRealDiv,
    #[token("%%")]
    OperatorRealMod,
    #[token("==")]
    OperatorEqEq,
}
