use serde::Serialize;

use crate::{
    tokens::{self, FileId, LexicalError, LexicalErrorKind, Span},
    types::Type,
};

pub(crate) mod format;

#[derive(Clone, Debug, Serialize)]
pub struct Message {
    pub span: Span,
    pub error: Box<MessageKind>,
}

impl Message {
    pub fn with_new_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

impl From<LexicalError> for Message {
    fn from(value: LexicalError) -> Self {
        Self {
            span: value.span,
            error: Box::new(MessageKind::LexerError(value.kind)),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
#[allow(clippy::enum_variant_names)]
pub enum MessageKind {
    ParseError(ParseError),
    LexerError(LexicalErrorKind),
    ComplierError(CompilerErrorKind),
}

impl MessageKind {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Message {
        Message {
            span: Span::new(file_id, start, end),
            error: Box::new(self),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum ParseError {
    UnrecognizedEof {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        token: String,
    },
}

impl Message {
    pub fn from_lalrpop(
        value: lalrpop_util::ParseError<usize, tokens::Token<'_>, LexicalError>,
        file_id: FileId,
    ) -> Self {
        match value {
            lalrpop_util::ParseError::InvalidToken { location } => LexicalErrorKind::InvalidToken
                .with_span(file_id, location, location)
                .into(),
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                MessageKind::ParseError(ParseError::UnrecognizedEof { expected })
                    .with_span(file_id, location, location)
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                MessageKind::ParseError(ParseError::UnrecognizedToken {
                    token: format!("{:?}", token.1),
                    expected,
                })
                .with_span(file_id, token.0, token.2)
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                MessageKind::ParseError(ParseError::ExtraToken {
                    token: format!("{:?}", token.1),
                })
                .with_span(file_id, token.0, token.2)
            }
            lalrpop_util::ParseError::User { error } => error.into(),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum CompilerErrorKind {
    UnknownVariable(String),
    TypeError { expected: Type, actual: Type },
    UnknownType(String),
    BinaryOperatorTypeError { lhs_type: Type, rhs_type: Type },
    InvalidTypeForBinaryOperator { type_: Type },
}

impl CompilerErrorKind {
    pub fn into_message(self, span: Span) -> Message {
        Message {
            span,
            error: Box::new(MessageKind::ComplierError(self)),
        }
    }
}
