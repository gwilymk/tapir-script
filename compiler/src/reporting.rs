use std::path::Path;

use serde::Serialize;

use crate::{
    DiagnosticCache,
    tokens::{self, FileId, LexicalError, LexicalErrorKind, Span},
    types::Type,
};

pub(crate) mod format;

#[derive(Debug, Clone)]
pub struct Diagnostics {
    messages: Vec<Message>,
    cache: DiagnosticCache,
}

impl Diagnostics {
    pub fn new(file_id: FileId, filename: impl AsRef<Path>, content: &str) -> Self {
        Self {
            messages: vec![],
            cache: DiagnosticCache::new(file_id, filename, content),
        }
    }

    pub fn add_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    pub fn add_lalrpop(
        &mut self,
        value: lalrpop_util::ParseError<usize, tokens::Token<'_>, LexicalError>,
        file_id: FileId,
    ) {
        self.add_message(Message::from_lalrpop(value, file_id));
    }

    pub fn pretty_string(&mut self, colourful: bool) -> String {
        self.string_with_optional_colour(colourful)
    }

    fn string_with_optional_colour(&mut self, colourful: bool) -> String {
        let mut output = Vec::new();

        for message in &self.messages {
            message
                .write_diagnostic(&mut output, &mut self.cache, colourful)
                .unwrap();
        }

        String::from_utf8_lossy(&output).into_owned()
    }

    pub fn has_any(&self) -> bool {
        !self.messages.is_empty()
    }
}

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
        expected: Box<[String]>,
    },
    UnrecognizedToken {
        token: String,
        expected: Box<[String]>,
    },
    ExtraToken {
        token: String,
    },
    UnknownType {
        token: String,
    },
    ExternFunctionWithBlock {
        name: String,
    },
}

impl ParseError {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Message {
        MessageKind::ParseError(self).with_span(file_id, start, end)
    }
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
                ParseError::UnrecognizedEof {
                    expected: expected.into_boxed_slice(),
                }
                .with_span(file_id, location, location)
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseError::UnrecognizedToken {
                    token: format!("{:?}", token.1),
                    expected: expected.into_boxed_slice(),
                }
                .with_span(file_id, token.0, token.2)
            }
            lalrpop_util::ParseError::ExtraToken { token } => ParseError::ExtraToken {
                token: format!("{:?}", token.1),
            }
            .with_span(file_id, token.0, token.2),
            lalrpop_util::ParseError::User { error } => error.into(),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum CompilerErrorKind {
    UnknownVariable(String),
    TypeError {
        expected: Type,
        expected_span: Option<Span>,
        actual: Type,
        actual_span: Span,
    },
    PropertyTypeError {
        property_name: String,
        expected: Type,
        actual: Type,
        actual_span: Span,
    },
    FunctionArgumentTypeError {
        function_name: String,
        argument_name: String,
        expected: Type,
        expected_span: Span,
        actual: Type,
        actual_span: Span,
    },
    UnknownType(String),
    BinaryOperatorTypeError {
        lhs_type: Type,
        rhs_type: Type,
    },
    InvalidTypeForBinaryOperator {
        type_: Type,
    },
    InvalidTypeForIfCondition {
        got: Type,
    },
    IncorrectNumberOfReturnTypes {
        expected: usize,
        actual: usize,
        function_return_location: Span,
    },
    MismatchingReturnTypes {
        expected: Type,
        actual: Type,
        expected_location: Span,
        actual_location: Span,
    },
    FunctionAlreadyDeclared {
        function_name: String,
        old_function_declaration: Span,
        new_function_declaration: Span,
    },
    UnknownFunction {
        name: String,
    },
    IncorrectNumberOfArguments {
        function_name: String,
        expected: usize,
        actual: usize,
        function_span: Span,
    },
    FunctionMustReturnOneValueInThisLocation {
        actual: usize,
    },
    FunctionDoesNotHaveReturn {
        name: String,
        return_location: Span,
    },
    BreakOrContinueOutsideOfLoop,
    DivideByZero,
    EventFunctionsShouldNotHaveAReturnType {
        return_type_span: Span,
        event_span: Span,
        function_name: String,
    },
    CannotCallEventHandler {
        function_span: Span,
        function_name: String,
    },
    TriggerIncorrectArgs {
        name: String,
        first_definition_span: Span,
        first_definition_args: Box<[Type]>,
        second_definition_args: Box<[Type]>,
    },
    CountMismatch {
        ident_count: usize,
        expr_count: usize,
        extras: CountMismatchExtras,
    },
    CannotShadowBuiltin {
        name: String,
    },
}

#[derive(Clone, Debug, Serialize)]
pub enum CountMismatchExtras {
    Idents(Vec<Span>),
    Expressions(Vec<Span>),
}

impl CompilerErrorKind {
    pub fn into_message(self, span: Span) -> Message {
        Message {
            span,
            error: Box::new(MessageKind::ComplierError(self)),
        }
    }
}
