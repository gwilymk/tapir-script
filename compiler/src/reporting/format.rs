use std::{
    collections::HashMap,
    io::{self, Write},
};

use ariadne::{Label, Source};

use crate::tokens::{FileId, LexicalErrorKind, Span};

use super::{CompilerErrorKind, Message, MessageKind, ParseError};

impl Message {
    pub fn write_diagnostic<W: Write>(
        &self,
        w: W,
        code: &mut DiagnosticCache,
        include_colour: bool,
    ) -> io::Result<()> {
        let report = match &*self.error {
            MessageKind::ParseError(parse_error) => parse_error_report(parse_error, self.span),
            MessageKind::LexerError(lexical_error_kind) => {
                lexical_error_report(lexical_error_kind, self.span)
            }
            MessageKind::ComplierError(compiler_error_kind) => {
                compiler_error_report(compiler_error_kind, self.span)
            }
        };

        report
            .with_config(ariadne::Config::default().with_color(include_colour))
            .finish()
            .write_for_stdout(code, w)
    }
}

pub struct DiagnosticCache {
    map: HashMap<FileId, (String, ariadne::Source<String>)>,
}

impl DiagnosticCache {
    pub fn new(files: impl Iterator<Item = (FileId, (String, String))>) -> Self {
        let map = files
            .into_iter()
            .map(|(id, (name, content))| (id, (name, Source::from(content))))
            .collect();
        Self { map }
    }
}

impl ariadne::Cache<FileId> for DiagnosticCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &FileId,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        match self.map.get(id) {
            Some((_, data)) => Ok(data),
            None => Err(Box::new(format!("Failed to find file with ID {id:?}"))),
        }
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let (filename, _) = self.map.get(id)?;
        Some(Box::new(filename.clone()))
    }
}

impl ariadne::Span for Span {
    type SourceId = FileId;

    fn source(&self) -> &Self::SourceId {
        &self.file_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

fn build_error_report(span: Span) -> ariadne::ReportBuilder<'static, Span> {
    ariadne::Report::build(ariadne::ReportKind::Error, span.file_id, 0)
}

fn parse_error_report(parse_error: &ParseError, span: Span) -> ariadne::ReportBuilder<'_, Span> {
    match parse_error {
        ParseError::UnrecognizedEof { expected } => build_error_report(span)
            .with_label(Label::new(span).with_message("End of file not expected here"))
            .with_message("Unexpected end of file")
            .with_note(format!("Expected one of tokens {}", expected.join(", "))),
        ParseError::UnrecognizedToken { token, expected } => build_error_report(span)
            .with_label(Label::new(span).with_message("Unexpected token"))
            .with_message(format!(
                "Unexpected token {token}, expected one of {}",
                expected.join(", ")
            )),
        ParseError::ExtraToken { token } => build_error_report(span)
            .with_label(Label::new(span).with_message("Extra token"))
            .with_message(format!("Unexpected extra token {token}")),
        ParseError::UnknownType { token } => build_error_report(span)
            .with_label(Label::new(span).with_message("Unknown type"))
            .with_message(format!(
                "'{token}' is not a valid type, must be one of fix, bool or int"
            )),
    }
}

fn lexical_error_report(
    lexical_error_kind: &LexicalErrorKind,
    span: Span,
) -> ariadne::ReportBuilder<'_, Span> {
    match lexical_error_kind {
        LexicalErrorKind::InvalidNumber(parse_int_error) => build_error_report(span)
            .with_label(Label::new(span).with_message("Invalid integer"))
            .with_message(format!("{parse_int_error}"))
            .with_note(match parse_int_error.kind() {
                std::num::IntErrorKind::PosOverflow => {
                    format!("Larger than maximum positive number which is {}", i32::MAX)
                }
                std::num::IntErrorKind::NegOverflow => format!(
                    "Smaller than minimum negative integer which is {}",
                    i32::MIN
                ),
                _ => String::default(),
            }),
        LexicalErrorKind::InvalidToken => {
            build_error_report(span).with_label(Label::new(span).with_message("Invalid token"))
        }
        LexicalErrorKind::InvalidFix => {
            build_error_report(span).with_label(Label::new(span).with_message("Invalid fixnum"))
        }
    }
}

fn compiler_error_report(
    compiler_error_kind: &CompilerErrorKind,
    span: Span,
) -> ariadne::ReportBuilder<'_, Span> {
    match compiler_error_kind {
        CompilerErrorKind::UnknownVariable(var) => build_error_report(span)
            .with_label(Label::new(span).with_message("Unknown variable"))
            .with_message(format!("Unknown variable '{var}'")),
        CompilerErrorKind::TypeError { expected, actual } => build_error_report(span)
            .with_label(Label::new(span).with_message("Incorrect type"))
            .with_message(format!(
                "Incorrect type, expected {expected} but got {actual}",
            )),
        CompilerErrorKind::UnknownType(var) => build_error_report(span)
            .with_label(Label::new(span).with_message("Unknown type for variable"))
            .with_message(format!("Unknown type for variable '{var}'")),
        CompilerErrorKind::BinaryOperatorTypeError { lhs_type, rhs_type } => {
            build_error_report(span)
                .with_label(Label::new(span).with_message("Mismatching types on binary operator"))
                .with_message(format!(
                    "Left hand side has type {lhs_type} but right hand side has type {rhs_type}"
                ))
        }
        CompilerErrorKind::InvalidTypeForBinaryOperator { type_ } => build_error_report(span)
            .with_label(Label::new(span).with_message("Binary operator cannot handle this type"))
            .with_message(format!("Binary operator cannot items of type {type_}")),
        CompilerErrorKind::InvalidTypeForIfCondition { got } => build_error_report(span)
            .with_label(Label::new(span).with_message(format!("This has type {got}")))
            .with_message(format!(
                "Condition in if statement must be a bool, but got a {got}"
            )),
        CompilerErrorKind::IncorrectNumberOfReturnTypes { expected, actual, function_return_location } => build_error_report(span)
            .with_label(Label::new(span).with_message(format!("This has {actual} return values")))
            .with_label(Label::new(*function_return_location).with_message(format!("Function returns {expected} values")))
            .with_message(format!("Function should be returning {expected} return values, but you are actually returning {actual}."))
            .with_note("Functions must return a fixed number of values"),
    }
}
