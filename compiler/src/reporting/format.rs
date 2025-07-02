use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    io::{self, Write},
    iter,
    path::Path,
};

use ariadne::{Cache, Label, Source};

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

#[derive(Clone)]
pub struct DiagnosticCache {
    map: HashMap<FileId, (String, ariadne::Source<String>)>,
}

impl fmt::Debug for DiagnosticCache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (file_id, (filename, _)) in &self.map {
            write!(f, "({file_id:?} -> {filename})")?;
        }

        Ok(())
    }
}

impl DiagnosticCache {
    pub fn new(
        toplevel_file_id: FileId,
        toplevel_filename: impl AsRef<Path>,
        toplevel_content: &str,
    ) -> Self {
        let map = HashMap::from_iter(iter::once((
            toplevel_file_id,
            (
                toplevel_filename.as_ref().to_string_lossy().into_owned(),
                Source::from(toplevel_content.to_string()),
            ),
        )));

        Self { map }
    }
}

impl ariadne::Cache<FileId> for DiagnosticCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &FileId,
    ) -> Result<&ariadne::Source<<Self as Cache<FileId>>::Storage>, impl Debug> {
        match self.map.get(id) {
            Some((_, data)) => Ok(data),
            None => Err(Box::new(format!("Failed to find file with ID {id:?}"))),
        }
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<impl Display + 'a> {
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
    ariadne::Report::build(ariadne::ReportKind::Error, span)
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
        ParseError::ExternFunctionWithBlock { name } => build_error_report(span)
            .with_label(Label::new(span).with_message("extern function cannot have body"))
            .with_message(format!("extern function '{name}' cannot have a body")),
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
        CompilerErrorKind::MismatchingReturnTypes { expected, actual, expected_location, actual_location } => build_error_report(span)
            .with_label(Label::new(*actual_location).with_message(format!("This has type {actual}")))
            .with_label(Label::new(*expected_location).with_message(format!("This has type {expected}")))
            .with_message(format!("Function is declared to return type {expected} but got {actual}")),
        CompilerErrorKind::FunctionAlreadyDeclared { function_name, old_function_declaration, new_function_declaration } => build_error_report(span)
            .with_label(Label::new(*old_function_declaration).with_message("Originally declared here"))
            .with_label(Label::new(*new_function_declaration).with_message("Also declared here"))
            .with_message(format!("Function with name '{function_name}' already exists")),
        CompilerErrorKind::UnknownFunction { name } => build_error_report(span)
            .with_label(Label::new(span).with_message("Unknown function"))
            .with_message(format!("No such function {name}")),
        CompilerErrorKind::IncorrectNumberOfArguments { expected, actual, function_span, function_name } => build_error_report(span)
            .with_label(Label::new(span).with_message(format!("Got {actual} arguments")))
            .with_label(Label::new(*function_span).with_message(format!("Expected {expected} arguments")))
            .with_message(format!("Incorrect number of argumets for function {function_name}, expected {expected} arguments but got {actual}.")),
        CompilerErrorKind::FunctionMustReturnOneValueInThisLocation { actual } => build_error_report(span)
            .with_label(Label::new(span).with_message("Function must return 1 value here"))
            .with_message(format!("Function call must return exactly 1 value here, but got {actual}")),
        CompilerErrorKind::FunctionDoesNotHaveReturn { name, return_location } => build_error_report(span)
            .with_label(Label::new(*return_location).with_message("Function returns results"))
            .with_message(format!("Function {name} should return results, but not all branches return.")),
        CompilerErrorKind::BreakOrContinueOutsideOfLoop => build_error_report(span)
            .with_label(Label::new(span).with_message("This statement"))
            .with_message("`break` or `continue` must be within a loop"),
        CompilerErrorKind::DivideByZero => build_error_report(span)
            .with_label(Label::new(span).with_message("This reduces to 0"))
            .with_message("Divide by zero not allowed"),
        CompilerErrorKind::EventFunctionsShouldNotHaveAReturnType { return_type_span, function_name, event_span } => build_error_report(span)
            .with_label(Label::new(*return_type_span).with_message("Expected no return type"))
            .with_label(Label::new(*event_span).with_message(format!("'{function_name}' has been declared as an event handler")))
            .with_message("Event handlers should not have a return type")
            .with_help("Either remove the return type, or change this to be a regular function"),
        CompilerErrorKind::CannotCallEventHandler { function_span, function_name } => build_error_report(span)
            .with_label(Label::new(span).with_message("This call here"))
            .with_label(Label::new(*function_span).with_message("This event handler"))
            .with_message("Cannot call event handlers")
            .with_note(format!("'{function_name}' is an event handler. It must be called in rust via the generated 'on_{function_name}' method")),
        CompilerErrorKind::TriggerIncorrectArgs { name, first_definition_span, first_definition_args, second_definition_args } => build_error_report(span)
            .with_label(Label::new(*first_definition_span).with_message(format!("This is called with types {}", first_definition_args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))))
            .with_label(Label::new(span).with_message(format!("This is called with types {}", second_definition_args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))))
            .with_message(format!("Trigger '{name}' has been called with inconsistent arguments"))
            .with_help("`trigger` calls must be made with the same argument types"),
    }
}
