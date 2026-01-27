use std::path::Path;

use ariadne::Cache;

use crate::{
    DiagnosticCache,
    tokens::{self, FileId, LexicalError, LexicalErrorKind, Span},
    types::Type,
};

/// Severity level for diagnostics.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

/// A position in source code (0-indexed line and column).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourcePosition {
    /// 0-indexed line number.
    pub line: usize,
    /// 0-indexed column (character offset within line).
    pub column: usize,
}

/// A range in source code with start and end positions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceRange {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

pub(crate) mod format;

/// All user-facing translatable messages.
#[derive(Clone, Debug)]
pub enum DiagnosticMessage {
    // Primary messages (one per ErrorKind)
    UnknownVariable {
        name: String,
    },
    TypeError {
        expected: Type,
        actual: Type,
    },
    PropertyTypeError {
        property_name: String,
        expected: Type,
        actual: Type,
    },
    FunctionArgumentTypeError {
        function_name: String,
        argument_name: String,
        expected: Type,
        actual: Type,
    },
    UnknownType {
        name: String,
    },
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
    },
    MismatchingReturnTypes {
        expected: Type,
        actual: Type,
    },
    FunctionAlreadyDeclared {
        name: String,
    },
    UnknownFunction {
        name: String,
    },
    IncorrectNumberOfArguments {
        function_name: String,
        expected: usize,
        actual: usize,
    },
    FunctionMustReturnOneValueInThisLocation {
        actual: usize,
    },
    FunctionDoesNotHaveReturn {
        name: String,
    },
    BreakOrContinueOutsideOfLoop,
    DivideByZero,
    DivisionOccursHere,
    OperationOccursHere,
    IntegerOverflow,
    CausesOverflow,
    EventFunctionsShouldNotHaveAReturnType {
        function_name: String,
    },
    CannotCallEventHandler {
        function_name: String,
    },
    TriggerIncorrectArgs {
        name: String,
    },
    CountMismatch {
        ident_count: usize,
        expr_count: usize,
    },
    CannotShadowBuiltin {
        name: String,
    },
    GlobalInitializerNotConstant {
        name: String,
    },
    DuplicatePropertyDeclaration {
        name: String,
    },
    PropertyConflictsWithGlobal {
        name: String,
    },
    PropertyNotInStruct {
        name: String,
    },
    TypeAnnotationMismatch {
        annotated: Type,
        actual: Type,
    },

    // Parse errors
    UnrecognizedEof,
    UnrecognizedToken {
        token: String,
    },
    ExtraToken {
        token: String,
    },
    UnknownTypeToken {
        token: String,
    },
    ExternFunctionWithBlock {
        name: String,
    },

    // Lexer errors
    InvalidNumber {
        error: String,
    },
    InvalidToken,
    InvalidFix,

    // Label messages (reusable across errors)
    AssigningType {
        ty: Type,
    },
    DefinedAs {
        ty: Type,
    },
    ExpectedType {
        ty: Type,
    },
    PassingType {
        ty: Type,
    },
    HasType {
        ty: Type,
    },
    HasReturnValues {
        count: usize,
    },
    FunctionReturnsValues {
        count: usize,
    },
    UnknownVariableLabel,
    UnknownFunctionLabel,
    UnknownTypeLabel,
    MismatchingTypesOnBinaryOperator,
    BinaryOperatorCannotHandleType,
    GotArguments {
        count: usize,
    },
    ExpectedArguments {
        count: usize,
    },
    FunctionMustReturnOneHere,
    FunctionReturnsResults,
    ThisStatement,
    ReducesToZero,
    ExpectedNoReturnType,
    DeclaredAsEventHandler {
        name: String,
    },
    ThisCallHere,
    ThisEventHandler,
    CalledWithTypes {
        types: Box<[Type]>,
    },
    NoValueForVariable,
    NoVariableToReceiveValue,
    CannotShadowBuiltinLabel,
    NotAConstant,
    OriginallyDeclaredHere,
    AlsoDeclaredHere,
    PropertyAlreadyDeclared,
    ConflictsWithGlobal,
    PropertyNotInStructLabel,

    // Parse error labels
    EndOfFileNotExpectedHere,
    UnexpectedToken,
    ExtraTokenLabel,
    UnknownTypeLabel2,
    ExternFunctionCannotHaveBody,

    // Lexer error labels
    InvalidInteger,
    InvalidTokenLabel,
    InvalidFixnumLabel,

    // Notes
    ExpectedOneOfTokens {
        tokens: String,
    },
    LargerThanMaxPositive,
    SmallerThanMinNegative,
    FunctionsMustReturnFixedNumber,
    CannotCallEventHandlerNote {
        function_name: String,
    },
    TriggerCallsMustHaveSameArgTypes,
    WhenAssigningMultipleVars,
    BuiltinVariableNote {
        name: String,
    },
    GlobalInitializersMustBeConstant,

    // Help messages
    RemoveReturnTypeOrChangeToRegularFunction,
}

impl DiagnosticMessage {
    /// Render the message to a string.
    /// Future: accept a locale/translator parameter.
    pub fn render(&self) -> String {
        match self {
            // Primary messages
            DiagnosticMessage::UnknownVariable { name } => format!("Unknown variable '{name}'"),
            DiagnosticMessage::TypeError { expected, actual } => {
                format!("Incorrect type, expected {expected} but got {actual}")
            }
            DiagnosticMessage::PropertyTypeError { property_name, expected, actual } => {
                format!("Incorrect type, property '{property_name}' is declared as {expected} but got {actual}")
            }
            DiagnosticMessage::FunctionArgumentTypeError { function_name, argument_name, expected, actual } => {
                format!("Incorrect type for argument '{argument_name}' of function '{function_name}', expected {expected} but got {actual}")
            }
            DiagnosticMessage::UnknownType { name } => format!("Unknown type for variable '{name}'"),
            DiagnosticMessage::BinaryOperatorTypeError { lhs_type, rhs_type } => {
                format!("Left hand side has type {lhs_type} but right hand side has type {rhs_type}")
            }
            DiagnosticMessage::InvalidTypeForBinaryOperator { type_ } => {
                format!("Binary operator cannot items of type {type_}")
            }
            DiagnosticMessage::InvalidTypeForIfCondition { got } => {
                format!("Condition in if statement must be a bool, but got a {got}")
            }
            DiagnosticMessage::IncorrectNumberOfReturnTypes { expected, actual } => {
                format!("Function should be returning {expected} return values, but you are actually returning {actual}.")
            }
            DiagnosticMessage::MismatchingReturnTypes { expected, actual } => {
                format!("Function is declared to return type {expected} but got {actual}")
            }
            DiagnosticMessage::FunctionAlreadyDeclared { name } => {
                format!("Function with name '{name}' already exists")
            }
            DiagnosticMessage::UnknownFunction { name } => format!("No such function {name}"),
            DiagnosticMessage::IncorrectNumberOfArguments { function_name, expected, actual } => {
                format!("Incorrect number of argumets for function {function_name}, expected {expected} arguments but got {actual}.")
            }
            DiagnosticMessage::FunctionMustReturnOneValueInThisLocation { actual } => {
                format!("Function call must return exactly 1 value here, but got {actual}")
            }
            DiagnosticMessage::FunctionDoesNotHaveReturn { name } => {
                format!("Function {name} should return results, but not all branches return.")
            }
            DiagnosticMessage::BreakOrContinueOutsideOfLoop => {
                "`break` or `continue` must be within a loop".into()
            }
            DiagnosticMessage::DivideByZero => "Divide by zero not allowed".into(),
            DiagnosticMessage::DivisionOccursHere => "This division will fail".into(),
            DiagnosticMessage::OperationOccursHere => "This operation will fail".into(),
            DiagnosticMessage::IntegerOverflow => "Integer overflow detected".into(),
            DiagnosticMessage::CausesOverflow => "This causes overflow".into(),
            DiagnosticMessage::EventFunctionsShouldNotHaveAReturnType { .. } => {
                "Event handlers should not have a return type".into()
            }
            DiagnosticMessage::CannotCallEventHandler { .. } => "Cannot call event handlers".into(),
            DiagnosticMessage::TriggerIncorrectArgs { name } => {
                format!("Trigger '{name}' has been called with inconsistent arguments")
            }
            DiagnosticMessage::CountMismatch { ident_count, expr_count } => {
                format!("Expected {ident_count} expressions, but got {expr_count} of them")
            }
            DiagnosticMessage::CannotShadowBuiltin { name } => {
                format!("Cannot shadow built-in variable '{name}'")
            }
            DiagnosticMessage::GlobalInitializerNotConstant { name } => {
                format!("Global variable '{name}' must be initialized with a constant value")
            }
            DiagnosticMessage::DuplicatePropertyDeclaration { name } => {
                format!("Property '{name}' is already declared")
            }
            DiagnosticMessage::PropertyConflictsWithGlobal { name } => {
                format!("Property '{name}' conflicts with an existing global variable")
            }
            DiagnosticMessage::PropertyNotInStruct { name } => {
                format!("Property '{name}' is declared but no corresponding field exists in the Rust struct")
            }
            DiagnosticMessage::TypeAnnotationMismatch { annotated, actual } => {
                format!("type annotation `{annotated}` doesn't match expression type `{actual}`")
            }

            // Parse errors
            DiagnosticMessage::UnrecognizedEof => "Unexpected end of file".into(),
            DiagnosticMessage::UnrecognizedToken { token } => {
                format!("Unexpected token {token}")
            }
            DiagnosticMessage::ExtraToken { token } => format!("Unexpected extra token {token}"),
            DiagnosticMessage::UnknownTypeToken { token } => {
                format!("'{token}' is not a valid type, must be one of fix, bool or int")
            }
            DiagnosticMessage::ExternFunctionWithBlock { name } => {
                format!("extern function '{name}' cannot have a body")
            }

            // Lexer errors
            DiagnosticMessage::InvalidNumber { error } => error.clone(),
            DiagnosticMessage::InvalidToken => "Invalid token".into(),
            DiagnosticMessage::InvalidFix => "Invalid fixnum".into(),

            // Label messages
            DiagnosticMessage::AssigningType { ty } => format!("Assigning {ty}"),
            DiagnosticMessage::DefinedAs { ty } => format!("Defined as {ty}"),
            DiagnosticMessage::ExpectedType { ty } => format!("Expected {ty}"),
            DiagnosticMessage::PassingType { ty } => format!("Passing {ty}"),
            DiagnosticMessage::HasType { ty } => format!("This has type {ty}"),
            DiagnosticMessage::HasReturnValues { count } => format!("This has {count} return values"),
            DiagnosticMessage::FunctionReturnsValues { count } => format!("Function returns {count} values"),
            DiagnosticMessage::UnknownVariableLabel => "Unknown variable".into(),
            DiagnosticMessage::UnknownFunctionLabel => "Unknown function".into(),
            DiagnosticMessage::UnknownTypeLabel => "Unknown type for variable".into(),
            DiagnosticMessage::MismatchingTypesOnBinaryOperator => "Mismatching types on binary operator".into(),
            DiagnosticMessage::BinaryOperatorCannotHandleType => "Binary operator cannot handle this type".into(),
            DiagnosticMessage::GotArguments { count } => format!("Got {count} arguments"),
            DiagnosticMessage::ExpectedArguments { count } => format!("Expected {count} arguments"),
            DiagnosticMessage::FunctionMustReturnOneHere => "Function must return 1 value here".into(),
            DiagnosticMessage::FunctionReturnsResults => "Function returns results".into(),
            DiagnosticMessage::ThisStatement => "This statement".into(),
            DiagnosticMessage::ReducesToZero => "This reduces to 0".into(),
            DiagnosticMessage::ExpectedNoReturnType => "Expected no return type".into(),
            DiagnosticMessage::DeclaredAsEventHandler { name } => {
                format!("'{name}' has been declared as an event handler")
            }
            DiagnosticMessage::ThisCallHere => "This call here".into(),
            DiagnosticMessage::ThisEventHandler => "This event handler".into(),
            DiagnosticMessage::CalledWithTypes { types } => {
                let types_str = types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                format!("This is called with types {types_str}")
            }
            DiagnosticMessage::NoValueForVariable => "No value for this variable".into(),
            DiagnosticMessage::NoVariableToReceiveValue => "No variable to receive this value".into(),
            DiagnosticMessage::CannotShadowBuiltinLabel => "Cannot shadow built-in variable".into(),
            DiagnosticMessage::NotAConstant => "Not a constant".into(),
            DiagnosticMessage::OriginallyDeclaredHere => "Originally declared here".into(),
            DiagnosticMessage::AlsoDeclaredHere => "Also declared here".into(),
            DiagnosticMessage::PropertyAlreadyDeclared => "Property already declared".into(),
            DiagnosticMessage::ConflictsWithGlobal => "Conflicts with global variable".into(),
            DiagnosticMessage::PropertyNotInStructLabel => "No corresponding field in struct".into(),

            // Parse error labels
            DiagnosticMessage::EndOfFileNotExpectedHere => "End of file not expected here".into(),
            DiagnosticMessage::UnexpectedToken => "Unexpected token".into(),
            DiagnosticMessage::ExtraTokenLabel => "Extra token".into(),
            DiagnosticMessage::UnknownTypeLabel2 => "Unknown type".into(),
            DiagnosticMessage::ExternFunctionCannotHaveBody => "extern function cannot have body".into(),

            // Lexer error labels
            DiagnosticMessage::InvalidInteger => "Invalid integer".into(),
            DiagnosticMessage::InvalidTokenLabel => "Invalid token".into(),
            DiagnosticMessage::InvalidFixnumLabel => "Invalid fixnum".into(),

            // Notes
            DiagnosticMessage::ExpectedOneOfTokens { tokens } => {
                format!("Expected one of tokens {tokens}")
            }
            DiagnosticMessage::LargerThanMaxPositive => {
                format!("Larger than maximum positive number which is {}", i32::MAX)
            }
            DiagnosticMessage::SmallerThanMinNegative => {
                format!("Smaller than minimum negative integer which is {}", i32::MIN)
            }
            DiagnosticMessage::FunctionsMustReturnFixedNumber => {
                "Functions must return a fixed number of values".into()
            }
            DiagnosticMessage::CannotCallEventHandlerNote { function_name } => {
                format!("'{function_name}' is an event handler. It must be called in rust via the generated 'on_{function_name}' method")
            }
            DiagnosticMessage::TriggerCallsMustHaveSameArgTypes => {
                "`trigger` calls must be made with the same argument types".into()
            }
            DiagnosticMessage::WhenAssigningMultipleVars => {
                "When assigning to multiple variables, both sides of the '=' must have the same number of arguments".into()
            }
            DiagnosticMessage::BuiltinVariableNote { name } => {
                format!("'{name}' is a built-in variable provided by the runtime")
            }
            DiagnosticMessage::GlobalInitializersMustBeConstant => {
                "Global initializers must be integer, fix, or bool literals".into()
            }

            // Help messages
            DiagnosticMessage::RemoveReturnTypeOrChangeToRegularFunction => {
                "Either remove the return type, or change this to be a regular function".into()
            }
        }
    }
}

/// Semantic error codes - just the data, no spans or presentation.
#[derive(Clone, Debug)]
pub enum ErrorKind {
    UnknownVariable {
        name: String,
    },
    TypeError {
        expected: Type,
        actual: Type,
    },
    PropertyTypeError {
        property_name: String,
        expected: Type,
        actual: Type,
    },
    FunctionArgumentTypeError {
        function_name: String,
        argument_name: String,
        expected: Type,
        actual: Type,
    },
    UnknownType {
        name: String,
    },
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
    },
    MismatchingReturnTypes {
        expected: Type,
        actual: Type,
    },
    FunctionAlreadyDeclared {
        name: String,
    },
    UnknownFunction {
        name: String,
    },
    IncorrectNumberOfArguments {
        function_name: String,
        expected: usize,
        actual: usize,
    },
    FunctionMustReturnOneValueInThisLocation {
        actual: usize,
    },
    FunctionDoesNotHaveReturn {
        name: String,
    },
    BreakOrContinueOutsideOfLoop,
    DivideByZero,
    EventFunctionsShouldNotHaveAReturnType {
        function_name: String,
    },
    CannotCallEventHandler {
        function_name: String,
    },
    TriggerIncorrectArgs {
        name: String,
        first_definition_args: Box<[Type]>,
        second_definition_args: Box<[Type]>,
    },
    CountMismatch {
        ident_count: usize,
        expr_count: usize,
    },
    CannotShadowBuiltin {
        name: String,
    },
    GlobalInitializerNotConstant {
        name: String,
    },
    DuplicatePropertyDeclaration {
        name: String,
    },
    PropertyConflictsWithGlobal {
        name: String,
    },
    PropertyNotInStruct {
        name: String,
    },
    /// Type annotation doesn't match the inferred expression type
    TypeAnnotationMismatch {
        annotated: Type,
        actual: Type,
    },

    // Parse errors
    UnrecognizedEof {
        expected: Box<[String]>,
    },
    UnrecognizedToken {
        token: String,
    },
    ExtraToken {
        token: String,
    },
    UnknownTypeToken {
        token: String,
    },
    ExternFunctionWithBlock {
        name: String,
    },

    // Lexer errors
    InvalidNumber {
        error: std::num::ParseIntError,
    },
    InvalidToken,
    InvalidFix,
}

impl ErrorKind {
    /// Returns a unique, stable identifier for this error kind.
    pub fn code(&self) -> &'static str {
        match self {
            Self::UnknownVariable { .. } => "E0001",
            Self::TypeError { .. } => "E0002",
            Self::PropertyTypeError { .. } => "E0003",
            Self::FunctionArgumentTypeError { .. } => "E0004",
            Self::UnknownType { .. } => "E0005",
            Self::BinaryOperatorTypeError { .. } => "E0006",
            Self::InvalidTypeForBinaryOperator { .. } => "E0007",
            Self::InvalidTypeForIfCondition { .. } => "E0008",
            Self::IncorrectNumberOfReturnTypes { .. } => "E0009",
            Self::MismatchingReturnTypes { .. } => "E0010",
            Self::FunctionAlreadyDeclared { .. } => "E0011",
            Self::UnknownFunction { .. } => "E0012",
            Self::IncorrectNumberOfArguments { .. } => "E0013",
            Self::FunctionMustReturnOneValueInThisLocation { .. } => "E0014",
            Self::FunctionDoesNotHaveReturn { .. } => "E0015",
            Self::BreakOrContinueOutsideOfLoop => "E0016",
            Self::DivideByZero => "E0017",
            Self::EventFunctionsShouldNotHaveAReturnType { .. } => "E0018",
            Self::CannotCallEventHandler { .. } => "E0019",
            Self::TriggerIncorrectArgs { .. } => "E0020",
            Self::CountMismatch { .. } => "E0021",
            Self::CannotShadowBuiltin { .. } => "E0022",
            Self::GlobalInitializerNotConstant { .. } => "E0023",
            Self::DuplicatePropertyDeclaration { .. } => "E0033",
            Self::PropertyConflictsWithGlobal { .. } => "E0034",
            Self::PropertyNotInStruct { .. } => "E0035",
            Self::TypeAnnotationMismatch { .. } => "E0036",
            Self::UnrecognizedEof { .. } => "E0025",
            Self::UnrecognizedToken { .. } => "E0026",
            Self::ExtraToken { .. } => "E0027",
            Self::UnknownTypeToken { .. } => "E0028",
            Self::ExternFunctionWithBlock { .. } => "E0029",
            Self::InvalidNumber { .. } => "E0030",
            Self::InvalidToken => "E0031",
            Self::InvalidFix => "E0032",
        }
    }

    /// Get the primary message for this error.
    pub fn message(&self) -> DiagnosticMessage {
        match self {
            Self::UnknownVariable { name } => {
                DiagnosticMessage::UnknownVariable { name: name.clone() }
            }
            Self::TypeError { expected, actual } => DiagnosticMessage::TypeError {
                expected: *expected,
                actual: *actual,
            },
            Self::PropertyTypeError {
                property_name,
                expected,
                actual,
            } => DiagnosticMessage::PropertyTypeError {
                property_name: property_name.clone(),
                expected: *expected,
                actual: *actual,
            },
            Self::FunctionArgumentTypeError {
                function_name,
                argument_name,
                expected,
                actual,
            } => DiagnosticMessage::FunctionArgumentTypeError {
                function_name: function_name.clone(),
                argument_name: argument_name.clone(),
                expected: *expected,
                actual: *actual,
            },
            Self::UnknownType { name } => DiagnosticMessage::UnknownType { name: name.clone() },
            Self::BinaryOperatorTypeError { lhs_type, rhs_type } => {
                DiagnosticMessage::BinaryOperatorTypeError {
                    lhs_type: *lhs_type,
                    rhs_type: *rhs_type,
                }
            }
            Self::InvalidTypeForBinaryOperator { type_ } => {
                DiagnosticMessage::InvalidTypeForBinaryOperator { type_: *type_ }
            }
            Self::InvalidTypeForIfCondition { got } => {
                DiagnosticMessage::InvalidTypeForIfCondition { got: *got }
            }
            Self::IncorrectNumberOfReturnTypes { expected, actual } => {
                DiagnosticMessage::IncorrectNumberOfReturnTypes {
                    expected: *expected,
                    actual: *actual,
                }
            }
            Self::MismatchingReturnTypes { expected, actual } => {
                DiagnosticMessage::MismatchingReturnTypes {
                    expected: *expected,
                    actual: *actual,
                }
            }
            Self::FunctionAlreadyDeclared { name } => {
                DiagnosticMessage::FunctionAlreadyDeclared { name: name.clone() }
            }
            Self::UnknownFunction { name } => {
                DiagnosticMessage::UnknownFunction { name: name.clone() }
            }
            Self::IncorrectNumberOfArguments {
                function_name,
                expected,
                actual,
            } => DiagnosticMessage::IncorrectNumberOfArguments {
                function_name: function_name.clone(),
                expected: *expected,
                actual: *actual,
            },
            Self::FunctionMustReturnOneValueInThisLocation { actual } => {
                DiagnosticMessage::FunctionMustReturnOneValueInThisLocation { actual: *actual }
            }
            Self::FunctionDoesNotHaveReturn { name } => {
                DiagnosticMessage::FunctionDoesNotHaveReturn { name: name.clone() }
            }
            Self::BreakOrContinueOutsideOfLoop => DiagnosticMessage::BreakOrContinueOutsideOfLoop,
            Self::DivideByZero => DiagnosticMessage::DivideByZero,
            Self::EventFunctionsShouldNotHaveAReturnType { function_name } => {
                DiagnosticMessage::EventFunctionsShouldNotHaveAReturnType {
                    function_name: function_name.clone(),
                }
            }
            Self::CannotCallEventHandler { function_name } => {
                DiagnosticMessage::CannotCallEventHandler {
                    function_name: function_name.clone(),
                }
            }
            Self::TriggerIncorrectArgs { name, .. } => {
                DiagnosticMessage::TriggerIncorrectArgs { name: name.clone() }
            }
            Self::CountMismatch {
                ident_count,
                expr_count,
            } => DiagnosticMessage::CountMismatch {
                ident_count: *ident_count,
                expr_count: *expr_count,
            },
            Self::CannotShadowBuiltin { name } => {
                DiagnosticMessage::CannotShadowBuiltin { name: name.clone() }
            }
            Self::GlobalInitializerNotConstant { name } => {
                DiagnosticMessage::GlobalInitializerNotConstant { name: name.clone() }
            }
            Self::DuplicatePropertyDeclaration { name } => {
                DiagnosticMessage::DuplicatePropertyDeclaration { name: name.clone() }
            }
            Self::PropertyConflictsWithGlobal { name } => {
                DiagnosticMessage::PropertyConflictsWithGlobal { name: name.clone() }
            }
            Self::PropertyNotInStruct { name } => {
                DiagnosticMessage::PropertyNotInStruct { name: name.clone() }
            }
            Self::TypeAnnotationMismatch { annotated, actual } => {
                DiagnosticMessage::TypeAnnotationMismatch {
                    annotated: *annotated,
                    actual: *actual,
                }
            }
            Self::UnrecognizedEof { .. } => DiagnosticMessage::UnrecognizedEof,
            Self::UnrecognizedToken { token } => DiagnosticMessage::UnrecognizedToken {
                token: token.clone(),
            },
            Self::ExtraToken { token } => DiagnosticMessage::ExtraToken {
                token: token.clone(),
            },
            Self::UnknownTypeToken { token } => DiagnosticMessage::UnknownTypeToken {
                token: token.clone(),
            },
            Self::ExternFunctionWithBlock { name } => {
                DiagnosticMessage::ExternFunctionWithBlock { name: name.clone() }
            }
            Self::InvalidNumber { error } => match error.kind() {
                std::num::IntErrorKind::PosOverflow => DiagnosticMessage::InvalidNumber {
                    error: format!(
                        "Number larger than maximum positive number which is {}",
                        i32::MAX,
                    ),
                },
                std::num::IntErrorKind::NegOverflow => DiagnosticMessage::InvalidNumber {
                    error: format!(
                        "Number smaller than minimum negative number which is {}",
                        i32::MIN,
                    ),
                },
                _ => todo!(),
            },
            Self::InvalidToken => DiagnosticMessage::InvalidToken,
            Self::InvalidFix => DiagnosticMessage::InvalidFix,
        }
    }

    /// Start building a diagnostic at the given span.
    pub(crate) fn at(self, span: Span) -> DiagnosticBuilder {
        DiagnosticBuilder {
            kind: self,
            primary_span: span,
            labels: vec![],
            notes: vec![],
            help: None,
        }
    }
}

/// A diagnostic under construction.
#[must_use = "diagnostics do nothing unless `.emit()`ed"]
pub(crate) struct DiagnosticBuilder {
    kind: ErrorKind,
    primary_span: Span,
    labels: Vec<(Span, DiagnosticMessage)>,
    notes: Vec<DiagnosticMessage>,
    help: Option<DiagnosticMessage>,
}

impl DiagnosticBuilder {
    pub(crate) fn label(mut self, span: Span, message: DiagnosticMessage) -> Self {
        self.labels.push((span, message));
        self
    }

    pub(crate) fn note(mut self, message: DiagnosticMessage) -> Self {
        self.notes.push(message);
        self
    }

    pub(crate) fn help(mut self, message: DiagnosticMessage) -> Self {
        self.help = Some(message);
        self
    }

    pub(crate) fn emit(self, diagnostics: &mut Diagnostics) {
        diagnostics.add(self.build());
    }

    pub(crate) fn build(self) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            kind: DiagnosticKind::Error(self.kind),
            primary_span: self.primary_span,
            labels: self.labels,
            notes: self.notes,
            help: self.help,
        }
    }
}

// ============================================================================
// Warning Types
// ============================================================================

/// Warning codes for compile-time warnings.
#[derive(Clone, Debug)]
pub enum WarningKind {
    /// Division by a value that is known to be zero at compile time.
    DivisionByZero,
    /// Integer overflow detected at compile time.
    IntegerOverflow,
}

impl WarningKind {
    /// Returns a unique, stable identifier for this warning kind.
    pub fn code(&self) -> &'static str {
        match self {
            Self::DivisionByZero => "W0001",
            Self::IntegerOverflow => "W0002",
        }
    }

    /// Get the primary message for this warning.
    pub fn message(&self) -> DiagnosticMessage {
        match self {
            Self::DivisionByZero => DiagnosticMessage::DivideByZero,
            Self::IntegerOverflow => DiagnosticMessage::IntegerOverflow,
        }
    }

    /// Start building a warning at the given span.
    pub fn at(self, span: Span) -> WarningBuilder {
        WarningBuilder {
            kind: self,
            primary_span: span,
            labels: vec![],
            notes: vec![],
        }
    }
}

/// A warning under construction.
#[must_use = "warnings do nothing unless `.emit()`ed"]
pub struct WarningBuilder {
    kind: WarningKind,
    primary_span: Span,
    labels: Vec<(Span, DiagnosticMessage)>,
    notes: Vec<DiagnosticMessage>,
}

impl WarningBuilder {
    pub fn label(mut self, span: Span, message: DiagnosticMessage) -> Self {
        self.labels.push((span, message));
        self
    }

    #[allow(dead_code)]
    pub fn note(mut self, message: DiagnosticMessage) -> Self {
        self.notes.push(message);
        self
    }

    pub fn emit(self, diagnostics: &mut Diagnostics) {
        // Skip duplicate warnings at the same location
        if diagnostics.has_warning_at(self.kind.code(), self.primary_span) {
            return;
        }
        diagnostics.add(self.build());
    }

    pub fn build(self) -> Diagnostic {
        Diagnostic {
            severity: Severity::Warning,
            kind: DiagnosticKind::Warning(self.kind),
            primary_span: self.primary_span,
            labels: self.labels,
            notes: self.notes,
            help: None,
        }
    }
}

/// The kind of diagnostic - either an error or a warning.
#[derive(Clone, Debug)]
pub enum DiagnosticKind {
    Error(ErrorKind),
    Warning(WarningKind),
}

impl DiagnosticKind {
    pub fn code(&self) -> &'static str {
        match self {
            Self::Error(e) => e.code(),
            Self::Warning(w) => w.code(),
        }
    }

    pub fn message(&self) -> DiagnosticMessage {
        match self {
            Self::Error(e) => e.message(),
            Self::Warning(w) => w.message(),
        }
    }
}

/// A complete diagnostic ready for rendering.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub kind: DiagnosticKind,
    pub primary_span: Span,
    pub labels: Vec<(Span, DiagnosticMessage)>,
    pub notes: Vec<DiagnosticMessage>,
    pub help: Option<DiagnosticMessage>,
}

impl Diagnostic {
    /// Get the primary message text.
    pub fn message(&self) -> String {
        self.kind.message().render()
    }

    /// Create a diagnostic from a lalrpop parse error.
    pub(crate) fn from_lalrpop(
        value: lalrpop_util::ParseError<usize, tokens::Token<'_>, LexicalError>,
        file_id: FileId,
    ) -> Self {
        match value {
            lalrpop_util::ParseError::InvalidToken { location } => {
                let span = Span::new(file_id, location, location);
                ErrorKind::InvalidToken
                    .at(span)
                    .label(span, DiagnosticMessage::InvalidTokenLabel)
                    .build()
            }
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                let span = Span::new(file_id, location, location);
                ErrorKind::UnrecognizedEof {
                    expected: expected.clone().into_boxed_slice(),
                }
                .at(span)
                .label(span, DiagnosticMessage::EndOfFileNotExpectedHere)
                .note(DiagnosticMessage::ExpectedOneOfTokens {
                    tokens: expected.join(", "),
                })
                .build()
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let span = Span::new(file_id, token.0, token.2);
                ErrorKind::UnrecognizedToken {
                    token: format!("{:?}", token.1),
                }
                .at(span)
                .label(span, DiagnosticMessage::UnexpectedToken)
                .note(DiagnosticMessage::ExpectedOneOfTokens {
                    tokens: expected.join(", "),
                })
                .build()
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let span = Span::new(file_id, token.0, token.2);
                ErrorKind::ExtraToken {
                    token: format!("{:?}", token.1),
                }
                .at(span)
                .label(span, DiagnosticMessage::ExtraTokenLabel)
                .build()
            }
            lalrpop_util::ParseError::User { error } => {
                // Lexer error - convert to diagnostic
                let span = error.span;
                match error.kind {
                    LexicalErrorKind::InvalidNumber(e) => ErrorKind::InvalidNumber { error: e }
                        .at(span)
                        .label(span, DiagnosticMessage::InvalidInteger)
                        .build(),
                    LexicalErrorKind::InvalidFix => ErrorKind::InvalidFix
                        .at(span)
                        .label(span, DiagnosticMessage::InvalidFixnumLabel)
                        .build(),
                    LexicalErrorKind::InvalidToken => ErrorKind::InvalidToken
                        .at(span)
                        .label(span, DiagnosticMessage::InvalidTokenLabel)
                        .build(),
                }
            }
        }
    }
}

// ============================================================================
// Diagnostics Collection
// ============================================================================

#[derive(Debug, Clone)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
    cache: DiagnosticCache,
}

impl Diagnostics {
    pub fn new(file_id: FileId, filename: impl AsRef<Path>, content: &str) -> Self {
        Self {
            diagnostics: vec![],
            cache: DiagnosticCache::new(file_id, filename, content),
        }
    }

    /// Add a file to the diagnostic cache for error reporting.
    pub fn add_file(&mut self, file_id: FileId, filename: impl AsRef<Path>, content: &str) {
        self.cache.add_file(file_id, filename, content);
    }

    /// Add a diagnostic.
    pub(crate) fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Check if a warning with the given code already exists at the given span.
    pub(crate) fn has_warning_at(&self, code: &str, span: Span) -> bool {
        self.diagnostics.iter().any(|d| {
            d.severity == Severity::Warning && d.kind.code() == code && d.primary_span == span
        })
    }

    pub(crate) fn add_lalrpop(
        &mut self,
        value: lalrpop_util::ParseError<usize, tokens::Token<'_>, LexicalError>,
        file_id: FileId,
    ) {
        self.add(Diagnostic::from_lalrpop(value, file_id));
    }

    /// Get an iterator over the diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    /// Convert a span to a source range with line/column positions.
    ///
    /// Returns `None` if the span's file is not in the cache or the offsets are invalid.
    pub fn span_to_range(&mut self, span: Span) -> Option<SourceRange> {
        let source = self.cache.fetch(&span.file_id).ok()?;

        let (_, start_line, start_col) = source.get_offset_line(span.start)?;
        let (_, end_line, end_col) = source.get_offset_line(span.end)?;

        Some(SourceRange {
            start: SourcePosition {
                line: start_line,
                column: start_col,
            },
            end: SourcePosition {
                line: end_line,
                column: end_col,
            },
        })
    }

    pub fn pretty_string(&mut self, colourful: bool) -> String {
        self.string_with_optional_colour(colourful)
    }

    fn string_with_optional_colour(&mut self, colourful: bool) -> String {
        let mut output = Vec::new();

        for diagnostic in &self.diagnostics {
            diagnostic
                .write(&mut output, &mut self.cache, colourful)
                .unwrap();
        }

        String::from_utf8_lossy(&output).into_owned()
    }

    pub fn has_any(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Returns true if there are any errors (not just warnings).
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }
}
