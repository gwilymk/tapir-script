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
    FunctionCouldReturnDueToThis,
    BreakOrContinueOutsideOfLoop,
    DivideByZero,
    DivisionOccursHere,
    OperationOccursHere,
    IntegerOverflow,
    CausesOverflow,
    BuiltinWillFail {
        name: String,
    },
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
    /// Division by zero in constant expression
    DivisionByZeroInConstant,
    /// Integer overflow in constant expression
    IntegerOverflowInConstant,
    /// Type mismatch in constant expression
    TypeMismatchInConstantLabel {
        expected: &'static str,
        found: &'static str,
    },
    GlobalRequiresTypeOrInitializer {
        name: String,
    },
    InvalidAssignmentTarget,
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
    BuiltinOutsidePrelude {
        name: String,
    },
    StructShadowsBuiltinType {
        name: String,
    },
    DuplicateStructName {
        name: String,
    },
    DuplicateStructField {
        struct_name: String,
        field_name: String,
    },
    UnknownField {
        struct_name: String,
        field_name: String,
    },
    FieldAccessOnNonStruct {
        ty: Type,
    },
    RecursiveStruct {
        name: String,
    },
    UnknownMethodType {
        name: String,
    },
    UnknownMethod {
        type_name: String,
        method_name: String,
    },
    MethodCannotBeEventHandler {
        method_name: String,
    },
    OperatorRequiresStruct {
        left: String,
        right: String,
    },
    OperatorCannotBeEventHandler,
    OperatorRequiresTwoArguments {
        actual: usize,
    },
    NoOperatorOverload {
        left_type: String,
        operator: String,
        right_type: String,
    },
    OperatorMustReturnOneValue {
        operator: String,
        actual_count: usize,
    },
    TypeMismatch {
        expected: Type,
        got: Type,
    },
    InvalidTypeForUnaryOperator {
        operator: crate::ast::UnaryOperator,
        operand_type: Type,
    },
    /// Script has event handlers but no event_type was specified in the macro
    EventHandlerWithoutEventType,

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
    UnaryOperatorCannotHandleType {
        operator: crate::ast::UnaryOperator,
        operand_type: Type,
    },
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
    BuiltinTypeCannotBeStructName,
    NotAConstant,
    OriginallyDeclaredHere,
    AlsoDeclaredHere,
    PropertyAlreadyDeclared,
    ConflictsWithGlobal,
    PropertyNotInStructLabel,
    StructDefinedHere,
    FieldCreatesCycle {
        field_name: String,
    },
    FieldInCyclePath {
        field_name: String,
        target_struct: String,
    },
    MethodCannotBeEventHandlerLabel,
    MethodDefinedHere,
    FunctionDefinedHere,
    CallSiteHere,

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

    // Import error messages
    CircularImport {
        cycle: String,
    },
    ImportFileNotFound,
    DuplicateImportedDefinition {
        name: String,
    },
    ToplevelStatementsInImport,

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
    GlobalInitializersMustBeConstant,

    // Help messages
    RemoveReturnTypeOrChangeToRegularFunction,
    DefineOperatorFunction {
        left: String,
        op: String,
        right: String,
    },
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
            DiagnosticMessage::FunctionCouldReturnDueToThis => "Function could return due to this".into(),
            DiagnosticMessage::BreakOrContinueOutsideOfLoop => {
                "`break` or `continue` must be within a loop".into()
            }
            DiagnosticMessage::DivideByZero => "Divide by zero not allowed".into(),
            DiagnosticMessage::DivisionOccursHere => "This division will fail".into(),
            DiagnosticMessage::OperationOccursHere => "This operation will fail".into(),
            DiagnosticMessage::IntegerOverflow => "Integer overflow detected".into(),
            DiagnosticMessage::CausesOverflow => "This causes overflow".into(),
            DiagnosticMessage::BuiltinWillFail { name } => {
                format!("Call to '{name}' will fail at runtime")
            }
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
            DiagnosticMessage::DivisionByZeroInConstant => {
                "division by zero in constant expression".into()
            }
            DiagnosticMessage::IntegerOverflowInConstant => {
                "integer overflow in constant expression".into()
            }
            DiagnosticMessage::TypeMismatchInConstantLabel { expected, found } => {
                format!("expected {expected}, found {found}")
            }
            DiagnosticMessage::GlobalRequiresTypeOrInitializer { name } => {
                format!("Global variable '{name}' requires either a type annotation or an initializer")
            }
            DiagnosticMessage::InvalidAssignmentTarget => {
                "Invalid assignment target: expected a variable or field access".into()
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
            DiagnosticMessage::BuiltinOutsidePrelude { name } => {
                format!("builtin function `{name}` can only be declared in the prelude")
            }
            DiagnosticMessage::StructShadowsBuiltinType { name } => {
                format!("struct name `{name}` shadows a builtin type")
            }
            DiagnosticMessage::DuplicateStructName { name } => {
                format!("struct `{name}` is already defined")
            }
            DiagnosticMessage::DuplicateStructField {
                struct_name,
                field_name,
            } => {
                format!("field `{field_name}` is already defined in struct `{struct_name}`")
            }
            DiagnosticMessage::UnknownField {
                struct_name,
                field_name,
            } => {
                format!("struct `{struct_name}` has no field `{field_name}`")
            }
            DiagnosticMessage::FieldAccessOnNonStruct { ty } => {
                format!("cannot access field on type `{ty}` (not a struct)")
            }
            DiagnosticMessage::RecursiveStruct { name } => {
                format!("struct `{name}` contains itself, creating infinite recursion")
            }
            DiagnosticMessage::UnknownMethodType { name } => {
                format!("cannot define method on unknown type `{name}`")
            }
            DiagnosticMessage::UnknownMethod {
                type_name,
                method_name,
            } => {
                format!("type `{type_name}` has no method `{method_name}`")
            }
            DiagnosticMessage::MethodCannotBeEventHandler { method_name } => {
                format!("method `{method_name}` cannot be an event handler")
            }
            DiagnosticMessage::OperatorRequiresStruct { left, right } => {
                format!("operator function for `{left}` and `{right}` requires at least one struct type")
            }
            DiagnosticMessage::OperatorCannotBeEventHandler => {
                "operator functions cannot be event handlers".into()
            }
            DiagnosticMessage::OperatorRequiresTwoArguments { actual } => {
                format!("operator function must have exactly 2 arguments, but has {actual}")
            }
            DiagnosticMessage::NoOperatorOverload {
                left_type,
                operator,
                right_type,
            } => {
                format!("no operator `{operator}` defined for `{left_type}` and `{right_type}`")
            }
            DiagnosticMessage::OperatorMustReturnOneValue {
                operator,
                actual_count,
            } => {
                format!(
                    "operator `{operator}` function must return exactly one value, but returns {actual_count}"
                )
            }
            DiagnosticMessage::TypeMismatch { expected, got } => {
                format!("expected `{expected}`, got `{got}`")
            }
            DiagnosticMessage::InvalidTypeForUnaryOperator { operator, operand_type } => {
                format!("Operator `{operator}` cannot be applied to type `{operand_type}`")
            }
            DiagnosticMessage::EventHandlerWithoutEventType => {
                "Script contains event handlers but no event_type was specified. Add `event_type = MyEventEnum` to your #[tapir(...)] attribute.".into()
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
            DiagnosticMessage::UnaryOperatorCannotHandleType { operator, operand_type } => {
                format!("Operator `{operator}` cannot be applied to type `{operand_type}`")
            }
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
            DiagnosticMessage::BuiltinTypeCannotBeStructName => "Builtin type cannot be used as struct name".into(),
            DiagnosticMessage::NotAConstant => "Not a constant".into(),
            DiagnosticMessage::OriginallyDeclaredHere => "Originally declared here".into(),
            DiagnosticMessage::AlsoDeclaredHere => "Also declared here".into(),
            DiagnosticMessage::PropertyAlreadyDeclared => "Property already declared".into(),
            DiagnosticMessage::ConflictsWithGlobal => "Conflicts with global variable".into(),
            DiagnosticMessage::PropertyNotInStructLabel => "No corresponding field in struct".into(),
            DiagnosticMessage::StructDefinedHere => "Struct defined here".into(),
            DiagnosticMessage::FieldCreatesCycle { field_name } => {
                format!("Field `{field_name}` creates the cycle")
            }
            DiagnosticMessage::FieldInCyclePath {
                field_name,
                target_struct,
            } => {
                format!("Field `{field_name}` contains `{target_struct}`")
            }
            DiagnosticMessage::MethodCannotBeEventHandlerLabel => {
                "Methods cannot be event handlers".into()
            }
            DiagnosticMessage::MethodDefinedHere => "Method defined here".into(),
            DiagnosticMessage::FunctionDefinedHere => "Function defined here".into(),
            DiagnosticMessage::CallSiteHere => "Called here".into(),

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

            // Import error messages
            DiagnosticMessage::CircularImport { cycle } => {
                format!("Circular import detected: {cycle}")
            }
            DiagnosticMessage::ImportFileNotFound => "Imported file not found".into(),
            DiagnosticMessage::DuplicateImportedDefinition { name } => {
                format!("'{name}' is already defined")
            }
            DiagnosticMessage::ToplevelStatementsInImport => {
                "Imported files cannot contain top-level statements".into()
            }

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
            DiagnosticMessage::GlobalInitializersMustBeConstant => {
                "Global initializers must be constant expressions".into()
            }

            // Help messages
            DiagnosticMessage::RemoveReturnTypeOrChangeToRegularFunction => {
                "Either remove the return type, or change this to be a regular function".into()
            }
            DiagnosticMessage::DefineOperatorFunction { left, op, right } => {
                format!("define `fn {left} {op} {right}(a, b) -> ... {{ ... }}` to enable this operator")
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
    /// Division by zero in constant expression
    DivisionByZeroInConstant,
    /// Integer overflow in constant expression
    OverflowInConstant,
    /// Type mismatch in constant expression
    TypeMismatchInConstant {
        expected: &'static str,
        found: &'static str,
    },
    /// Global variable has neither type annotation nor initializer
    GlobalRequiresTypeOrInitializer {
        name: String,
    },
    /// Assignment target is not a valid l-value (must be variable or field access)
    InvalidAssignmentTarget,
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
    /// Builtin function declared outside of prelude
    BuiltinOutsidePrelude {
        name: String,
    },
    /// Struct name shadows a builtin type (int, fix, bool)
    StructShadowsBuiltinType {
        name: String,
    },
    /// Two structs have the same name
    DuplicateStructName {
        name: String,
    },
    /// A struct has duplicate field names
    DuplicateStructField {
        struct_name: String,
        field_name: String,
    },
    /// Field access on unknown field
    UnknownField {
        struct_name: String,
        field_name: String,
    },
    /// Field access on non-struct type
    FieldAccessOnNonStruct {
        ty: Type,
    },
    /// Recursive struct definition (struct contains itself directly or indirectly)
    RecursiveStruct {
        name: String,
    },

    /// Method defined on unknown type
    UnknownMethodType {
        name: String,
    },

    /// Unknown method on type
    UnknownMethod {
        type_name: String,
        method_name: String,
    },

    /// Event modifier not allowed on methods
    MethodCannotBeEventHandler {
        method_name: String,
    },

    /// Operator definition requires at least one struct type
    OperatorRequiresStruct {
        left: String,
        right: String,
    },

    /// Operator function cannot be an event handler
    OperatorCannotBeEventHandler,

    /// Operator function must have exactly 2 arguments
    OperatorRequiresTwoArguments {
        actual: usize,
    },

    /// No operator overload found for types
    NoOperatorOverload {
        left_type: String,
        operator: String,
        right_type: String,
    },

    /// Operator function must return exactly one value
    OperatorMustReturnOneValue {
        operator: String,
        actual_count: usize,
    },

    /// Expected a specific type but got something else (e.g., wait frames must be int)
    TypeMismatch {
        expected: Type,
        got: Type,
    },

    /// Invalid type for unary operator
    InvalidTypeForUnaryOperator {
        operator: crate::ast::UnaryOperator,
        operand_type: Type,
    },

    /// Script has event handlers but no event_type was specified in the macro
    EventHandlerWithoutEventType,

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

    // Import errors
    /// Circular import detected
    CircularImport {
        cycle: String,
    },
    /// Imported file not found
    ImportFileNotFound {
        path: String,
    },
    /// Duplicate definition from import
    DuplicateImportedDefinition {
        name: String,
        kind: String,
    },
    /// Top-level statements in imported file
    ToplevelStatementsInImport,
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
            Self::DivisionByZeroInConstant => "E0055",
            Self::OverflowInConstant => "E0056",
            Self::TypeMismatchInConstant { .. } => "E0057",
            Self::GlobalRequiresTypeOrInitializer { .. } => "E0052",
            Self::InvalidAssignmentTarget => "E0053",
            Self::DuplicatePropertyDeclaration { .. } => "E0033",
            Self::PropertyConflictsWithGlobal { .. } => "E0034",
            Self::PropertyNotInStruct { .. } => "E0035",
            Self::TypeAnnotationMismatch { .. } => "E0036",
            Self::BuiltinOutsidePrelude { .. } => "E0037",
            Self::StructShadowsBuiltinType { .. } => "E0038",
            Self::DuplicateStructName { .. } => "E0039",
            Self::DuplicateStructField { .. } => "E0040",
            Self::UnknownField { .. } => "E0041",
            Self::FieldAccessOnNonStruct { .. } => "E0042",
            Self::RecursiveStruct { .. } => "E0043",
            Self::UnknownMethodType { .. } => "E0044",
            Self::UnknownMethod { .. } => "E0045",
            Self::MethodCannotBeEventHandler { .. } => "E0046",
            Self::OperatorRequiresStruct { .. } => "E0047",
            Self::OperatorCannotBeEventHandler => "E0048",
            Self::OperatorRequiresTwoArguments { .. } => "E0049",
            Self::NoOperatorOverload { .. } => "E0050",
            Self::OperatorMustReturnOneValue { .. } => "E0051",
            Self::TypeMismatch { .. } => "E0054",
            Self::InvalidTypeForUnaryOperator { .. } => "E0058",
            Self::EventHandlerWithoutEventType => "E0059",
            Self::UnrecognizedEof { .. } => "E0025",
            Self::UnrecognizedToken { .. } => "E0026",
            Self::ExtraToken { .. } => "E0027",
            Self::UnknownTypeToken { .. } => "E0028",
            Self::ExternFunctionWithBlock { .. } => "E0029",
            Self::InvalidNumber { .. } => "E0030",
            Self::InvalidToken => "E0031",
            Self::InvalidFix => "E0032",
            Self::CircularImport { .. } => "E0060",
            Self::ImportFileNotFound { .. } => "E0061",
            Self::DuplicateImportedDefinition { .. } => "E0062",
            Self::ToplevelStatementsInImport => "E0063",
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
            Self::DivisionByZeroInConstant => DiagnosticMessage::DivisionByZeroInConstant,
            Self::OverflowInConstant => DiagnosticMessage::IntegerOverflowInConstant,
            Self::TypeMismatchInConstant { expected, found } => {
                DiagnosticMessage::TypeMismatchInConstantLabel { expected, found }
            }
            Self::GlobalRequiresTypeOrInitializer { name } => {
                DiagnosticMessage::GlobalRequiresTypeOrInitializer { name: name.clone() }
            }
            Self::InvalidAssignmentTarget => DiagnosticMessage::InvalidAssignmentTarget,
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
            Self::BuiltinOutsidePrelude { name } => {
                DiagnosticMessage::BuiltinOutsidePrelude { name: name.clone() }
            }
            Self::StructShadowsBuiltinType { name } => {
                DiagnosticMessage::StructShadowsBuiltinType { name: name.clone() }
            }
            Self::DuplicateStructName { name } => {
                DiagnosticMessage::DuplicateStructName { name: name.clone() }
            }
            Self::DuplicateStructField {
                struct_name,
                field_name,
            } => DiagnosticMessage::DuplicateStructField {
                struct_name: struct_name.clone(),
                field_name: field_name.clone(),
            },
            Self::UnknownField {
                struct_name,
                field_name,
            } => DiagnosticMessage::UnknownField {
                struct_name: struct_name.clone(),
                field_name: field_name.clone(),
            },
            Self::FieldAccessOnNonStruct { ty } => {
                DiagnosticMessage::FieldAccessOnNonStruct { ty: *ty }
            }
            Self::RecursiveStruct { name } => {
                DiagnosticMessage::RecursiveStruct { name: name.clone() }
            }
            Self::UnknownMethodType { name } => {
                DiagnosticMessage::UnknownMethodType { name: name.clone() }
            }
            Self::UnknownMethod {
                type_name,
                method_name,
            } => DiagnosticMessage::UnknownMethod {
                type_name: type_name.clone(),
                method_name: method_name.clone(),
            },
            Self::MethodCannotBeEventHandler { method_name } => {
                DiagnosticMessage::MethodCannotBeEventHandler {
                    method_name: method_name.clone(),
                }
            }
            Self::OperatorRequiresStruct { left, right } => {
                DiagnosticMessage::OperatorRequiresStruct {
                    left: left.clone(),
                    right: right.clone(),
                }
            }
            Self::OperatorCannotBeEventHandler => DiagnosticMessage::OperatorCannotBeEventHandler,
            Self::OperatorRequiresTwoArguments { actual } => {
                DiagnosticMessage::OperatorRequiresTwoArguments { actual: *actual }
            }
            Self::NoOperatorOverload {
                left_type,
                operator,
                right_type,
            } => DiagnosticMessage::NoOperatorOverload {
                left_type: left_type.clone(),
                operator: operator.clone(),
                right_type: right_type.clone(),
            },
            Self::OperatorMustReturnOneValue {
                operator,
                actual_count,
            } => DiagnosticMessage::OperatorMustReturnOneValue {
                operator: operator.clone(),
                actual_count: *actual_count,
            },
            Self::TypeMismatch { expected, got } => DiagnosticMessage::TypeMismatch {
                expected: *expected,
                got: *got,
            },
            Self::InvalidTypeForUnaryOperator {
                operator,
                operand_type,
            } => DiagnosticMessage::UnaryOperatorCannotHandleType {
                operator: *operator,
                operand_type: *operand_type,
            },
            Self::EventHandlerWithoutEventType => DiagnosticMessage::EventHandlerWithoutEventType,
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
            Self::CircularImport { cycle } => DiagnosticMessage::CircularImport { cycle: cycle.clone() },
            Self::ImportFileNotFound { .. } => {
                DiagnosticMessage::ImportFileNotFound
            }
            Self::DuplicateImportedDefinition { name, .. } => {
                DiagnosticMessage::DuplicateImportedDefinition { name: name.clone() }
            }
            Self::ToplevelStatementsInImport => DiagnosticMessage::ToplevelStatementsInImport,
        }
    }

    /// Start building a diagnostic at the given span with a primary label.
    pub(crate) fn at(self, span: Span, label: DiagnosticMessage) -> DiagnosticBuilder {
        DiagnosticBuilder {
            kind: self,
            primary_span: span,
            primary_label: label,
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
    primary_label: DiagnosticMessage,
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
        // Primary label comes first, then secondary labels
        let mut labels = vec![(self.primary_span, self.primary_label)];
        labels.extend(self.labels);

        Diagnostic {
            severity: Severity::Error,
            kind: DiagnosticKind::Error(self.kind),
            primary_span: self.primary_span,
            labels,
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
    /// Builtin function call will fail at runtime (e.g., sqrt of negative).
    BuiltinWillFail { name: String },
}

impl WarningKind {
    /// Returns a unique, stable identifier for this warning kind.
    pub fn code(&self) -> &'static str {
        match self {
            Self::DivisionByZero => "W0001",
            Self::IntegerOverflow => "W0002",
            Self::BuiltinWillFail { .. } => "W0003",
        }
    }

    /// Get the primary message for this warning.
    pub fn message(&self) -> DiagnosticMessage {
        match self {
            Self::DivisionByZero => DiagnosticMessage::DivideByZero,
            Self::IntegerOverflow => DiagnosticMessage::IntegerOverflow,
            Self::BuiltinWillFail { name } => {
                DiagnosticMessage::BuiltinWillFail { name: name.clone() }
            }
        }
    }

    /// Start building a warning at the given span with a primary label.
    pub fn at(self, span: Span, label: DiagnosticMessage) -> WarningBuilder {
        WarningBuilder {
            kind: self,
            primary_span: span,
            primary_label: label,
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
    primary_label: DiagnosticMessage,
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
        // Primary label comes first, then secondary labels
        let mut labels = vec![(self.primary_span, self.primary_label)];
        labels.extend(self.labels);

        Diagnostic {
            severity: Severity::Warning,
            kind: DiagnosticKind::Warning(self.kind),
            primary_span: self.primary_span,
            labels,
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

    /// Simplify a list of expected tokens for error messages.
    /// Groups binary/comparison operators to avoid overwhelming the user.
    fn simplify_expected_tokens(expected: &[String]) -> String {
        const BINARY_OPS: &[&str] = &[
            "\"+\"", "\"-\"", "\"*\"", "\"/\"", "\"%\"", "\"//\"", "\"%%\"",
        ];
        const COMPARISON_OPS: &[&str] = &["\"==\"", "\"!=\"", "\">\"", "\">=\"", "\"<\"", "\"<=\""];
        const LOGICAL_OPS: &[&str] = &["\"||\"", "\"&&\""];
        const BITWISE_OPS: &[&str] = &["\"<<\"", "\">>\"", "\"&\"", "\"|\""];

        let mut result = Vec::new();
        let mut has_binary = false;
        let mut has_comparison = false;
        let mut has_logical = false;
        let mut has_bitwise = false;

        for token in expected {
            if BINARY_OPS.contains(&token.as_str()) {
                has_binary = true;
            } else if COMPARISON_OPS.contains(&token.as_str()) {
                has_comparison = true;
            } else if LOGICAL_OPS.contains(&token.as_str()) {
                has_logical = true;
            } else if BITWISE_OPS.contains(&token.as_str()) {
                has_bitwise = true;
            } else {
                result.push(token.clone());
            }
        }

        // Only group if we have multiple operator types
        let op_count = [has_binary, has_comparison, has_logical, has_bitwise]
            .iter()
            .filter(|&&b| b)
            .count();

        if op_count >= 2 {
            result.push("<operator>".to_string());
        } else {
            // Include individual operators if only one category
            for token in expected {
                if BINARY_OPS.contains(&token.as_str())
                    || COMPARISON_OPS.contains(&token.as_str())
                    || LOGICAL_OPS.contains(&token.as_str())
                    || BITWISE_OPS.contains(&token.as_str())
                {
                    result.push(token.clone());
                }
            }
        }

        result.join(", ")
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
                    .at(span, DiagnosticMessage::InvalidTokenLabel)
                    .build()
            }
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                let span = Span::new(file_id, location, location);
                ErrorKind::UnrecognizedEof {
                    expected: expected.clone().into_boxed_slice(),
                }
                .at(span, DiagnosticMessage::EndOfFileNotExpectedHere)
                .note(DiagnosticMessage::ExpectedOneOfTokens {
                    tokens: Self::simplify_expected_tokens(&expected),
                })
                .build()
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let span = Span::new(file_id, token.0, token.2);
                ErrorKind::UnrecognizedToken {
                    token: format!("{:?}", token.1),
                }
                .at(span, DiagnosticMessage::UnexpectedToken)
                .note(DiagnosticMessage::ExpectedOneOfTokens {
                    tokens: Self::simplify_expected_tokens(&expected),
                })
                .build()
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let span = Span::new(file_id, token.0, token.2);
                ErrorKind::ExtraToken {
                    token: format!("{:?}", token.1),
                }
                .at(span, DiagnosticMessage::ExtraTokenLabel)
                .build()
            }
            lalrpop_util::ParseError::User { error } => {
                // Lexer error - convert to diagnostic
                let span = error.span;
                match error.kind {
                    LexicalErrorKind::InvalidNumber(e) => ErrorKind::InvalidNumber { error: e }
                        .at(span, DiagnosticMessage::InvalidInteger)
                        .build(),
                    LexicalErrorKind::InvalidFix => ErrorKind::InvalidFix
                        .at(span, DiagnosticMessage::InvalidFixnumLabel)
                        .build(),
                    LexicalErrorKind::InvalidToken => ErrorKind::InvalidToken
                        .at(span, DiagnosticMessage::InvalidTokenLabel)
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
