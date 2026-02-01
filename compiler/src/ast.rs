use std::{borrow::Cow, fmt::Display, iter};

use crate::{
    tokens::{FileId, Span},
    types::{StructId, Type},
};

pub(crate) use metadata::Metadata;

use serde::Serialize;

mod metadata;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, Serialize, PartialOrd, Ord)]
pub struct SymbolId(pub u64);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, Serialize, PartialOrd, Ord)]
pub struct FunctionId(pub usize);

impl FunctionId {
    pub fn toplevel() -> Self {
        Self(0)
    }

    pub fn is_toplevel(self) -> bool {
        self == Self::toplevel()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, Serialize, PartialOrd, Ord)]
pub struct ExternalFunctionId(pub usize);

/// ID for a builtin function. Positive IDs are pure (constant foldable),
/// negative IDs are impure (require runtime state).
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, Serialize, PartialOrd, Ord)]
pub struct BuiltinFunctionId(pub i16);

impl BuiltinFunctionId {
    pub fn is_pure(self) -> bool {
        self.0 >= 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, PartialOrd, Ord)]
pub enum InternalOrExternalFunctionId {
    Internal(FunctionId),
    External(ExternalFunctionId),
    Builtin(BuiltinFunctionId),
    /// Implicit constructor function for a struct type
    StructConstructor(StructId),
}

/// Stored in MethodCall expression metadata during type checking.
/// Contains the resolved function ID for IR lowering.
#[derive(Clone, Copy, Debug)]
pub struct MethodCallInfo {
    /// The resolved function ID (maps to mangled name like Point@distance)
    pub function_id: InternalOrExternalFunctionId,
}

/// Stored in BinaryOperation expression metadata when the operator
/// is resolved to a user-defined operator function.
#[derive(Clone, Copy, Debug)]
pub struct OperatorOverloadInfo {
    /// The resolved function ID for the operator (mangled name like Point@+@Point)
    pub function_id: InternalOrExternalFunctionId,
}

pub type Fix = agb_fixnum::Num<i32, 8>;

#[derive(Clone, Debug, Serialize)]
pub struct Script<'input> {
    pub struct_declarations: Vec<StructDeclaration<'input>>,
    pub property_declarations: Vec<PropertyDeclaration<'input>>,
    pub globals: Vec<GlobalDeclaration<'input>>,
    pub functions: Vec<Function<'input>>,
    pub extern_functions: Vec<ExternFunctionDefinition<'input>>,
    pub builtin_functions: Vec<BuiltinFunction<'input>>,
}

/// A builtin function declaration: `builtin(N) fn name(args) -> ret;`
/// For methods: `builtin(N) fn Type.name(self, args) -> ret;`
#[derive(Clone, Debug, Serialize)]
pub struct BuiltinFunction<'input> {
    /// Doc comment for this builtin function, if any
    pub doc_comment: Option<String>,
    /// If Some, this is a method on the given type (e.g., "Point", "fix").
    /// Uses Ident to preserve span for error messages.
    pub receiver_type: Option<Ident<'input>>,
    pub name: &'input str,
    pub builtin_id: BuiltinFunctionId,
    pub span: Span,
    pub arguments: Vec<TypedIdent<'input>>,
    pub return_type: FunctionReturn<'input>,
    pub meta: Metadata,
}

#[derive(Clone, Debug, Serialize)]
pub struct PropertyDeclaration<'input> {
    /// Doc comment for this property, if any
    pub doc_comment: Option<String>,
    pub name: TypedIdent<'input>,
    pub span: Span,
}

/// A struct type declaration: `struct Point { x: int, y: int }`
#[derive(Clone, Debug, Serialize)]
pub struct StructDeclaration<'input> {
    /// Doc comment for this struct, if any
    pub doc_comment: Option<String>,
    /// The name of the struct as a type (allows parsing `struct int {}` to give better errors)
    pub name: TypeWithLocation<'input>,
    /// The fields of the struct (type is required by grammar)
    pub fields: Vec<TypedIdent<'input>>,
    /// Span of entire declaration
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct GlobalDeclaration<'input> {
    /// Doc comment for this global, if any
    pub doc_comment: Option<String>,
    pub name: TypedIdent<'input>,
    /// The initializer expression. None for uninitialized globals.
    pub value: Option<Expression<'input>>,
    pub span: Span,
}

impl<'input> Script<'input> {
    pub fn from_top_level(
        top_level: impl IntoIterator<Item = TopLevelStatement<'input>>,
        file_id: FileId,
    ) -> Self {
        let mut top_level_function_statements = vec![];
        let mut functions = vec![];
        let mut extern_functions = vec![];
        let mut builtin_functions = vec![];
        let mut globals = vec![];
        let mut property_declarations = vec![];
        let mut struct_declarations = vec![];

        for top_level_statement in top_level.into_iter() {
            match top_level_statement {
                TopLevelStatement::Statement(statement) => {
                    top_level_function_statements.push(statement)
                }
                TopLevelStatement::FunctionDefinition(function) => functions.push(function),
                TopLevelStatement::ExternFunctionDefinition(extern_function) => {
                    extern_functions.push(extern_function)
                }
                TopLevelStatement::BuiltinFunctionDefinition(builtin_function) => {
                    builtin_functions.push(builtin_function)
                }
                TopLevelStatement::GlobalDeclaration(global) => globals.push(global),
                TopLevelStatement::PropertyDeclaration(property) => {
                    property_declarations.push(property)
                }
                TopLevelStatement::StructDeclaration(struct_decl) => {
                    struct_declarations.push(struct_decl)
                }

                TopLevelStatement::Error => {}
            }
        }

        let top_level_function = Function {
            doc_comment: None,
            kind: FunctionKind::Regular,
            name: "@toplevel",
            span: Span::new(file_id, 0, 0),
            statements: top_level_function_statements,
            arguments: vec![],
            return_types: FunctionReturn {
                types: vec![],
                span: Span::new(file_id, 0, 0),
            },
            modifiers: FunctionModifiers::default(),

            meta: Metadata::new(),
        };

        functions.insert(0, top_level_function);

        Self {
            struct_declarations,
            property_declarations,
            globals,
            functions,
            extern_functions,
            builtin_functions,
        }
    }

    /// Merge another script into this one. The other script's definitions
    /// are prepended (so prelude functions come before user functions).
    pub fn merge_from(&mut self, other: Script<'input>) {
        // For functions: keep self's @toplevel at [0], insert other's non-toplevel functions after
        let other_functions: Vec<_> = other
            .functions
            .into_iter()
            .filter(|f| f.name != "@toplevel")
            .collect();

        // Insert after @toplevel
        self.functions.splice(1..1, other_functions);

        // Prepend other declarations
        self.struct_declarations
            .splice(0..0, other.struct_declarations);
        self.property_declarations
            .splice(0..0, other.property_declarations);
        self.globals.splice(0..0, other.globals);
        self.extern_functions.splice(0..0, other.extern_functions);
        self.builtin_functions.splice(0..0, other.builtin_functions);
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct ExternFunctionDefinition<'input> {
    /// Doc comment for this extern function, if any
    pub doc_comment: Option<String>,
    pub name: &'input str,
    pub span: Span,
    pub arguments: Vec<TypedIdent<'input>>,
    pub return_types: FunctionReturn<'input>,

    pub meta: Metadata,
}

/// Information about an operator function definition.
/// Used for operator overloading: `fn Point + Point(a, b) -> Point { ... }`
#[derive(Clone, Debug, Serialize)]
pub struct OperatorDef<'input> {
    /// Left operand type (e.g., "Point", "int")
    pub left_type: Ident<'input>,
    /// The operator being overloaded
    pub op: BinaryOperator,
    /// Right operand type (e.g., "Point", "fix")
    pub right_type: Ident<'input>,
}

/// The kind of function: regular, method, or operator overload.
#[derive(Clone, Debug, Serialize)]
pub enum FunctionKind<'input> {
    /// Regular standalone function: `fn foo()`
    Regular,
    /// Method on a type: `fn Type.method(self)`
    Method {
        /// The receiver type (e.g., "Point", "fix")
        receiver_type: Ident<'input>,
    },
    /// Operator overload: `fn Type + Type(a, b)`
    Operator(OperatorDef<'input>),
}

/// A function definition
#[derive(Clone, Debug, Serialize)]
pub struct Function<'input> {
    /// Doc comment for this function, if any
    pub doc_comment: Option<String>,
    /// The kind of function (regular, method, or operator)
    pub kind: FunctionKind<'input>,
    /// The function/method name (empty string for operators)
    pub name: &'input str,
    /// Span of the function name (or operator for operator overloads)
    pub span: Span,
    /// Function body
    pub statements: Vec<Statement<'input>>,
    /// Function arguments (for methods, first should be `self`)
    pub arguments: Vec<TypedIdent<'input>>,
    /// Return type(s)
    pub return_types: FunctionReturn<'input>,
    /// Modifiers (event not allowed for methods or operators)
    pub modifiers: FunctionModifiers,
    /// Metadata for compilation passes
    pub(crate) meta: Metadata,
}

#[derive(Clone, Debug, Serialize, Default)]
pub struct FunctionModifiers {
    pub is_event_handler: Option<Span>,
}

impl<'input> Function<'input> {
    /// Returns true if this is a method (has a receiver type)
    pub fn is_method(&self) -> bool {
        matches!(self.kind, FunctionKind::Method { .. })
    }

    /// Returns true if this is an operator overload definition
    pub fn is_operator(&self) -> bool {
        matches!(self.kind, FunctionKind::Operator(_))
    }

    /// Returns the receiver type if this is a method, None otherwise
    pub fn receiver_type(&self) -> Option<&Ident<'input>> {
        match &self.kind {
            FunctionKind::Method { receiver_type } => Some(receiver_type),
            _ => None,
        }
    }

    /// Returns the operator definition if this is an operator overload, None otherwise
    pub fn operator_def(&self) -> Option<&OperatorDef<'input>> {
        match &self.kind {
            FunctionKind::Operator(op_def) => Some(op_def),
            _ => None,
        }
    }

    /// Returns the mangled function name.
    /// For operators: "LeftType@op@RightType"
    /// For methods: "Type@method"
    /// For regular functions: just the name
    pub fn mangled_name(&self) -> Cow<'_, str> {
        match &self.kind {
            FunctionKind::Operator(op_def) => Cow::Owned(format!(
                "{}@{}@{}",
                op_def.left_type.ident, op_def.op, op_def.right_type.ident
            )),
            FunctionKind::Method { receiver_type } => {
                Cow::Owned(format!("{}@{}", receiver_type.ident, self.name))
            }
            FunctionKind::Regular => Cow::Borrowed(self.name),
        }
    }
}

impl<'input> BuiltinFunction<'input> {
    /// Returns the mangled function name.
    /// For methods: "Type@method", for regular functions: just the name.
    pub fn mangled_name(&self) -> Cow<'_, str> {
        match &self.receiver_type {
            Some(receiver) => Cow::Owned(format!("{}@{}", receiver.ident, self.name)),
            None => Cow::Borrowed(self.name),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct FunctionReturn<'input> {
    pub types: Vec<TypeWithLocation<'input>>,
    pub span: Span,
}

/// A type annotation in the source code.
///
/// Stores both the original name (for resolution) and the resolved Type.
/// During parsing, `t` is set to `Some(Type)` for builtins (int, fix, bool)
/// or `None` for user-defined types that need resolution.
#[derive(Clone, Debug, Serialize)]
pub struct TypeWithLocation<'input> {
    /// The resolved type. `None` if not yet resolved (user-defined types).
    pub t: Option<Type>,
    /// The original type name from source (e.g., "int", "Point").
    /// Used for resolving user-defined types and error messages.
    pub name: &'input str,
    pub span: Span,
}

impl<'input> TypeWithLocation<'input> {
    /// Returns the resolved type, panicking if not yet resolved.
    /// Use only after type resolution has run.
    pub fn resolved(&self) -> Type {
        self.t.expect("type not yet resolved")
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum TopLevelStatement<'input> {
    Statement(Statement<'input>),
    FunctionDefinition(Function<'input>),
    ExternFunctionDefinition(ExternFunctionDefinition<'input>),
    BuiltinFunctionDefinition(BuiltinFunction<'input>),
    GlobalDeclaration(GlobalDeclaration<'input>),
    PropertyDeclaration(PropertyDeclaration<'input>),
    StructDeclaration(StructDeclaration<'input>),
    Error,
}

#[derive(Clone, Debug, Serialize)]
pub struct Statement<'input> {
    pub span: Span,
    pub kind: StatementKind<'input>,

    pub(crate) meta: Metadata,
}

#[derive(Clone, Debug, Serialize)]
pub struct Ident<'input> {
    pub span: Span,
    pub ident: &'input str,
}

/// An identifier with an optional type annotation.
///
/// Used throughout the AST for any name that may have a type:
/// - Variable declarations: `var x: int = 5;` (type optional, inferred if absent)
/// - Global declarations: `global G: int = 10;` (type optional, inferred if absent)
/// - Function arguments: `fn foo(x: int)` (type required by grammar)
/// - Property declarations: `property p: int;` (type required by grammar)
///
/// Whether the type is required or optional is enforced by the grammar, not the AST.
#[derive(Clone, Debug, Serialize)]
pub struct TypedIdent<'input> {
    pub ident: Ident<'input>,
    pub ty: Option<TypeWithLocation<'input>>,
}

impl<'input> TypedIdent<'input> {
    /// Returns the identifier string.
    pub fn name(&self) -> &'input str {
        self.ident.ident
    }

    /// Returns the span of the identifier.
    pub fn span(&self) -> Span {
        self.ident.span
    }

    /// Returns the type, panicking if not present.
    /// Use only when the grammar guarantees a type exists.
    pub fn ty_required(&self) -> &TypeWithLocation<'input> {
        self.ty.as_ref().expect("type required by grammar")
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub enum StatementKind<'input> {
    Error,
    VariableDeclaration {
        idents: Vec<TypedIdent<'input>>,
        values: Vec<Expression<'input>>,
    },
    Assignment {
        /// Each target is an expression that must be a valid l-value (Variable or FieldAccess)
        targets: Vec<Expression<'input>>,
        values: Vec<Expression<'input>>,
    },
    Wait {
        /// Number of frames to wait. None means 1 frame (original behavior).
        frames: Option<Expression<'input>>,
    },
    Block {
        block: Vec<Statement<'input>>,
    },
    Continue,
    Break,
    #[default]
    Nop,

    If {
        condition: Expression<'input>,
        true_block: Vec<Statement<'input>>,
        false_block: Vec<Statement<'input>>,
    },
    Loop {
        block: Vec<Statement<'input>>,
    },

    /// An expression used as a statement (e.g., function call `foo();`)
    /// The expression is evaluated for its side effects and the result is discarded.
    Expression {
        expression: Box<Expression<'input>>,
    },
    Trigger {
        name: &'input str,
        arguments: Vec<Expression<'input>>,
    },
    Return {
        values: Vec<Expression<'input>>,
    },
}

impl<'input> StatementKind<'input> {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Statement<'input> {
        Statement {
            span: Span::new(file_id, start, end),
            kind: self,
            meta: Metadata::new(),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Expression<'input> {
    pub span: Span,
    pub kind: ExpressionKind<'input>,

    pub(crate) meta: Metadata,
}

impl<'input> Expression<'input> {
    /// For assignment targets, extract the root variable name and span.
    /// Returns Some((name, span)) if this is a valid l-value (Variable or FieldAccess chain).
    /// Returns None if this is not a valid assignment target.
    pub fn as_lvalue_root(&self) -> Option<(&'input str, Span)> {
        match &self.kind {
            ExpressionKind::Variable(name) => Some((name, self.span)),
            ExpressionKind::FieldAccess { base, .. } => base.as_lvalue_root(),
            _ => None,
        }
    }

    /// Returns a mutable reference to the root expression of an l-value.
    /// For a simple variable, returns self. For field access like a.b.c, returns the 'a' expression.
    pub fn lvalue_root_mut(&mut self) -> Option<&mut Self> {
        // Check the variant first to avoid borrow checker issues
        if matches!(self.kind, ExpressionKind::Variable(_)) {
            Some(self)
        } else if let ExpressionKind::FieldAccess { base, .. } = &mut self.kind {
            base.lvalue_root_mut()
        } else {
            None
        }
    }

    /// For assignment targets, extract the field path as a list of (name, span) pairs.
    /// The first element is the root variable, followed by field names.
    /// Returns None if this is not a valid l-value.
    pub fn as_lvalue_path(&self) -> Option<Vec<(&'input str, Span)>> {
        match &self.kind {
            ExpressionKind::Variable(name) => Some(vec![(name, self.span)]),
            ExpressionKind::FieldAccess { base, field } => {
                let mut path = base.as_lvalue_path()?;
                path.push((field.ident, field.span));
                Some(path)
            }
            _ => None,
        }
    }

    pub fn all_inner(&self) -> Box<dyn Iterator<Item = &Expression<'input>> + '_> {
        match &self.kind {
            ExpressionKind::Integer(_)
            | ExpressionKind::Fix(_)
            | ExpressionKind::Bool(_)
            | ExpressionKind::Variable(_)
            | ExpressionKind::Error => Box::new(iter::once(self)),
            ExpressionKind::Nop => Box::new(iter::empty()),
            ExpressionKind::Call { arguments, .. } => Box::new(
                iter::once(self).chain(arguments.iter().flat_map(|argument| argument.all_inner())),
            ),
            ExpressionKind::BinaryOperation { lhs, rhs, .. } => Box::new(
                iter::once(self)
                    .chain(lhs.all_inner())
                    .chain(rhs.all_inner()),
            ),
            ExpressionKind::UnaryOperation { operand, .. } => {
                Box::new(iter::once(self).chain(operand.all_inner()))
            }
            ExpressionKind::FieldAccess { base, .. } => {
                Box::new(iter::once(self).chain(base.all_inner()))
            }
            ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => Box::new(
                iter::once(self)
                    .chain(receiver.all_inner())
                    .chain(arguments.iter().flat_map(|argument| argument.all_inner())),
            ),
            ExpressionKind::Spawn { call } => Box::new(iter::once(self).chain(call.all_inner())),
        }
    }
}

#[derive(Clone, Default, Debug, Serialize)]
pub enum ExpressionKind<'input> {
    Integer(i32),
    Fix(#[serde(skip)] Fix),
    Bool(bool),
    Variable(&'input str),
    BinaryOperation {
        lhs: Box<Expression<'input>>,
        operator: BinaryOperator,
        rhs: Box<Expression<'input>>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression<'input>>,
    },
    Error,
    #[default]
    Nop,

    Call {
        name: &'input str,
        arguments: Vec<Expression<'input>>,
    },

    /// Field access on a struct: `base.field`
    FieldAccess {
        base: Box<Expression<'input>>,
        field: Ident<'input>,
    },

    /// Method call: receiver.method(args)
    MethodCall {
        receiver: Box<Expression<'input>>,
        method: Ident<'input>,
        arguments: Vec<Expression<'input>>,
    },

    /// Spawn a function call as a concurrent task
    Spawn {
        call: Box<Expression<'input>>,
    },
}

impl<'input> ExpressionKind<'input> {
    pub fn with_span(self, file_id: FileId, start: usize, end: usize) -> Expression<'input> {
        Expression {
            kind: self,
            span: Span::new(file_id, start, end),

            meta: Metadata::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Eq)]
pub enum UnaryOperator {
    /// Arithmetic negation: -x
    Neg,
    /// Logical NOT: !x
    Not,
    /// Bitwise NOT: ~x
    BitNot,
}

impl UnaryOperator {
    /// Returns whether this operator can be applied to the given type.
    pub fn can_handle_type(self, operand_type: Type) -> bool {
        match self {
            UnaryOperator::Neg => matches!(operand_type, Type::Int | Type::Fix),
            UnaryOperator::Not => matches!(operand_type, Type::Bool),
            UnaryOperator::BitNot => matches!(operand_type, Type::Int),
        }
    }

    /// Returns the result type of applying this operator to the given type.
    pub fn resulting_type(self, operand_type: Type) -> Type {
        match self {
            UnaryOperator::Neg => operand_type, // int -> int, fix -> fix
            UnaryOperator::Not => Type::Bool,
            UnaryOperator::BitNot => Type::Int,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Neg => "-",
                UnaryOperator::Not => "!",
                UnaryOperator::BitNot => "~",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    RealDiv,
    RealMod,

    FixMul,
    FixDiv,

    Shl,
    Shr,
    BitAnd,
    BitOr,

    EqEq,
    NeEq,
    Gt,
    GtEq,
    Lt,
    LtEq,

    And,
    Or,

    Then,
}

impl BinaryOperator {
    pub fn update_type_with_lhs(&mut self, lhs_type: Type) {
        use BinaryOperator as B;
        match (*self, lhs_type) {
            (B::Mul, Type::Fix) => *self = B::FixMul,
            (B::Div, Type::Fix) => *self = B::FixDiv,
            _ => {}
        }
    }

    pub fn can_handle_type(self, lhs_type: Type) -> bool {
        use BinaryOperator as B;
        match self {
            B::Add
            | B::Sub
            | B::Mul
            | B::Div
            | B::Mod
            | B::RealDiv
            | B::RealMod
            | B::Gt
            | B::GtEq
            | B::Lt
            | B::LtEq => {
                matches!(lhs_type, Type::Fix | Type::Int)
            }
            B::FixMul | B::FixDiv => matches!(lhs_type, Type::Fix),
            B::Shl | B::Shr | B::BitAnd | B::BitOr => matches!(lhs_type, Type::Int),
            B::EqEq | B::NeEq => !matches!(lhs_type, Type::Error),
            B::Then => true,
            B::And | B::Or => matches!(lhs_type, Type::Bool),
        }
    }

    pub fn resulting_type(self, lhs_type: Type, rhs_type: Type) -> Type {
        use BinaryOperator as B;

        match self {
            B::Add
            | B::Sub
            | B::Mul
            | B::Div
            | B::Mod
            | B::RealDiv
            | B::RealMod
            | B::FixMul
            | B::FixDiv
            | B::Shl
            | B::Shr
            | B::BitAnd
            | B::BitOr => lhs_type,

            B::EqEq | B::NeEq | B::Gt | B::GtEq | B::Lt | B::LtEq | B::And | B::Or => Type::Bool,
            B::Then => rhs_type,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Add => "+",
                BinaryOperator::Sub => "-",
                BinaryOperator::Mul => "*",
                BinaryOperator::Div => "/",
                BinaryOperator::Mod => "%",
                BinaryOperator::RealDiv => "//",
                BinaryOperator::RealMod => "%%",
                BinaryOperator::FixMul => "f*",
                BinaryOperator::FixDiv => "f/",
                BinaryOperator::Shl => "<<",
                BinaryOperator::Shr => ">>",
                BinaryOperator::BitAnd => "&",
                BinaryOperator::BitOr => "|",
                BinaryOperator::EqEq => "==",
                BinaryOperator::NeEq => "!=",
                BinaryOperator::Gt => ">",
                BinaryOperator::GtEq => ">=",
                BinaryOperator::Lt => "<",
                BinaryOperator::LtEq => "<=",
                BinaryOperator::Then => "then",
                BinaryOperator::And => "&&",
                BinaryOperator::Or => "||",
            }
        )
    }
}
