use std::{borrow::Cow, collections::HashMap};

use crate::{
    ast::{
        Expression, ExpressionKind, ExternalFunctionId, Function, FunctionId,
        InternalOrExternalFunctionId, MaybeResolved, Script, Statement, StatementKind, SymbolId,
    },
    builtins::BuiltinVariable,
    reporting::{DiagnosticMessage, Diagnostics, ErrorKind},
    tokens::Span,
    types::Type,
};

use super::{CompileSettings, Property};

/// Identifies a global variable by its index in the globals array.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(pub usize);

impl GlobalId {
    const GLOBAL_BIT: u64 = 1 << 62;

    /// Convert this GlobalId to a SymbolId for use in the symbol table.
    pub fn to_symbol_id(self) -> SymbolId {
        SymbolId(Self::GLOBAL_BIT | (self.0 as u64))
    }

    /// Try to extract a GlobalId from a SymbolId.
    /// Returns None if this SymbolId doesn't represent a global.
    pub fn from_symbol_id(id: SymbolId) -> Option<Self> {
        if id.0 & Self::GLOBAL_BIT != 0 && id.0 & BuiltinVariable::RESERVED_BIT == 0 {
            Some(GlobalId((id.0 & !(Self::GLOBAL_BIT)) as usize))
        } else {
            None
        }
    }
}

/// Metadata about a declared global variable.
#[derive(Clone, Debug)]
pub struct GlobalInfo {
    pub id: GlobalId,
    pub name: String,
    pub ty: Type,
    pub initial_value: i32,
    pub span: Span,
}

/// Evaluate a constant initializer expression for a global variable.
/// Returns the type and raw i32 value if successful, or (Type::Error, 0) if not a constant.
fn evaluate_constant_initializer(
    expr: &Expression<'_>,
    diagnostics: &mut Diagnostics,
    global_name: &str,
) -> (Type, i32) {
    match &expr.kind {
        ExpressionKind::Integer(i) => (Type::Int, *i),
        ExpressionKind::Fix(num) => (Type::Fix, num.to_raw()),
        ExpressionKind::Bool(b) => (Type::Bool, *b as i32),
        _ => {
            // Not a constant literal
            ErrorKind::GlobalInitializerNotConstant {
                name: global_name.to_string(),
            }
            .at(expr.span)
            .label(expr.span, DiagnosticMessage::NotAConstant)
            .note(DiagnosticMessage::GlobalInitializersMustBeConstant)
            .emit(diagnostics);
            (Type::Error, 0)
        }
    }
}

pub struct SymTabVisitor<'input> {
    symtab: SymTab<'input>,

    symbol_names: NameTable<'input>,
    function_names: HashMap<&'input str, InternalOrExternalFunctionId>,
}

impl<'input> SymTabVisitor<'input> {
    pub fn new(
        settings: &CompileSettings,
        script: &mut Script<'input>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut function_declarations = HashMap::new();
        let mut function_names = HashMap::new();
        let mut functions_map = HashMap::new();

        for (i, function) in script.extern_functions.iter_mut().enumerate() {
            let fid = ExternalFunctionId(i);
            function.meta.set(fid);

            if let Some(other_span) = function_declarations.insert(function.name, function.span) {
                ErrorKind::FunctionAlreadyDeclared {
                    name: function.name.to_string(),
                }
                .at(function.span)
                .label(other_span, DiagnosticMessage::OriginallyDeclaredHere)
                .label(function.span, DiagnosticMessage::AlsoDeclaredHere)
                .emit(diagnostics);
            }

            functions_map.insert(function.name, InternalOrExternalFunctionId::External(fid));
            function_names.insert(InternalOrExternalFunctionId::External(fid), function.name);
        }

        for (i, function) in script.functions.iter_mut().enumerate() {
            let fid = FunctionId(i);
            function.meta.set(fid);

            if let Some(other_span) = function_declarations.insert(function.name, function.span) {
                ErrorKind::FunctionAlreadyDeclared {
                    name: function.name.to_string(),
                }
                .at(function.span)
                .label(other_span, DiagnosticMessage::OriginallyDeclaredHere)
                .label(function.span, DiagnosticMessage::AlsoDeclaredHere)
                .emit(diagnostics);
            }

            functions_map.insert(function.name, InternalOrExternalFunctionId::Internal(fid));
            function_names.insert(InternalOrExternalFunctionId::Internal(fid), function.name);
        }

        let mut visitor = Self {
            symtab: SymTab::new(settings, function_names),
            symbol_names: NameTable::new(settings),
            function_names: functions_map,
        };

        // Process global declarations
        for (index, global) in script.globals.iter().enumerate() {
            // Check for conflicts with properties
            if settings
                .properties
                .iter()
                .any(|p| p.name == global.name.ident)
            {
                ErrorKind::GlobalConflictsWithProperty {
                    name: global.name.ident.to_string(),
                }
                .at(global.name.span)
                .label(global.name.span, DiagnosticMessage::ConflictsWithProperty)
                .emit(diagnostics);
                continue;
            }

            // Check for conflicts with built-ins (reuse existing error)
            if BuiltinVariable::from_name(global.name.ident).is_some() {
                ErrorKind::CannotShadowBuiltin {
                    name: global.name.ident.to_string(),
                }
                .at(global.name.span)
                .label(
                    global.name.span,
                    DiagnosticMessage::CannotShadowBuiltinLabel,
                )
                .note(DiagnosticMessage::BuiltinVariableNote {
                    name: global.name.ident.to_string(),
                })
                .emit(diagnostics);
                continue;
            }

            // Validate initializer is a constant and infer type
            let (ty, initial_value) =
                evaluate_constant_initializer(&global.value, diagnostics, global.name.ident);

            let global_id = GlobalId(index);
            visitor.symtab.add_global(
                global.name.ident,
                GlobalInfo {
                    id: global_id,
                    name: global.name.ident.to_string(),
                    ty,
                    initial_value,
                    span: global.span,
                },
            );
        }

        visitor
    }

    pub fn get_symtab(&self) -> &SymTab<'input> {
        &self.symtab
    }

    pub fn into_symtab(self) -> SymTab<'input> {
        self.symtab
    }

    pub fn visit_function(
        &mut self,
        function: &mut Function<'input>,
        diagnostics: &mut Diagnostics,
    ) {
        self.symbol_names.push_scope();

        for argument in &mut function.arguments {
            let MaybeResolved::Unresolved(name) = argument.name else {
                panic!("Should not have resolved arguments yet");
            };

            let symbol_id = self.symtab.new_symbol(name, argument.span);
            self.symbol_names.insert(name, symbol_id);

            argument.name = MaybeResolved::Resolved(symbol_id);
        }

        self.visit_block(&mut function.statements, diagnostics);

        self.symbol_names.pop_scope();
    }

    fn visit_block(&mut self, ast: &mut [Statement<'input>], diagnostics: &mut Diagnostics) {
        self.symbol_names.push_scope();

        for statement in ast {
            match &mut statement.kind {
                StatementKind::VariableDeclaration { idents, values } => {
                    // Visit all the value expressions first
                    for value in values {
                        self.visit_expr(value, diagnostics);
                    }

                    // no need to do counting checks, that's done in type checking
                    let mut statement_meta = vec![];
                    for ident in idents {
                        if BuiltinVariable::from_name(ident.ident).is_some() {
                            ErrorKind::CannotShadowBuiltin {
                                name: ident.ident.to_string(),
                            }
                            .at(ident.span)
                            .label(ident.span, DiagnosticMessage::CannotShadowBuiltinLabel)
                            .note(DiagnosticMessage::BuiltinVariableNote {
                                name: ident.ident.to_string(),
                            })
                            .emit(diagnostics);
                        }

                        let symbol_id = self.symtab.new_symbol(ident.ident, ident.span);
                        self.symbol_names.insert(ident.ident, symbol_id);

                        statement_meta.push(symbol_id);
                    }

                    statement.meta.set(statement_meta);
                }
                StatementKind::Assignment { idents, values } => {
                    // Visit all the value expressions first
                    for value in values {
                        self.visit_expr(value, diagnostics);
                    }

                    // no need to do counting checks, that's done in type checking
                    let mut statement_meta = vec![];
                    for ident in idents {
                        if BuiltinVariable::from_name(ident.ident).is_some() {
                            ErrorKind::CannotShadowBuiltin {
                                name: ident.ident.to_string(),
                            }
                            .at(ident.span)
                            .label(ident.span, DiagnosticMessage::CannotShadowBuiltinLabel)
                            .note(DiagnosticMessage::BuiltinVariableNote {
                                name: ident.ident.to_string(),
                            })
                            .emit(diagnostics);
                        }

                        if let Some(symbol_id) = self.symbol_names.get(ident.ident, &self.symtab) {
                            statement_meta.push(symbol_id);
                        } else {
                            ErrorKind::UnknownVariable {
                                name: ident.ident.to_string(),
                            }
                            .at(ident.span)
                            .label(ident.span, DiagnosticMessage::UnknownVariableLabel)
                            .emit(diagnostics);

                            // create a dummy symbol to ensure that the meta stays correct
                            statement_meta.push(self.symtab.new_symbol(ident.ident, ident.span));
                        }
                    }

                    statement.meta.set(statement_meta);
                }
                StatementKind::If {
                    condition,
                    true_block,
                    false_block,
                } => {
                    self.visit_expr(condition, diagnostics);

                    self.visit_block(true_block, diagnostics);
                    self.visit_block(false_block, diagnostics);
                }
                StatementKind::Error
                | StatementKind::Wait
                | StatementKind::Nop
                | StatementKind::Continue
                | StatementKind::Break => {}
                StatementKind::Return { values } => {
                    for expr in values {
                        self.visit_expr(expr, diagnostics);
                    }
                }
                StatementKind::Block { block } => {
                    self.visit_block(block, diagnostics);
                }
                StatementKind::Call { arguments, name }
                | StatementKind::Spawn { arguments, name } => {
                    if let Some(function) = self.function_names.get(name) {
                        statement.meta.set(*function);
                    } else {
                        ErrorKind::UnknownFunction {
                            name: name.to_string(),
                        }
                        .at(statement.span)
                        .label(statement.span, DiagnosticMessage::UnknownFunctionLabel)
                        .emit(diagnostics);
                    }

                    for argument in arguments {
                        self.visit_expr(argument, diagnostics);
                    }
                }
                StatementKind::Trigger { arguments, .. } => {
                    for argument in arguments {
                        self.visit_expr(argument, diagnostics);
                    }
                }
                StatementKind::Loop { block } => {
                    self.visit_block(block, diagnostics);
                }
            };
        }

        self.symbol_names.pop_scope();
    }

    fn visit_expr(&self, expr: &mut Expression<'_>, diagnostics: &mut Diagnostics) {
        match &mut expr.kind {
            ExpressionKind::Variable(ident) => {
                if let Some(symbol_id) = self.symbol_names.get(ident, &self.symtab) {
                    expr.meta.set(symbol_id);
                } else {
                    ErrorKind::UnknownVariable {
                        name: ident.to_string(),
                    }
                    .at(expr.span)
                    .label(expr.span, DiagnosticMessage::UnknownVariableLabel)
                    .emit(diagnostics);
                }
            }
            ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
                self.visit_expr(lhs, diagnostics);
                self.visit_expr(rhs, diagnostics);
            }
            ExpressionKind::Call { arguments, name } => {
                if let Some(function) = self.function_names.get(name) {
                    expr.meta.set(*function);
                } else {
                    ErrorKind::UnknownFunction {
                        name: name.to_string(),
                    }
                    .at(expr.span)
                    .label(expr.span, DiagnosticMessage::UnknownFunctionLabel)
                    .emit(diagnostics);
                }

                for argument in arguments {
                    self.visit_expr(argument, diagnostics);
                }
            }
            ExpressionKind::Integer(_)
            | ExpressionKind::Fix(_)
            | ExpressionKind::Error
            | ExpressionKind::Nop
            | ExpressionKind::Bool(_) => {}
        }
    }
}

struct NameTable<'input> {
    names: Vec<HashMap<Cow<'input, str>, SymbolId>>,
}

impl<'input> NameTable<'input> {
    pub fn new(settings: &CompileSettings) -> Self {
        let property_symbols = settings
            .properties
            .iter()
            .enumerate()
            .map(|(i, prop)| (Cow::Owned(prop.name.clone()), SymbolId(i as u64)))
            .collect();

        Self {
            names: vec![property_symbols],
        }
    }

    pub fn insert(&mut self, name: &'input str, id: SymbolId) {
        self.names
            .last_mut()
            .unwrap()
            .insert(Cow::Borrowed(name), id);
    }

    pub fn get(&self, name: &str, symtab: &SymTab) -> Option<SymbolId> {
        // Check built-ins first (cannot be shadowed)
        if let Some(builtin) = BuiltinVariable::from_name(name) {
            return Some(builtin.symbol_id());
        }

        // Check local variables and properties (can shadow globals)
        for nametab in self.names.iter().rev() {
            if let Some(id) = nametab.get(name) {
                return Some(*id);
            }
        }

        // Check globals last (can be shadowed by locals)
        if let Some(global) = symtab.get_global_by_name(name) {
            return Some(global.id.to_symbol_id());
        }

        None
    }

    pub fn push_scope(&mut self) {
        self.names.push(HashMap::new())
    }

    pub fn pop_scope(&mut self) {
        self.names.pop();
        assert!(!self.names.is_empty());
    }
}

pub struct SymTab<'input> {
    properties: Vec<Property>,

    symbol_names: Vec<(Cow<'input, str>, Option<Span>)>,
    function_names: HashMap<InternalOrExternalFunctionId, &'input str>,

    globals: Vec<GlobalInfo>,
    global_names: HashMap<Cow<'input, str>, GlobalId>,
}

impl<'input> SymTab<'input> {
    fn new(
        settings: &CompileSettings,
        function_names: HashMap<InternalOrExternalFunctionId, &'input str>,
    ) -> Self {
        let properties = settings.properties.clone();
        let symbol_names = properties
            .iter()
            .map(|prop| (Cow::Owned(prop.name.clone()), None))
            .collect();

        Self {
            properties,
            symbol_names,
            function_names,
            globals: vec![],
            global_names: HashMap::new(),
        }
    }

    fn new_symbol(&mut self, ident: &'input str, span: Span) -> SymbolId {
        self.symbol_names.push((Cow::Borrowed(ident), Some(span)));
        SymbolId((self.symbol_names.len() - 1) as u64)
    }

    pub(crate) fn new_rename(&mut self, symbol_id: SymbolId) -> SymbolId {
        self.symbol_names
            .push(self.symbol_names[symbol_id.0 as usize].clone());
        SymbolId((self.symbol_names.len() - 1) as u64)
    }

    pub(crate) fn new_temporary(&mut self) -> SymbolId {
        let id = self.symbol_names.len();
        self.symbol_names.push((Cow::Borrowed(""), None));
        SymbolId(id as u64)
    }

    pub(crate) fn name_for_symbol(&self, symbol_id: SymbolId) -> Cow<'input, str> {
        let name = self.symbol_names[symbol_id.0 as usize].0.clone();
        if name.is_empty() {
            Cow::Owned(format!("temp.{}", symbol_id.0))
        } else {
            name
        }
    }

    #[cfg(test)]
    pub(crate) fn debug_name_for_symbol(&self, symbol_id: SymbolId) -> String {
        let name = self.symbol_names[symbol_id.0 as usize].0.clone();
        if name.is_empty() {
            format!("temp.{}", symbol_id.0)
        } else {
            format!("{name}.{}", symbol_id.0)
        }
    }

    pub(crate) fn name_for_function(
        &self,
        function_id: InternalOrExternalFunctionId,
    ) -> &'input str {
        self.function_names
            .get(&function_id)
            .expect("Should have a function name if you have an InternalOrExternalFunctionId")
    }

    pub(crate) fn span_for_symbol(&self, symbol_id: SymbolId) -> Span {
        self.symbol_names[symbol_id.0 as usize]
            .1
            .expect("Symbol should have a span")
    }

    #[cfg(test)]
    pub fn all_symbols(&self) -> impl Iterator<Item = (&'_ str, SymbolId)> + '_ {
        self.symbol_names
            .iter()
            .enumerate()
            .map(|(i, (name, _span))| (name.as_ref(), SymbolId(i as u64)))
    }

    pub fn get_property(&self, symbol_id: SymbolId) -> Option<&Property> {
        self.properties.get(symbol_id.0 as usize)
    }

    pub fn get_global_by_name(&self, name: &str) -> Option<&GlobalInfo> {
        self.global_names.get(name).map(|id| &self.globals[id.0])
    }

    pub fn get_global(&self, id: GlobalId) -> &GlobalInfo {
        &self.globals[id.0]
    }

    pub fn globals(&self) -> &[GlobalInfo] {
        &self.globals
    }

    fn add_global(&mut self, name: &'input str, info: GlobalInfo) {
        self.global_names.insert(Cow::Borrowed(name), info.id);
        self.globals.push(info);
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{grammar, lexer::Lexer, tokens::FileId, types::Type};

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "symtab_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let mut visitor = SymTabVisitor::new(
                &CompileSettings {
                    properties: vec![Property {
                        ty: Type::Int,
                        index: 0,
                        name: "int_prop".to_string(),
                    }],
                    enable_optimisations: false,
                },
                &mut script,
                &mut diagnostics,
            );

            for function in &mut script.functions {
                visitor.visit_function(function, &mut diagnostics);
            }

            assert_ron_snapshot!(script, {
                ".**.span" => "[span]",
            });
        });
    }

    #[test]
    fn symtab_fail_snapshot_tests() {
        glob!("snapshot_tests", "symtab_visitor/*_fail.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let file_id = FileId::new(0);
            let lexer = Lexer::new(&input, file_id);
            let parser = grammar::ScriptParser::new();

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let mut visitor = SymTabVisitor::new(
                &CompileSettings {
                    properties: vec![Property {
                        ty: Type::Int,
                        index: 0,
                        name: "int_prop".to_string(),
                    }],
                    enable_optimisations: false,
                },
                &mut script,
                &mut diagnostics,
            );

            for function in &mut script.functions {
                visitor.visit_function(function, &mut diagnostics);
            }

            assert_snapshot!(diagnostics.pretty_string(false));
        });
    }
}
