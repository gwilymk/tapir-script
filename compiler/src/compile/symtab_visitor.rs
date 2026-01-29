use std::{borrow::Cow, collections::HashMap};

use crate::{
    ast::{
        BuiltinFunctionId, Expression, ExpressionKind, ExternalFunctionId, Function, FunctionId,
        InternalOrExternalFunctionId, PropertyDeclaration, Script, Statement, StatementKind,
        SymbolId,
    },
    prelude::PRELUDE_FILE_ID,
    reporting::{DiagnosticMessage, Diagnostics, ErrorKind},
    tokens::Span,
    types::{StructId, StructRegistry, Type},
};

/// Represents the expansion of a struct-typed symbol into its component field symbols.
///
/// When a struct variable is assigned (e.g., `var p = Point(1, 2)`), we expand it into
/// symbols for each field. Scalar fields get a SymbolId directly; struct-typed fields
/// get their own ExpandedStruct entry in the symbol table.
#[derive(Clone, Debug)]
pub struct ExpandedStruct {
    /// The struct type this expansion represents.
    pub struct_id: StructId,
    /// One entry per field in declaration order. For scalar fields, contains the SymbolId.
    /// For struct-typed fields, contains the SymbolId of the nested struct (which has
    /// its own ExpandedStruct entry in the symbol table).
    pub fields: Vec<SymbolId>,
}

/// Stores resolved symbol IDs for function arguments.
/// This replaces the old MaybeResolved approach with an immutable AST pattern.
#[derive(Clone, Debug)]
pub struct FunctionArgumentSymbols(pub Vec<SymbolId>);

use super::{CompileSettings, Property, StructPropertyInfo};

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
        if id.0 & Self::GLOBAL_BIT != 0 {
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

/// Metadata about a declared builtin function.
#[derive(Clone, Debug)]
pub struct BuiltinFunctionInfo {
    pub name: String,
    pub return_type: Type,
}

use crate::ast::GlobalDeclaration;

/// Collect all leaf (scalar) types from a struct type recursively.
fn collect_struct_field_types(struct_id: StructId, registry: &StructRegistry, out: &mut Vec<Type>) {
    let def = registry.get(struct_id);
    for field in &def.fields {
        if let Type::Struct(nested_id) = field.ty {
            collect_struct_field_types(nested_id, registry, out);
        } else {
            out.push(field.ty);
        }
    }
}

/// Recursively expand a struct-typed property into its component scalar properties.
/// Each field becomes a property with a path name like "pos.x", "pos.y".
fn expand_property_fields(
    path: &str,
    rust_field_name: &str,
    root_struct_id: StructId,
    current_struct_id: StructId,
    current_index: &mut usize,
    tuple_position: &mut usize,
    registry: &StructRegistry,
    field_types: &[Type],
    span: Span,
    properties: &mut Vec<Property>,
) {
    let def = registry.get(current_struct_id);

    for field in &def.fields {
        let field_path = format!("{}.{}", path, field.name);

        match field.ty {
            Type::Struct(nested_id) => {
                // Recurse for nested structs
                expand_property_fields(
                    &field_path,
                    rust_field_name,
                    root_struct_id,
                    nested_id,
                    current_index,
                    tuple_position,
                    registry,
                    field_types,
                    span,
                    properties,
                );
            }
            scalar_type => {
                properties.push(Property {
                    ty: scalar_type,
                    index: *current_index,
                    name: field_path,
                    span,
                    struct_info: Some(StructPropertyInfo {
                        rust_field_name: rust_field_name.to_string(),
                        tuple_position: *tuple_position,
                        field_types: field_types.to_vec().into_boxed_slice(),
                        struct_id: root_struct_id,
                    }),
                });
                *current_index += 1;
                *tuple_position += 1;
            }
        }
    }
}

/// Extract properties from AST property declarations with validation.
/// Struct-typed properties are expanded to consecutive property indices.
fn extract_properties_from_ast(
    declarations: &[PropertyDeclaration<'_>],
    globals: &[GlobalDeclaration<'_>],
    available_fields: Option<&[String]>,
    struct_registry: &StructRegistry,
    diagnostics: &mut Diagnostics,
) -> Vec<Property> {
    let mut properties = Vec::new();
    let mut seen_names: HashMap<&str, Span> = HashMap::new();
    let mut current_index = 0;

    for decl in declarations {
        // Skip properties with unresolved or error types
        let prop_ty = decl.name.ty_required().resolved();
        if prop_ty == Type::Error {
            continue;
        }

        let name = decl.name.name();
        let name_span = decl.name.span();

        // Check for duplicate property names
        if let Some(first_span) = seen_names.get(name) {
            ErrorKind::DuplicatePropertyDeclaration {
                name: name.to_string(),
            }
            .at(name_span)
            .label(*first_span, DiagnosticMessage::OriginallyDeclaredHere)
            .label(name_span, DiagnosticMessage::PropertyAlreadyDeclared)
            .emit(diagnostics);
            continue;
        }
        seen_names.insert(name, name_span);

        // Check for conflicts with globals
        if let Some(global) = globals.iter().find(|g| g.name.name() == name) {
            ErrorKind::PropertyConflictsWithGlobal {
                name: name.to_string(),
            }
            .at(name_span)
            .label(
                global.name.span(),
                DiagnosticMessage::OriginallyDeclaredHere,
            )
            .label(name_span, DiagnosticMessage::ConflictsWithGlobal)
            .emit(diagnostics);
            // Continue anyway to report more errors
        }

        // Validate property exists in available fields (if provided)
        if let Some(fields) = available_fields
            && !fields.iter().any(|f| f == name)
        {
            ErrorKind::PropertyNotInStruct {
                name: name.to_string(),
            }
            .at(name_span)
            .label(name_span, DiagnosticMessage::PropertyNotInStructLabel)
            .emit(diagnostics);
            // Continue anyway to report more errors
        }

        // Check if this is a struct-typed property
        if let Type::Struct(struct_id) = prop_ty {
            // Collect all leaf field types first
            let mut field_types = Vec::new();
            collect_struct_field_types(struct_id, struct_registry, &mut field_types);

            // Expand struct property into its component scalar properties
            let mut tuple_position = 0;
            expand_property_fields(
                name,
                name,
                struct_id,
                struct_id,
                &mut current_index,
                &mut tuple_position,
                struct_registry,
                &field_types,
                decl.span,
                &mut properties,
            );
        } else {
            // Scalar property - add directly
            properties.push(Property {
                ty: prop_ty,
                index: current_index,
                name: name.to_string(),
                span: decl.span,
                struct_info: None,
            });
            current_index += 1;
        }
    }

    properties
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
    function_names: HashMap<String, InternalOrExternalFunctionId>,
}

/// Helper to register a function in the declaration maps.
/// Returns true if registration succeeded, false if a duplicate was found.
fn register_function(
    name: String,
    span: Span,
    id: InternalOrExternalFunctionId,
    function_declarations: &mut HashMap<String, Span>,
    functions_map: &mut HashMap<String, InternalOrExternalFunctionId>,
    function_names: &mut HashMap<InternalOrExternalFunctionId, String>,
    diagnostics: &mut Diagnostics,
) -> bool {
    if let Some(other_span) = function_declarations.get(&name) {
        ErrorKind::FunctionAlreadyDeclared { name }
            .at(span)
            .label(*other_span, DiagnosticMessage::OriginallyDeclaredHere)
            .label(span, DiagnosticMessage::AlsoDeclaredHere)
            .emit(diagnostics);
        return false;
    }

    function_declarations.insert(name.clone(), span);
    functions_map.insert(name.clone(), id);
    function_names.insert(id, name);
    true
}

impl<'input> SymTabVisitor<'input> {
    pub fn new(
        settings: &CompileSettings,
        script: &mut Script<'input>,
        struct_registry: &StructRegistry,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut function_declarations = HashMap::new();
        let mut function_names = HashMap::new();
        let mut functions_map = HashMap::new();

        // Register struct constructors (implicit functions for each struct)
        for (i, struct_def) in struct_registry.iter().enumerate() {
            let struct_id = StructId(i as u32);
            register_function(
                struct_def.name.clone(),
                struct_def.span,
                InternalOrExternalFunctionId::StructConstructor(struct_id),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            );
        }

        for (i, function) in script.extern_functions.iter_mut().enumerate() {
            let fid = ExternalFunctionId(i);
            function.meta.set(fid);
            register_function(
                function.name.to_string(),
                function.span,
                InternalOrExternalFunctionId::External(fid),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            );
        }

        let mut builtin_function_infos = HashMap::new();

        for function in script.builtin_functions.iter_mut() {
            let fid = function.builtin_id;
            function.meta.set(fid);

            // Builtin functions can only be declared in the prelude
            if function.span.file_id != PRELUDE_FILE_ID {
                ErrorKind::BuiltinOutsidePrelude {
                    name: function.name.to_string(),
                }
                .at(function.span)
                .emit(diagnostics);
                continue;
            }

            // Use mangled_name() - returns "Type@method" for methods, "name" for functions
            if !register_function(
                function.mangled_name().into_owned(),
                function.span,
                InternalOrExternalFunctionId::Builtin(fid),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            ) {
                continue;
            }

            // Extract return type (builtins should have exactly one return type)
            let return_type = function
                .return_type
                .types
                .first()
                .map(|t| t.resolved())
                .unwrap_or(Type::Error);

            builtin_function_infos.insert(
                fid,
                BuiltinFunctionInfo {
                    name: function.name.to_string(),
                    return_type,
                },
            );
        }

        for (i, function) in script.functions.iter_mut().enumerate() {
            let fid = FunctionId(i);
            function.meta.set(fid);

            // Check for event modifier on methods (not allowed)
            if function.is_method() && function.modifiers.is_event_handler.is_some() {
                ErrorKind::MethodCannotBeEventHandler {
                    method_name: function.name.to_string(),
                }
                .at(function.modifiers.is_event_handler.unwrap())
                .label(
                    function.modifiers.is_event_handler.unwrap(),
                    DiagnosticMessage::MethodCannotBeEventHandlerLabel,
                )
                .label(function.span, DiagnosticMessage::MethodDefinedHere)
                .emit(diagnostics);
                continue;
            }

            // Use mangled_name() - returns "Type@method" for methods, "name" for functions
            register_function(
                function.mangled_name().into_owned(),
                function.span,
                InternalOrExternalFunctionId::Internal(fid),
                &mut function_declarations,
                &mut functions_map,
                &mut function_names,
                diagnostics,
            );
        }

        // Extract properties from AST property declarations
        let properties = extract_properties_from_ast(
            &script.property_declarations,
            &script.globals,
            settings.available_fields.as_deref(),
            struct_registry,
            diagnostics,
        );

        let mut visitor = Self {
            symtab: SymTab::new(
                &properties,
                function_names,
                functions_map.clone(),
                builtin_function_infos,
            ),
            symbol_names: NameTable::new(
                &properties,
                &struct_property_bases_for_name_table(&properties),
            ),
            function_names: functions_map,
        };

        // Process global declarations
        for (index, global) in script.globals.iter().enumerate() {
            let name = global.name.name();

            // Skip globals that conflict with properties (error already reported in extract_properties_from_ast)
            if properties.iter().any(|p| p.name == name) {
                continue;
            }

            // Validate initializer is a constant and infer type
            let (ty, initial_value) =
                evaluate_constant_initializer(&global.value, diagnostics, name);

            let global_id = GlobalId(index);
            visitor.symtab.add_global(
                name,
                GlobalInfo {
                    id: global_id,
                    name: name.to_string(),
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

        let mut argument_symbols = Vec::with_capacity(function.arguments.len());
        for argument in &function.arguments {
            let name = argument.name();
            let symbol_id = self.symtab.new_symbol(name, argument.span());
            self.symbol_names.insert(name, symbol_id);
            argument_symbols.push(symbol_id);
        }
        function.meta.set(FunctionArgumentSymbols(argument_symbols));

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
                        let name = ident.name();
                        let span = ident.span();

                        let symbol_id = self.symtab.new_symbol(name, span);
                        self.symbol_names.insert(name, symbol_id);

                        statement_meta.push(symbol_id);
                    }

                    statement.meta.set(statement_meta);
                }
                StatementKind::Assignment { targets, values } => {
                    // Visit all the value expressions first
                    for value in values {
                        self.visit_expr(value, diagnostics);
                    }

                    // no need to do counting checks, that's done in type checking
                    // For each target path, resolve the root variable (first ident)
                    let mut statement_meta = vec![];
                    for path in targets {
                        if let Some(first) = path.first() {
                            if let Some(symbol_id) =
                                self.symbol_names.get(first.ident, &self.symtab)
                            {
                                statement_meta.push(symbol_id);
                            } else {
                                ErrorKind::UnknownVariable {
                                    name: first.ident.to_string(),
                                }
                                .at(first.span)
                                .label(first.span, DiagnosticMessage::UnknownVariableLabel)
                                .emit(diagnostics);

                                // create a dummy symbol to ensure that the meta stays correct
                                statement_meta
                                    .push(self.symtab.new_symbol(first.ident, first.span));
                            }
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
                StatementKind::Expression { expression } => {
                    self.visit_expr(expression, diagnostics);
                }
                StatementKind::Spawn { arguments, name } => {
                    if let Some(function) = self.function_names.get(*name) {
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
                if let Some(function) = self.function_names.get(*name) {
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
            ExpressionKind::FieldAccess { base, .. } => {
                // Visit the base expression; field resolution happens during type checking
                self.visit_expr(base, diagnostics);
            }
            ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                // Visit receiver and arguments; method resolution happens during type checking
                self.visit_expr(receiver, diagnostics);
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

/// Helper to extract struct property base names for NameTable initialization.
fn struct_property_bases_for_name_table(properties: &[Property]) -> HashMap<String, StructId> {
    let mut bases = HashMap::new();
    for prop in properties {
        if let Some(ref info) = prop.struct_info {
            bases
                .entry(info.rust_field_name.clone())
                .or_insert(info.struct_id);
        }
    }
    bases
}

struct NameTable<'input> {
    names: Vec<HashMap<Cow<'input, str>, SymbolId>>,
    /// Maps struct property base names to their struct type.
    /// These are "virtual" symbols used during type checking for field access.
    struct_property_bases: HashMap<String, StructId>,
}

/// Marker bit for struct property base symbols.
/// These symbols don't correspond to actual values but are used
/// to resolve field access on struct properties during type checking.
pub const STRUCT_PROPERTY_BASE_BIT: u64 = 1 << 61;

impl<'input> NameTable<'input> {
    pub fn new(properties: &[Property], struct_property_bases: &HashMap<String, StructId>) -> Self {
        let property_symbols = properties
            .iter()
            .enumerate()
            .map(|(i, prop)| (Cow::Owned(prop.name.clone()), SymbolId(i as u64)))
            .collect();

        Self {
            names: vec![property_symbols],
            struct_property_bases: struct_property_bases.clone(),
        }
    }

    pub fn insert(&mut self, name: &'input str, id: SymbolId) {
        self.names
            .last_mut()
            .unwrap()
            .insert(Cow::Borrowed(name), id);
    }

    pub fn get(&self, name: &str, symtab: &SymTab) -> Option<SymbolId> {
        // Check local variables and properties (can shadow globals)
        for nametab in self.names.iter().rev() {
            if let Some(id) = nametab.get(name) {
                return Some(*id);
            }
        }

        // Check struct property bases (e.g., "pos" for "property pos: Point;")
        if let Some(&struct_id) = self.struct_property_bases.get(name) {
            // Return a special symbol ID that encodes the struct property base
            return Some(SymbolId(STRUCT_PROPERTY_BASE_BIT | struct_id.0 as u64));
        }

        // Check globals last (can be shadowed by locals)
        if let Some(global) = symtab.get_global_by_name(name) {
            return Some(global.id.to_symbol_id());
        }

        None
    }

    /// Check if a name is a struct property base.
    #[allow(dead_code)]
    pub fn is_struct_property_base(&self, name: &str) -> bool {
        self.struct_property_bases.contains_key(name)
    }

    /// Get the struct ID for a struct property base name.
    #[allow(dead_code)]
    pub fn get_struct_property_base(&self, name: &str) -> Option<StructId> {
        self.struct_property_bases.get(name).copied()
    }

    pub fn push_scope(&mut self) {
        self.names.push(HashMap::new())
    }

    pub fn pop_scope(&mut self) {
        self.names.pop();
        assert!(!self.names.is_empty());
    }
}

/// Information about a struct property base (e.g., "pos" for "property pos: Point;").
#[derive(Clone, Debug)]
pub struct StructPropertyBase {
    pub struct_id: StructId,
    /// The indices of all expanded property fields for this struct property.
    /// These are property indices, not symbol IDs.
    pub expanded_indices: Vec<usize>,
    pub span: Span,
}

pub struct SymTab<'input> {
    properties: Vec<Property>,

    symbol_names: Vec<(Cow<'input, str>, Option<Span>)>,
    function_names: HashMap<InternalOrExternalFunctionId, String>,
    /// Reverse lookup: function/method name to ID (uses mangled names for methods)
    functions_by_name: HashMap<String, InternalOrExternalFunctionId>,

    globals: Vec<GlobalInfo>,
    global_names: HashMap<Cow<'input, str>, GlobalId>,

    builtin_functions: HashMap<BuiltinFunctionId, BuiltinFunctionInfo>,

    /// Tracks struct expansions created during IR lowering.
    /// Maps the "parent" symbol (the struct variable itself) to its expanded field symbols.
    struct_expansions: HashMap<SymbolId, ExpandedStruct>,

    /// Tracks struct property bases (e.g., "pos" for "property pos: Point;").
    /// Maps the base property name to its struct information.
    struct_property_bases: HashMap<String, StructPropertyBase>,
}

impl<'input> SymTab<'input> {
    fn new(
        properties: &[Property],
        function_names: HashMap<InternalOrExternalFunctionId, String>,
        functions_by_name: HashMap<String, InternalOrExternalFunctionId>,
        builtin_functions: HashMap<BuiltinFunctionId, BuiltinFunctionInfo>,
    ) -> Self {
        let properties = properties.to_vec();
        let symbol_names = properties
            .iter()
            .map(|prop| (Cow::Owned(prop.name.clone()), None))
            .collect();

        // Build struct property bases from the expanded properties
        let mut struct_property_bases = HashMap::new();
        for prop in &properties {
            if let Some(ref info) = prop.struct_info {
                // Use the rust_field_name as the base name
                let base_name = &info.rust_field_name;
                struct_property_bases
                    .entry(base_name.clone())
                    .or_insert_with(|| StructPropertyBase {
                        struct_id: info.struct_id,
                        expanded_indices: Vec::new(),
                        span: prop.span,
                    })
                    .expanded_indices
                    .push(prop.index);
            }
        }

        Self {
            properties,
            symbol_names,
            function_names,
            functions_by_name,
            globals: vec![],
            global_names: HashMap::new(),
            builtin_functions,
            struct_expansions: HashMap::new(),
            struct_property_bases,
        }
    }

    /// Look up a function/method by its mangled name.
    /// For methods, use "Type@method" format (e.g., "Point@distance", "fix@round").
    pub fn function_by_mangled_name(&self, mangled: &str) -> Option<InternalOrExternalFunctionId> {
        self.functions_by_name.get(mangled).copied()
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

    /// Create a new symbol with an owned name (for generated field symbols).
    fn new_symbol_owned(&mut self, name: String, span: Span) -> SymbolId {
        self.symbol_names.push((Cow::Owned(name), Some(span)));
        SymbolId((self.symbol_names.len() - 1) as u64)
    }

    /// Expand a struct-typed symbol into its component field symbols.
    ///
    /// Creates symbols for each field. Scalar fields get a direct SymbolId.
    /// Struct-typed fields get their own ExpandedStruct entry (recursive).
    ///
    /// This is called during IR lowering when assigning to a struct variable.
    pub fn expand_struct_symbol(
        &mut self,
        parent_symbol: SymbolId,
        base_name: &str,
        struct_id: StructId,
        span: Span,
        registry: &StructRegistry,
    ) -> ExpandedStruct {
        // Check if already expanded
        if let Some(existing) = self.struct_expansions.get(&parent_symbol) {
            return existing.clone();
        }

        let def = registry.get(struct_id);
        let mut fields = Vec::with_capacity(def.fields.len());

        for field in &def.fields {
            let field_name = format!("{}.{}", base_name, field.name);

            match field.ty {
                Type::Struct(nested_id) => {
                    // Create a symbol for the nested struct and expand it recursively
                    let nested_symbol = self.new_symbol_owned(field_name.clone(), span);
                    self.expand_struct_symbol(
                        nested_symbol,
                        &field_name,
                        nested_id,
                        span,
                        registry,
                    );
                    fields.push(nested_symbol);
                }
                _ => {
                    // Scalar field - just create a symbol
                    let symbol_id = self.new_symbol_owned(field_name, span);
                    fields.push(symbol_id);
                }
            }
        }

        let expansion = ExpandedStruct { struct_id, fields };
        self.struct_expansions
            .insert(parent_symbol, expansion.clone());
        expansion
    }

    /// Get the struct expansion for a symbol, if it exists.
    pub fn get_struct_expansion(&self, symbol_id: SymbolId) -> Option<&ExpandedStruct> {
        self.struct_expansions.get(&symbol_id)
    }

    pub(crate) fn name_for_symbol(&self, symbol_id: SymbolId) -> Cow<'input, str> {
        // Check if this is a struct property base symbol
        if symbol_id.0 & STRUCT_PROPERTY_BASE_BIT != 0 {
            let struct_id = StructId((symbol_id.0 & !(STRUCT_PROPERTY_BASE_BIT)) as u32);
            // Find the property base name for this struct_id
            for (name, base) in &self.struct_property_bases {
                if base.struct_id == struct_id {
                    return Cow::Owned(name.clone());
                }
            }
            return Cow::Owned(format!("struct_property.{}", struct_id.0));
        }

        if symbol_id.0 as usize >= self.symbol_names.len() {
            return Cow::Owned(format!("unknown.{}", symbol_id.0));
        }

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

    pub(crate) fn name_for_function(&self, function_id: InternalOrExternalFunctionId) -> &str {
        self.function_names
            .get(&function_id)
            .expect("Should have a function name if you have an InternalOrExternalFunctionId")
    }

    pub(crate) fn builtin_function_info(
        &self,
        builtin_id: BuiltinFunctionId,
    ) -> Option<&BuiltinFunctionInfo> {
        self.builtin_functions.get(&builtin_id)
    }

    pub(crate) fn span_for_symbol(&self, symbol_id: SymbolId) -> Span {
        // Check if this is a struct property base symbol
        if symbol_id.0 & STRUCT_PROPERTY_BASE_BIT != 0 {
            let struct_id = StructId((symbol_id.0 & !(STRUCT_PROPERTY_BASE_BIT)) as u32);
            for base in self.struct_property_bases.values() {
                if base.struct_id == struct_id {
                    return base.span;
                }
            }
            panic!(
                "Struct property base not found for struct_id {}",
                struct_id.0
            );
        }

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
        // Don't return properties for struct property bases or globals
        if symbol_id.0 & STRUCT_PROPERTY_BASE_BIT != 0 || symbol_id.0 & GlobalId::GLOBAL_BIT != 0 {
            return None;
        }
        self.properties.get(symbol_id.0 as usize)
    }

    /// Get a property by its full path name (e.g., "pos.x").
    pub fn get_property_by_path(&self, path: &str) -> Option<&Property> {
        self.properties.iter().find(|p| p.name == path)
    }

    pub fn properties(&self) -> &[Property] {
        &self.properties
    }

    /// Get struct property base info by name.
    pub fn get_struct_property_base(&self, name: &str) -> Option<&StructPropertyBase> {
        self.struct_property_bases.get(name)
    }

    /// Iterate over all struct property bases.
    pub fn struct_property_bases(&self) -> impl Iterator<Item = (&str, &StructPropertyBase)> {
        self.struct_property_bases
            .iter()
            .map(|(name, base)| (name.as_str(), base))
    }

    /// Check if a symbol ID represents a struct property base.
    pub fn is_struct_property_base_symbol(symbol_id: SymbolId) -> bool {
        symbol_id.0 & STRUCT_PROPERTY_BASE_BIT != 0
    }

    /// Extract struct ID from a struct property base symbol.
    pub fn struct_id_from_base_symbol(symbol_id: SymbolId) -> Option<StructId> {
        if symbol_id.0 & STRUCT_PROPERTY_BASE_BIT != 0 {
            Some(StructId((symbol_id.0 & !(STRUCT_PROPERTY_BASE_BIT)) as u32))
        } else {
            None
        }
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

    use crate::{compile::struct_visitor, grammar, lexer::Lexer, tokens::FileId};

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

            // Struct registration and type resolution
            let mut struct_registry = StructRegistry::default();
            let struct_names =
                struct_visitor::register_structs(&script, &mut struct_registry, &mut diagnostics);
            struct_visitor::resolve_struct_fields(
                &script,
                &mut struct_registry,
                &struct_names,
                &mut diagnostics,
            );
            struct_visitor::resolve_all_types(&mut script, &struct_names, &mut diagnostics);

            let mut visitor = SymTabVisitor::new(
                &CompileSettings {
                    available_fields: None,
                    enable_optimisations: false,
                },
                &mut script,
                &struct_registry,
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

            // Struct registration and type resolution
            let mut struct_registry = StructRegistry::default();
            let struct_names =
                struct_visitor::register_structs(&script, &mut struct_registry, &mut diagnostics);
            struct_visitor::resolve_struct_fields(
                &script,
                &mut struct_registry,
                &struct_names,
                &mut diagnostics,
            );
            struct_visitor::resolve_all_types(&mut script, &struct_names, &mut diagnostics);

            let mut visitor = SymTabVisitor::new(
                &CompileSettings {
                    available_fields: None,
                    enable_optimisations: false,
                },
                &mut script,
                &struct_registry,
                &mut diagnostics,
            );

            for function in &mut script.functions {
                visitor.visit_function(function, &mut diagnostics);
            }

            assert_snapshot!(diagnostics.pretty_string(false));
        });
    }

    #[test]
    fn property_not_in_struct_test() {
        let input = fs::read_to_string(
            "src/compile/snapshot_tests/symtab_visitor/property_not_in_struct.tapir",
        )
        .unwrap();

        let file_id = FileId::new(0);
        let lexer = Lexer::new(&input, file_id);
        let parser = grammar::ScriptParser::new();

        let mut diagnostics = Diagnostics::new(file_id, "property_not_in_struct.tapir", &input);

        let mut script = parser
            .parse(FileId::new(0), &mut diagnostics, lexer)
            .unwrap();

        let struct_registry = StructRegistry::default();
        // Only 'health' and 'position' exist in the struct, 'nonexistent' doesn't
        let _visitor = SymTabVisitor::new(
            &CompileSettings {
                available_fields: Some(vec!["health".to_string(), "position".to_string()]),
                enable_optimisations: false,
            },
            &mut script,
            &struct_registry,
            &mut diagnostics,
        );

        assert_snapshot!(diagnostics.pretty_string(false));
    }
}
