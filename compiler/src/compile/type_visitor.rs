use std::{borrow::Cow, collections::HashMap};

use serde::Serialize;

use crate::{
    Trigger,
    ast::{
        self, BinaryOperator, BuiltinFunction, Expression, ExpressionKind,
        ExternFunctionDefinition, Function, FunctionModifiers, FunctionReturn, GlobalDeclaration,
        InternalOrExternalFunctionId, MethodCallInfo, OperatorOverloadInfo, SymbolId,
    },
    reporting::{DiagnosticMessage, Diagnostics, ErrorKind},
    tokens::Span,
    types::{StructId, StructRegistry, Type},
};

/// Stored in FieldAccess expression metadata during type checking.
/// Contains the struct type of the base expression and field index, needed for IR lowering.
#[derive(Clone, Copy, Debug)]
pub struct FieldAccessInfo {
    pub base_struct_id: StructId,
    pub field_index: usize,
}

/// Stored in Assignment statement metadata for targets that are field paths.
/// Each entry corresponds to a target - None for simple variables, Some for field paths.
/// For field paths, stores the root struct id and the field indices traversed.
#[derive(Clone, Debug)]
pub struct FieldAssignmentInfo(pub Vec<Option<(StructId, Vec<usize>)>>);

/// Stored in Call expression metadata during type checking.
/// Contains return types so IR lowering knows when to expand targets for struct returns.
#[derive(Clone, Debug)]
pub struct CallReturnInfo(pub Vec<Type>);

use super::{
    loop_visitor::LoopContainsNoBreak,
    symtab_visitor::{FunctionArgumentSymbols, GlobalId, SymTab},
};

pub struct TypeVisitor<'input, 'reg> {
    type_table: Vec<Option<(Type, Option<Span>)>>,
    functions: HashMap<InternalOrExternalFunctionId, FunctionInfo<'input>>,
    struct_registry: &'reg StructRegistry,

    trigger_types: HashMap<&'input str, TriggerInfo>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TriggerId(pub usize);

struct FunctionInfo<'input> {
    name: &'input str,
    span: Span,
    args: Vec<FunctionArgumentInfo<'input>>,
    rets: Vec<Type>,
    modifiers: FunctionModifiers,
}

struct FunctionArgumentInfo<'input> {
    name: Cow<'input, str>,
    ty: Type,
    span: Span,
}

#[derive(Serialize, Clone, Debug)]
struct TriggerInfo {
    span: Span,
    ty: Box<[Type]>,
    index: usize,
}

struct AssignmentTargets<'a, 'input> {
    symbol_ids: &'a [SymbolId],
    ident_spans: &'a [Span],
    annotations: &'a [Option<&'a ast::TypeWithLocation<'input>>],
}

impl<'input, 'reg> TypeVisitor<'input, 'reg> {
    pub fn new(
        functions: &[Function<'input>],
        extern_functions: &[ExternFunctionDefinition<'input>],
        builtin_functions: &[BuiltinFunction<'input>],
        struct_registry: &'reg StructRegistry,
        symtab: &SymTab<'input>,
    ) -> Self {
        let mut resolved_functions = HashMap::new();

        // Register struct constructors
        for (i, struct_def) in struct_registry.iter().enumerate() {
            let struct_id = StructId(i as u32);
            let args = struct_def
                .fields
                .iter()
                .map(|field| FunctionArgumentInfo {
                    name: Cow::Owned(field.name.clone()),
                    ty: field.ty,
                    span: field.span,
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::StructConstructor(struct_id),
                FunctionInfo {
                    name: Box::leak(struct_def.name.clone().into_boxed_str()),
                    span: struct_def.span,
                    args,
                    rets: vec![Type::Struct(struct_id)],
                    modifiers: FunctionModifiers::default(),
                },
            );
        }

        for function in functions {
            let args = function
                .arguments
                .iter()
                .map(|arg| FunctionArgumentInfo {
                    name: Cow::Borrowed(arg.name()),
                    ty: arg.ty_required().resolved(),
                    span: arg.span(),
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::Internal(*function.meta.get().unwrap()),
                FunctionInfo {
                    name: function.name,
                    span: function.span,
                    args,
                    rets: function
                        .return_types
                        .types
                        .iter()
                        .map(|t| t.resolved())
                        .collect(),
                    modifiers: function.modifiers.clone(),
                },
            );
        }

        for function in extern_functions {
            let args = function
                .arguments
                .iter()
                .map(|arg| FunctionArgumentInfo {
                    name: Cow::Borrowed(arg.name()),
                    ty: arg.ty_required().resolved(),
                    span: arg.span(),
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::External(*function.meta.get().unwrap()),
                FunctionInfo {
                    name: function.name,
                    span: function.span,
                    args,
                    rets: function
                        .return_types
                        .types
                        .iter()
                        .map(|t| t.resolved())
                        .collect(),
                    modifiers: FunctionModifiers::default(),
                },
            );
        }

        for function in builtin_functions {
            let args = function
                .arguments
                .iter()
                .map(|arg| FunctionArgumentInfo {
                    name: Cow::Borrowed(arg.name()),
                    ty: arg.ty_required().resolved(),
                    span: arg.span(),
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::Builtin(*function.meta.get().unwrap()),
                FunctionInfo {
                    name: function.name,
                    span: function.span,
                    args,
                    rets: function
                        .return_type
                        .types
                        .iter()
                        .map(|t| t.resolved())
                        .collect(),
                    modifiers: FunctionModifiers::default(),
                },
            );
        }

        Self {
            type_table: symtab
                .properties()
                .iter()
                .map(|prop| Some((prop.ty, None)))
                .collect(),

            functions: resolved_functions,
            struct_registry,
            trigger_types: HashMap::new(),
        }
    }

    fn resolve_type_with_spans(
        &mut self,
        symbol_id: SymbolId,
        ty: Type,
        ident_span: Span,
        value_span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        // Skip globals - their types are fixed at declaration
        if GlobalId::from_symbol_id(symbol_id).is_some() {
            return;
        }

        // Skip struct property bases - they don't have entries in the type table
        if SymTab::is_struct_property_base_symbol(symbol_id) {
            return;
        }

        if self.type_table.len() <= symbol_id.0 as usize {
            self.type_table.resize(symbol_id.0 as usize + 1, None);
        }

        if let Some((table_type, expected_span)) = self.type_table[symbol_id.0 as usize]
            && table_type != ty
        {
            if ty == Type::Error {
                // the error should already be reported
                return;
            }

            if let Some(property) = symtab.get_property(symbol_id) {
                ErrorKind::PropertyTypeError {
                    property_name: property.name.clone(),
                    expected: table_type,
                    actual: ty,
                }
                .at(value_span)
                .label(value_span, DiagnosticMessage::AssigningType { ty })
                .label(
                    property.span,
                    DiagnosticMessage::DefinedAs { ty: table_type },
                )
                .emit(diagnostics);
            } else {
                let builder = ErrorKind::TypeError {
                    expected: table_type,
                    actual: ty,
                }
                .at(value_span)
                .label(value_span, DiagnosticMessage::AssigningType { ty });

                let builder = if let Some(expected_span) = expected_span {
                    builder.label(
                        expected_span,
                        DiagnosticMessage::DefinedAs { ty: table_type },
                    )
                } else {
                    builder
                };

                builder.emit(diagnostics);
            }

            return;
        }

        self.type_table[symbol_id.0 as usize] = Some((ty, Some(ident_span)));
    }

    pub fn get_type(
        &self,
        symbol_id: SymbolId,
        span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Type {
        // Check if this is a global
        if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
            return symtab.get_global(global_id).ty;
        }

        // Check if this is a struct property base (e.g., "pos" for "property pos: Point;")
        if let Some(struct_id) = SymTab::struct_id_from_base_symbol(symbol_id) {
            return Type::Struct(struct_id);
        }

        match self.type_table.get(symbol_id.0 as usize) {
            Some(Some((ty, _))) => *ty,
            _ => {
                ErrorKind::UnknownType {
                    name: symtab.name_for_symbol(symbol_id).into_owned(),
                }
                .at(span)
                .label(span, DiagnosticMessage::UnknownTypeLabel)
                .emit(diagnostics);

                Type::Error
            }
        }
    }

    /// Check a type annotation against the inferred type.
    /// Returns the final type to use (annotation if present and valid, otherwise inferred).
    fn check_type_annotation(
        &self,
        annotation: Option<&ast::TypeWithLocation>,
        inferred_type: Type,
        ident_span: Span,
        value_span: Span,
        diagnostics: &mut Diagnostics,
    ) -> Type {
        if let Some(annotation) = annotation {
            let annotated = annotation.resolved();

            // Allow if types match or either is Error (to avoid cascading errors)
            let types_match = annotated == inferred_type
                || annotated == Type::Error
                || inferred_type == Type::Error;

            if !types_match {
                ErrorKind::TypeAnnotationMismatch {
                    annotated,
                    actual: inferred_type,
                }
                .at(ident_span)
                .label(
                    annotation.span,
                    DiagnosticMessage::ExpectedType { ty: annotated },
                )
                .label(value_span, DiagnosticMessage::HasType { ty: inferred_type })
                .emit(diagnostics);
                Type::Error
            } else {
                annotated
            }
        } else {
            // No annotation - use inferred type
            inferred_type
        }
    }

    /// Check type annotations on global declarations.
    /// The inferred type comes from the constant initializer (stored in symtab),
    /// and we validate it matches any explicit annotation.
    pub fn check_global_annotations(
        &self,
        globals: &[GlobalDeclaration<'_>],
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        for global in globals {
            let Some(ref annotation) = global.name.ty else {
                continue;
            };

            let Some(global_info) = symtab.get_global_by_name(global.name.name()) else {
                // Global wasn't registered (e.g., due to conflict with property)
                continue;
            };

            let inferred_type = global_info.ty;
            let annotated = annotation.resolved();

            let types_match = annotated == inferred_type
                || annotated == Type::Error
                || inferred_type == Type::Error;

            if !types_match {
                ErrorKind::TypeAnnotationMismatch {
                    annotated,
                    actual: inferred_type,
                }
                .at(global.span)
                .label(
                    annotation.span,
                    DiagnosticMessage::ExpectedType { ty: annotated },
                )
                .label(
                    global.value.span,
                    DiagnosticMessage::HasType { ty: inferred_type },
                )
                .emit(diagnostics);
            }
        }
    }

    fn check_variable_declaration(
        &mut self,
        symbol_ids: &[SymbolId],
        idents: &[ast::TypedIdent<'input>],
        values: &mut [Expression<'input>],
        statement_span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        let ident_spans: Vec<Span> = idents.iter().map(|i| i.span()).collect();
        let annotations: Vec<Option<&ast::TypeWithLocation>> =
            idents.iter().map(|i| i.ty.as_ref()).collect();

        let targets = AssignmentTargets {
            symbol_ids,
            ident_spans: &ident_spans,
            annotations: &annotations,
        };

        self.check_assignment_inner(targets, values, statement_span, symtab, diagnostics);
    }

    /// Check assignments. Returns field assignment info if any targets are field paths.
    fn check_assignment(
        &mut self,
        symbol_ids: &[SymbolId],
        paths: &[Vec<ast::Ident<'input>>],
        values: &mut [Expression<'input>],
        statement_span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Option<FieldAssignmentInfo> {
        // Type check values. Special handling for multi-return function calls:
        // if there's a single value but multiple targets, check if it's a function call
        // with multiple return types.
        let value_types: Vec<(Type, Span)> = if values.len() == 1
            && paths.len() > 1
            && let Some(fn_ret_types) =
                self.return_types_for_maybe_call(&mut values[0], symtab, diagnostics)
        {
            // For multi-return function calls, use the call expression span for all
            let call_span = values[0].span;
            fn_ret_types.into_iter().map(|t| (t, call_span)).collect()
        } else {
            values
                .iter_mut()
                .map(|v| {
                    let span = v.span;
                    (self.type_for_expression(v, symtab, diagnostics), span)
                })
                .collect()
        };

        // Check count
        if paths.len() != value_types.len() {
            let (extra_spans, label_message): (Vec<Span>, DiagnosticMessage) =
                if paths.len() > value_types.len() {
                    (
                        paths[value_types.len()..]
                            .iter()
                            .filter_map(|p| p.last().map(|i| i.span))
                            .collect(),
                        DiagnosticMessage::NoValueForVariable,
                    )
                } else {
                    (
                        values[paths.len()..]
                            .iter()
                            .map(|v| v.span)
                            .collect::<Vec<_>>(),
                        DiagnosticMessage::NoVariableToReceiveValue,
                    )
                };

            let mut builder = ErrorKind::CountMismatch {
                ident_count: paths.len(),
                expr_count: value_types.len(),
            }
            .at(statement_span)
            .help(DiagnosticMessage::WhenAssigningMultipleVars);

            for span in extra_spans {
                builder = builder.label(span, label_message.clone());
            }

            builder.emit(diagnostics);
        }

        // Track field assignment info (struct_id and indices for field paths)
        let mut field_info_list: Vec<Option<(StructId, Vec<usize>)>> =
            Vec::with_capacity(paths.len());
        let mut has_field_assignments = false;

        // Check each assignment
        for (idx, (path, &symbol_id)) in paths.iter().zip(symbol_ids.iter()).enumerate() {
            if idx >= value_types.len() {
                field_info_list.push(None);
                continue;
            }
            let (value_type, value_span) = value_types[idx];

            if path.len() == 1 {
                // Simple variable assignment
                let ident_span = path[0].span;
                self.resolve_type_with_spans(
                    symbol_id,
                    value_type,
                    ident_span,
                    value_span,
                    symtab,
                    diagnostics,
                );
                field_info_list.push(None);
            } else {
                // Field assignment - resolve path to get target type and field indices
                let target_span = path.last().map(|i| i.span).unwrap_or(statement_span);
                if let Some((target_type, root_struct_id, field_indices)) =
                    self.resolve_field_path_with_indices(symbol_id, &path[1..], diagnostics)
                {
                    // Check that value type matches target field type
                    if value_type != Type::Error
                        && target_type != Type::Error
                        && value_type != target_type
                    {
                        ErrorKind::TypeError {
                            expected: target_type,
                            actual: value_type,
                        }
                        .at(target_span)
                        .label(target_span, DiagnosticMessage::HasType { ty: target_type })
                        .label(value_span, DiagnosticMessage::HasType { ty: value_type })
                        .emit(diagnostics);
                    }
                    field_info_list.push(Some((root_struct_id, field_indices)));
                    has_field_assignments = true;
                } else {
                    // Error case
                    field_info_list.push(None);
                }
            }
        }

        if has_field_assignments {
            Some(FieldAssignmentInfo(field_info_list))
        } else {
            None
        }
    }

    /// Resolves a field path starting from a symbol to get the final field's type,
    /// the root struct id, and the field indices traversed.
    /// Returns None if any part of the path is invalid.
    fn resolve_field_path_with_indices(
        &self,
        root_symbol: SymbolId,
        path: &[ast::Ident<'input>],
        diagnostics: &mut Diagnostics,
    ) -> Option<(Type, StructId, Vec<usize>)> {
        // Check if this is a struct property base symbol first
        let mut current_type =
            if let Some(struct_id) = SymTab::struct_id_from_base_symbol(root_symbol) {
                Type::Struct(struct_id)
            } else {
                // Get the root symbol's type from the type table
                match self.type_table.get(root_symbol.0 as usize) {
                    Some(Some((ty, _))) => *ty,
                    _ => Type::Error,
                }
            };

        // The root must be a struct
        let root_struct_id = match current_type {
            Type::Struct(id) => id,
            Type::Error => return None,
            _ => {
                if let Some(first) = path.first() {
                    ErrorKind::FieldAccessOnNonStruct { ty: current_type }
                        .at(first.span)
                        .label(
                            first.span,
                            DiagnosticMessage::FieldAccessOnNonStruct { ty: current_type },
                        )
                        .emit(diagnostics);
                }
                return None;
            }
        };

        let mut field_indices = Vec::with_capacity(path.len());

        for field_ident in path {
            match current_type {
                Type::Struct(struct_id) => {
                    let struct_def = self.struct_registry.get(struct_id);
                    if let Some((field_index, field_def)) = struct_def
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, f)| f.name == field_ident.ident)
                    {
                        field_indices.push(field_index);
                        current_type = field_def.ty;
                    } else {
                        ErrorKind::UnknownField {
                            struct_name: struct_def.name.clone(),
                            field_name: field_ident.ident.to_string(),
                        }
                        .at(field_ident.span)
                        .label(
                            field_ident.span,
                            DiagnosticMessage::UnknownField {
                                struct_name: struct_def.name.clone(),
                                field_name: field_ident.ident.to_string(),
                            },
                        )
                        .emit(diagnostics);
                        return None;
                    }
                }
                Type::Error => return None,
                _ => {
                    ErrorKind::FieldAccessOnNonStruct { ty: current_type }
                        .at(field_ident.span)
                        .label(
                            field_ident.span,
                            DiagnosticMessage::FieldAccessOnNonStruct { ty: current_type },
                        )
                        .emit(diagnostics);
                    return None;
                }
            }
        }

        Some((current_type, root_struct_id, field_indices))
    }

    fn check_assignment_inner(
        &mut self,
        targets: AssignmentTargets<'_, 'input>,
        values: &mut [Expression<'input>],
        statement_span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        // this is _only_ valid if it's a function call on the RHS
        let value_types_and_spans: Vec<(Type, Span)> = if values.len() == 1
            && targets.ident_spans.len() > 1
            && let Some(fn_ret_types) =
                self.return_types_for_maybe_call(&mut values[0], symtab, diagnostics)
        {
            // For multi-return function calls, use the call expression span for all
            let call_span = values[0].span;
            fn_ret_types.into_iter().map(|t| (t, call_span)).collect()
        } else {
            values
                .iter_mut()
                .map(|v| {
                    let span = v.span;
                    (self.type_for_expression(v, symtab, diagnostics), span)
                })
                .collect()
        };

        if value_types_and_spans.len() != targets.ident_spans.len() {
            let (extra_spans, label_message): (Vec<Span>, DiagnosticMessage) =
                if targets.ident_spans.len() > value_types_and_spans.len() {
                    (
                        targets.ident_spans[value_types_and_spans.len()..].to_vec(),
                        DiagnosticMessage::NoValueForVariable,
                    )
                } else {
                    (
                        values[targets.ident_spans.len()..]
                            .iter()
                            .map(|v| v.span)
                            .collect::<Vec<_>>(),
                        DiagnosticMessage::NoVariableToReceiveValue,
                    )
                };

            let mut builder = ErrorKind::CountMismatch {
                ident_count: targets.ident_spans.len(),
                expr_count: value_types_and_spans.len(),
            }
            .at(statement_span)
            .help(DiagnosticMessage::WhenAssigningMultipleVars);

            for span in extra_spans {
                builder = builder.label(span, label_message.clone());
            }

            builder.emit(diagnostics);
        }

        for (((&symbol, (inferred_type, value_span)), &ident_span), annotation) in targets
            .symbol_ids
            .iter()
            .zip(value_types_and_spans)
            .zip(targets.ident_spans.iter())
            .zip(targets.annotations.iter())
        {
            // Check type annotation if present
            let final_type = self.check_type_annotation(
                *annotation,
                inferred_type,
                ident_span,
                value_span,
                diagnostics,
            );

            self.resolve_type_with_spans(
                symbol,
                final_type,
                ident_span,
                value_span,
                symtab,
                diagnostics,
            );
        }
    }

    pub fn visit_function(
        &mut self,
        function: &mut Function<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        let argument_symbols = function
            .meta
            .get::<FunctionArgumentSymbols>()
            .expect("Should've resolved arguments by now");

        for (argument, &symbol_id) in function.arguments.iter().zip(argument_symbols.0.iter()) {
            self.resolve_type_with_spans(
                symbol_id,
                argument.ty_required().resolved(),
                argument.span(),
                argument.span(),
                symtab,
                diagnostics,
            );
        }

        if let FunctionModifiers {
            is_event_handler: Some(event_span),
        } = &function.modifiers
            && !function.return_types.types.is_empty()
        {
            ErrorKind::EventFunctionsShouldNotHaveAReturnType {
                function_name: function.name.to_string(),
            }
            .at(function.span)
            .label(
                function.return_types.span,
                DiagnosticMessage::ExpectedNoReturnType,
            )
            .label(
                *event_span,
                DiagnosticMessage::DeclaredAsEventHandler {
                    name: function.name.to_string(),
                },
            )
            .help(DiagnosticMessage::RemoveReturnTypeOrChangeToRegularFunction)
            .emit(diagnostics);

            // we've added an error, so compilation will fail
            // but we also want to warn about invalid returns etc, so
            // change to what the user intended and warn about everything else too
            function.return_types.types.clear();
            function.return_types.span = *event_span;
        }

        let block_analysis_result = self.visit_block(
            &mut function.statements,
            symtab,
            &function.return_types,
            diagnostics,
        );

        if !function.return_types.types.is_empty()
            && block_analysis_result != BlockAnalysisResult::AllBranchesReturn
        {
            ErrorKind::FunctionDoesNotHaveReturn {
                name: function.name.to_string(),
            }
            .at(function.span)
            .label(
                function.return_types.span,
                DiagnosticMessage::FunctionReturnsResults,
            )
            .emit(diagnostics);
        }
    }

    fn visit_block(
        &mut self,
        ast: &mut [ast::Statement<'input>],
        symtab: &SymTab,
        expected_return_type: &FunctionReturn,
        diagnostics: &mut Diagnostics,
    ) -> BlockAnalysisResult {
        for statement in ast.iter_mut() {
            match &mut statement.kind {
                ast::StatementKind::Wait
                | ast::StatementKind::Break
                | ast::StatementKind::Continue
                | ast::StatementKind::Nop
                | ast::StatementKind::Error => {}
                ast::StatementKind::VariableDeclaration { idents, values } => {
                    let symbol_ids: &Vec<SymbolId> = statement
                        .meta
                        .get()
                        .expect("Should've been resolved by symbol resolution");

                    self.check_variable_declaration(
                        symbol_ids,
                        idents,
                        values,
                        statement.span,
                        symtab,
                        diagnostics,
                    );
                }
                ast::StatementKind::Assignment { targets, values } => {
                    let symbol_ids: &Vec<SymbolId> = statement
                        .meta
                        .get()
                        .expect("Should've been resolved by symbol resolution");

                    if let Some(field_info) = self.check_assignment(
                        symbol_ids,
                        targets,
                        values,
                        statement.span,
                        symtab,
                        diagnostics,
                    ) {
                        statement.meta.set(field_info);
                    }
                }
                ast::StatementKind::If {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let condition_type = self.type_for_expression(condition, symtab, diagnostics);
                    if !matches!(condition_type, Type::Bool | Type::Error) {
                        ErrorKind::InvalidTypeForIfCondition {
                            got: condition_type,
                        }
                        .at(condition.span)
                        .label(
                            condition.span,
                            DiagnosticMessage::HasType { ty: condition_type },
                        )
                        .emit(diagnostics);
                    }

                    let lhs_analysis_result =
                        self.visit_block(true_block, symtab, expected_return_type, diagnostics);
                    let rhs_analysis_result =
                        self.visit_block(false_block, symtab, expected_return_type, diagnostics);

                    if lhs_analysis_result == BlockAnalysisResult::AllBranchesReturn
                        && rhs_analysis_result == BlockAnalysisResult::AllBranchesReturn
                    {
                        return BlockAnalysisResult::AllBranchesReturn;
                    }
                }
                ast::StatementKind::Return { values } => {
                    let mut actual_return_types = Vec::with_capacity(values.len());
                    for value in values.iter_mut() {
                        actual_return_types.push(self.type_for_expression(
                            value,
                            symtab,
                            diagnostics,
                        ));
                    }

                    if actual_return_types.len() != expected_return_type.types.len() {
                        ErrorKind::IncorrectNumberOfReturnTypes {
                            expected: expected_return_type.types.len(),
                            actual: actual_return_types.len(),
                        }
                        .at(statement.span)
                        .label(
                            statement.span,
                            DiagnosticMessage::HasReturnValues {
                                count: actual_return_types.len(),
                            },
                        )
                        .label(
                            expected_return_type.span,
                            DiagnosticMessage::FunctionReturnsValues {
                                count: expected_return_type.types.len(),
                            },
                        )
                        .note(DiagnosticMessage::FunctionsMustReturnFixedNumber)
                        .emit(diagnostics);
                    }

                    for (i, (actual, expected)) in actual_return_types
                        .iter()
                        .zip(&expected_return_type.types)
                        .enumerate()
                    {
                        let expected_ty = expected.resolved();
                        if *actual != expected_ty
                            && *actual != Type::Error
                            && expected_ty != Type::Error
                        {
                            ErrorKind::MismatchingReturnTypes {
                                expected: expected_ty,
                                actual: *actual,
                            }
                            .at(statement.span)
                            .label(values[i].span, DiagnosticMessage::HasType { ty: *actual })
                            .label(
                                expected.span,
                                DiagnosticMessage::HasType { ty: expected_ty },
                            )
                            .emit(diagnostics);
                        }
                    }

                    return BlockAnalysisResult::AllBranchesReturn;
                }
                ast::StatementKind::Expression { expression } => {
                    // Type check the expression; the result is discarded.
                    // For call expressions, use type_for_call directly to avoid
                    // the "must return one value" check (which only applies when
                    // the result is actually used).
                    if let ast::ExpressionKind::Call { name, arguments } = &mut expression.kind {
                        self.type_for_call(
                            expression.span,
                            name,
                            expression.meta.get().copied(),
                            arguments,
                            symtab,
                            diagnostics,
                        );
                    } else {
                        self.type_for_expression(expression, symtab, diagnostics);
                    }
                }
                ast::StatementKind::Spawn { name, arguments } => {
                    self.type_for_call(
                        statement.span,
                        name,
                        statement.meta.get().copied(),
                        arguments,
                        symtab,
                        diagnostics,
                    );
                }
                ast::StatementKind::Loop { block } => {
                    match self.visit_block(block, symtab, expected_return_type, diagnostics) {
                        BlockAnalysisResult::AllBranchesReturn => {
                            return BlockAnalysisResult::AllBranchesReturn;
                        }
                        BlockAnalysisResult::ContainsNonReturningBranch => {
                            if statement.meta.has::<LoopContainsNoBreak>() {
                                return BlockAnalysisResult::AllBranchesReturn;
                            }
                        }
                    }
                }
                ast::StatementKind::Block { block } => {
                    if self.visit_block(block, symtab, expected_return_type, diagnostics)
                        == BlockAnalysisResult::AllBranchesReturn
                    {
                        return BlockAnalysisResult::AllBranchesReturn;
                    }
                }
                ast::StatementKind::Trigger { name, arguments } => {
                    let trigger_arguments = arguments
                        .iter_mut()
                        .map(|arg| self.type_for_expression(arg, symtab, diagnostics))
                        .collect::<Box<[_]>>();

                    let trigger_index;

                    if let Some(trigger_info) = self.trigger_types.get(name) {
                        if trigger_info.ty.len() != trigger_arguments.len()
                            || trigger_info.ty.iter().zip(&trigger_arguments).any(
                                |(expected, actual)| {
                                    actual != expected
                                        && *actual != Type::Error
                                        && *expected != Type::Error
                                },
                            )
                        {
                            ErrorKind::TriggerIncorrectArgs {
                                name: name.to_string(),
                                first_definition_args: trigger_info.ty.clone(),
                                second_definition_args: trigger_arguments.clone(),
                            }
                            .at(statement.span)
                            .label(
                                trigger_info.span,
                                DiagnosticMessage::CalledWithTypes {
                                    types: trigger_info.ty.clone(),
                                },
                            )
                            .label(
                                statement.span,
                                DiagnosticMessage::CalledWithTypes {
                                    types: trigger_arguments.clone(),
                                },
                            )
                            .help(DiagnosticMessage::TriggerCallsMustHaveSameArgTypes)
                            .emit(diagnostics);
                        }

                        trigger_index = trigger_info.index;
                    } else {
                        trigger_index = self.trigger_types.len();

                        self.trigger_types.insert(
                            name,
                            TriggerInfo {
                                span: statement.span,
                                ty: trigger_arguments,
                                index: trigger_index,
                            },
                        );
                    }

                    statement.meta.set(TriggerId(trigger_index));
                }
            }
        }

        BlockAnalysisResult::ContainsNonReturningBranch
    }

    pub fn into_type_table(
        self,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> TypeTable<'input> {
        let mut types = Vec::with_capacity(self.type_table.len());
        for (i, ty) in self.type_table.into_iter().enumerate() {
            if let Some((ty, _span)) = ty {
                types.push(ty);
            } else {
                // Push Error to maintain correct indexing
                types.push(Type::Error);

                // Only emit error for non-property symbols that should have types
                // Properties are at the start and get their types from the symtab
                if symtab.get_property(SymbolId(i as u64)).is_none() {
                    let span = symtab.span_for_symbol(SymbolId(i as u64));
                    ErrorKind::UnknownType {
                        name: symtab.name_for_symbol(SymbolId(i as u64)).into_owned(),
                    }
                    .at(span)
                    .label(span, DiagnosticMessage::UnknownTypeLabel)
                    .emit(diagnostics);
                }
            }
        }

        TypeTable {
            types,
            triggers: self.trigger_types,
        }
    }

    fn type_for_expression(
        &mut self,
        expression: &mut Expression<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Type {
        match &mut expression.kind {
            ast::ExpressionKind::Integer(_) => Type::Int,
            ast::ExpressionKind::Fix(_) => Type::Fix,
            ast::ExpressionKind::Bool(_) => Type::Bool,
            ast::ExpressionKind::Variable(_) => {
                // If symbol resolution failed (unknown variable), there won't be a SymbolId
                let Some(symbol_id) = expression.meta.get::<SymbolId>() else {
                    return Type::Error;
                };
                self.get_type(*symbol_id, expression.span, symtab, diagnostics)
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_type = self.type_for_expression(lhs, symtab, diagnostics);
                let rhs_type = self.type_for_expression(rhs, symtab, diagnostics);

                if lhs_type == Type::Error || rhs_type == Type::Error {
                    // we don't need to report the same error multiple times
                    return Type::Error;
                }

                // Extract spans before borrowing expression.kind
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;
                let op = *operator;

                // Check for operator overload if at least one operand is a struct
                // AND the operator is overloadable
                if (lhs_type.is_struct() || rhs_type.is_struct()) && is_overloadable_operator(op) {
                    if let Some(result) = self.try_operator_overload(
                        lhs_type,
                        op,
                        rhs_type,
                        expression,
                        symtab,
                        diagnostics,
                    ) {
                        return result;
                    }

                    // No overload found - emit helpful error
                    let left_type_name = lhs_type.name(self.struct_registry);
                    let right_type_name = rhs_type.name(self.struct_registry);
                    ErrorKind::NoOperatorOverload {
                        left_type: left_type_name.to_string(),
                        operator: op.to_string(),
                        right_type: right_type_name.to_string(),
                    }
                    .at(expression.span)
                    .label(lhs_span, DiagnosticMessage::HasType { ty: lhs_type })
                    .label(rhs_span, DiagnosticMessage::HasType { ty: rhs_type })
                    .help(DiagnosticMessage::DefineOperatorFunction {
                        left: left_type_name.to_string(),
                        op: op.to_string(),
                        right: right_type_name.to_string(),
                    })
                    .emit(diagnostics);
                    return Type::Error;
                }

                if lhs_type != rhs_type && op != BinaryOperator::Then {
                    ErrorKind::BinaryOperatorTypeError { lhs_type, rhs_type }
                        .at(expression.span)
                        .label(
                            expression.span,
                            DiagnosticMessage::MismatchingTypesOnBinaryOperator,
                        )
                        .emit(diagnostics);

                    return Type::Error;
                }

                // Re-borrow to update operator
                if let ast::ExpressionKind::BinaryOperation { operator, .. } =
                    &mut expression.kind
                {
                    if !operator.can_handle_type(lhs_type) {
                        ErrorKind::InvalidTypeForBinaryOperator { type_: lhs_type }
                            .at(lhs_span)
                            .label(lhs_span, DiagnosticMessage::BinaryOperatorCannotHandleType)
                            .emit(diagnostics);

                        return Type::Error;
                    }

                    operator.update_type_with_lhs(lhs_type);
                    operator.resulting_type(lhs_type, rhs_type)
                } else {
                    unreachable!()
                }
            }
            ast::ExpressionKind::Error => Type::Error,
            ast::ExpressionKind::Nop => Type::Error,
            ast::ExpressionKind::Call { name, arguments } => {
                let types = self.type_for_call(
                    expression.span,
                    name,
                    expression.meta.get().copied(),
                    arguments,
                    symtab,
                    diagnostics,
                );

                // Store return types for IR lowering (needed for struct returns)
                expression.meta.set(CallReturnInfo(types.clone()));

                if types.len() != 1 {
                    ErrorKind::FunctionMustReturnOneValueInThisLocation {
                        actual: types.len(),
                    }
                    .at(expression.span)
                    .label(
                        expression.span,
                        DiagnosticMessage::FunctionMustReturnOneHere,
                    )
                    .emit(diagnostics);
                    Type::Error
                } else {
                    types[0]
                }
            }
            ast::ExpressionKind::FieldAccess { base, field } => {
                let base_type = self.type_for_expression(base, symtab, diagnostics);

                match base_type {
                    Type::Struct(struct_id) => {
                        let struct_def = self.struct_registry.get(struct_id);
                        if let Some((field_index, field_def)) = struct_def
                            .fields
                            .iter()
                            .enumerate()
                            .find(|(_, f)| f.name == field.ident)
                        {
                            // Store info for IR lowering
                            expression.meta.set(FieldAccessInfo {
                                base_struct_id: struct_id,
                                field_index,
                            });
                            field_def.ty
                        } else {
                            ErrorKind::UnknownField {
                                struct_name: struct_def.name.clone(),
                                field_name: field.ident.to_string(),
                            }
                            .at(field.span)
                            .label(
                                field.span,
                                DiagnosticMessage::UnknownField {
                                    struct_name: struct_def.name.clone(),
                                    field_name: field.ident.to_string(),
                                },
                            )
                            .emit(diagnostics);
                            Type::Error
                        }
                    }
                    Type::Error => Type::Error,
                    _ => {
                        ErrorKind::FieldAccessOnNonStruct { ty: base_type }
                            .at(expression.span)
                            .label(base.span, DiagnosticMessage::HasType { ty: base_type })
                            .emit(diagnostics);
                        Type::Error
                    }
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                // Type check the receiver
                let receiver_type = self.type_for_expression(receiver, symtab, diagnostics);

                if receiver_type == Type::Error {
                    return Type::Error;
                }

                // Build the mangled name based on receiver type
                let type_name = receiver_type.name(self.struct_registry);

                let mangled = format!("{}@{}", type_name, method.ident);

                // Look up the method
                let Some(function_id) = symtab.function_by_mangled_name(&mangled) else {
                    ErrorKind::UnknownMethod {
                        type_name: type_name.to_string(),
                        method_name: method.ident.to_string(),
                    }
                    .at(method.span)
                    .label(
                        receiver.span,
                        DiagnosticMessage::HasType { ty: receiver_type },
                    )
                    .emit(diagnostics);
                    return Type::Error;
                };

                // Type check arguments first (before borrowing function_info)
                let argument_types: Vec<_> = arguments
                    .iter_mut()
                    .map(|arg| (self.type_for_expression(arg, symtab, diagnostics), arg.span))
                    .collect();

                // Now we can borrow function_info
                let function_info = self.functions.get(&function_id).unwrap();

                // Expected args = function args minus self (first param)
                let expected_args = if !function_info.args.is_empty() {
                    function_info.args.len() - 1
                } else {
                    0
                };

                if argument_types.len() != expected_args {
                    ErrorKind::IncorrectNumberOfArguments {
                        function_name: format!("{}.{}", type_name, method.ident),
                        expected: expected_args,
                        actual: argument_types.len(),
                    }
                    .at(expression.span)
                    .label(
                        expression.span,
                        DiagnosticMessage::GotArguments {
                            count: argument_types.len(),
                        },
                    )
                    .label(
                        function_info.span,
                        DiagnosticMessage::ExpectedArguments {
                            count: expected_args,
                        },
                    )
                    .emit(diagnostics);
                } else {
                    // Verify receiver type matches self parameter
                    if !function_info.args.is_empty() {
                        let self_param = &function_info.args[0];
                        if receiver_type != self_param.ty
                            && receiver_type != Type::Error
                            && self_param.ty != Type::Error
                        {
                            ErrorKind::FunctionArgumentTypeError {
                                function_name: format!("{}.{}", type_name, method.ident),
                                argument_name: "self".to_string(),
                                expected: self_param.ty,
                                actual: receiver_type,
                            }
                            .at(receiver.span)
                            .emit(diagnostics);
                        }
                    }

                    // Type check each explicit argument (skip self parameter in function_info.args)
                    for ((actual, actual_span), arg_info) in
                        argument_types.iter().zip(function_info.args.iter().skip(1))
                    {
                        if *actual != arg_info.ty
                            && *actual != Type::Error
                            && arg_info.ty != Type::Error
                        {
                            ErrorKind::FunctionArgumentTypeError {
                                function_name: format!("{}.{}", type_name, method.ident),
                                argument_name: arg_info.name.to_string(),
                                expected: arg_info.ty,
                                actual: *actual,
                            }
                            .at(*actual_span)
                            .label(
                                arg_info.span,
                                DiagnosticMessage::ExpectedType { ty: arg_info.ty },
                            )
                            .label(*actual_span, DiagnosticMessage::PassingType { ty: *actual })
                            .emit(diagnostics);
                        }
                    }
                }

                // Store method call info for IR lowering
                expression.meta.set(MethodCallInfo { function_id });

                // Store return types for IR lowering (needed for struct returns)
                let return_types = function_info.rets.clone();
                expression.meta.set(CallReturnInfo(return_types.clone()));

                // Return type is the function's return type
                if return_types.len() == 1 {
                    return_types[0]
                } else {
                    Type::Error // void or multi-return method used as expression
                }
            }
        }
    }

    fn return_types_for_maybe_call(
        &mut self,
        expr: &mut Expression<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Option<Vec<Type>> {
        match &mut expr.kind {
            ExpressionKind::Call {
                name, arguments, ..
            } => Some(self.type_for_call(
                expr.span,
                name,
                expr.meta.get().copied(),
                arguments,
                symtab,
                diagnostics,
            )),
            _ => None,
        }
    }

    fn type_for_call(
        &mut self,
        span: Span,
        name: &'input str,
        function_id: Option<InternalOrExternalFunctionId>,
        arguments: &mut [Expression<'input>],
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Vec<Type> {
        let argument_types: Vec<_> = arguments
            .iter_mut()
            .map(|arg| (self.type_for_expression(arg, symtab, diagnostics), arg.span))
            .collect();

        let Some(function_id) = function_id else {
            return vec![Type::Error];
        };

        let function_info = self.functions.get(&function_id).unwrap();

        if function_info.modifiers.is_event_handler.is_some() {
            ErrorKind::CannotCallEventHandler {
                function_name: name.to_string(),
            }
            .at(span)
            .label(span, DiagnosticMessage::ThisCallHere)
            .label(function_info.span, DiagnosticMessage::ThisEventHandler)
            .note(DiagnosticMessage::CannotCallEventHandlerNote {
                function_name: name.to_string(),
            })
            .emit(diagnostics);

            return vec![Type::Error];
        } else if argument_types.len() != function_info.args.len() {
            ErrorKind::IncorrectNumberOfArguments {
                function_name: name.to_string(),
                expected: function_info.args.len(),
                actual: argument_types.len(),
            }
            .at(span)
            .label(
                span,
                DiagnosticMessage::GotArguments {
                    count: argument_types.len(),
                },
            )
            .label(
                function_info.span,
                DiagnosticMessage::ExpectedArguments {
                    count: function_info.args.len(),
                },
            )
            .emit(diagnostics);
        } else {
            for ((actual, actual_span), arg_info) in argument_types.iter().zip(&function_info.args)
            {
                if *actual != arg_info.ty && *actual != Type::Error && arg_info.ty != Type::Error {
                    ErrorKind::FunctionArgumentTypeError {
                        function_name: function_info.name.to_string(),
                        argument_name: arg_info.name.to_string(),
                        expected: arg_info.ty,
                        actual: *actual,
                    }
                    .at(*actual_span)
                    .label(
                        arg_info.span,
                        DiagnosticMessage::ExpectedType { ty: arg_info.ty },
                    )
                    .label(*actual_span, DiagnosticMessage::PassingType { ty: *actual })
                    .emit(diagnostics);
                }
            }
        }

        function_info.rets.clone()
    }

    /// Try to resolve an operator overload for the given operand types.
    /// Returns Some(return_type) if an overload was found and resolved.
    /// Returns None if no overload exists (caller should emit an error).
    fn try_operator_overload(
        &self,
        lhs_type: Type,
        operator: BinaryOperator,
        rhs_type: Type,
        expression: &mut Expression<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Option<Type> {
        // Look up the operator function
        let function_id =
            symtab.lookup_operator(lhs_type, &operator, rhs_type, self.struct_registry)?;

        let function_info = self.functions.get(&function_id)?;

        // Store operator overload info for IR lowering
        expression.meta.set(OperatorOverloadInfo { function_id });

        // Store return types for IR lowering (needed for struct returns)
        let return_types = function_info.rets.clone();
        expression.meta.set(CallReturnInfo(return_types.clone()));

        // Operators must return exactly one value
        if return_types.len() != 1 {
            ErrorKind::OperatorMustReturnOneValue {
                operator: operator.to_string(),
                actual_count: return_types.len(),
            }
            .at(expression.span)
            .emit(diagnostics);
            return Some(Type::Error);
        }

        Some(return_types[0])
    }
}

/// Check if an operator can be overloaded.
fn is_overloadable_operator(op: BinaryOperator) -> bool {
    use BinaryOperator as B;
    matches!(
        op,
        B::Add
            | B::Sub
            | B::Mul
            | B::Div
            | B::Mod
            | B::RealDiv
            | B::RealMod
            | B::EqEq
            | B::NeEq
            | B::Lt
            | B::LtEq
            | B::Gt
            | B::GtEq
    )
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BlockAnalysisResult {
    AllBranchesReturn,
    ContainsNonReturningBranch,
}

#[derive(Clone, Serialize)]
pub struct TypeTable<'input> {
    types: Vec<Type>,

    triggers: HashMap<&'input str, TriggerInfo>,
}

impl TypeTable<'_> {
    pub fn type_for_symbol(&self, symbol_id: SymbolId, symtab: &SymTab) -> Type {
        if let Some(global_id) = GlobalId::from_symbol_id(symbol_id) {
            return symtab.get_global(global_id).ty;
        }

        // Check if it's a property
        if let Some(property) = symtab.get_property(symbol_id) {
            return property.ty;
        }

        // Handle case where type wasn't recorded (e.g., due to errors)
        self.types
            .get(symbol_id.0 as usize)
            .copied()
            .unwrap_or(Type::Error)
    }

    pub fn triggers(&self) -> Box<[Trigger]> {
        let mut result = vec![];
        result.resize_with(self.triggers.len(), || Trigger {
            name: String::new(),
            arguments: Box::new([]),
        });

        for (name, info) in &self.triggers {
            result[info.index] = Trigger {
                name: name.to_string(),
                arguments: info.ty.clone(),
            };
        }

        result.into_boxed_slice()
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{
        compile::{CompileSettings, loop_visitor, struct_visitor, symtab_visitor::SymTabVisitor},
        grammar,
        lexer::Lexer,
        tokens::FileId,
        types::StructRegistry,
    };

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "type_visitor/*_success.tapir", |path| {
            eprintln!("{}", path.display());
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

            let settings = CompileSettings {
                available_fields: None,
                enable_optimisations: false,
            };
            let mut symtab_visitor =
                SymTabVisitor::new(&settings, &mut script, &struct_registry, &mut diagnostics);

            let mut type_visitor = TypeVisitor::new(
                &script.functions,
                &script.extern_functions,
                &script.builtin_functions,
                &struct_registry,
                symtab_visitor.get_symtab(),
            );

            for function in &mut script.functions {
                loop_visitor::visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);

                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            type_visitor.check_global_annotations(
                &script.globals,
                symtab_visitor.get_symtab(),
                &mut diagnostics,
            );

            assert!(
                !diagnostics.has_any(),
                "{} failed with error {}",
                path.display(),
                diagnostics.pretty_string(true)
            );

            let symtab = symtab_visitor.get_symtab();
            let type_table = type_visitor.into_type_table(symtab, &mut diagnostics);

            let all_types: Vec<_> = symtab
                .all_symbols()
                .map(|(name, id)| (name, type_table.type_for_symbol(id, symtab)))
                .chain(symtab.globals().iter().map(|g| (g.name.as_str(), g.ty)))
                .collect();

            assert_ron_snapshot!(all_types);
        });
    }

    #[test]
    fn symtab_fail_snapshot_tests() {
        glob!("snapshot_tests", "type_visitor/*_fail.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let file_id = FileId::new(0);
            let lexer = Lexer::new(&input, file_id);
            let parser = grammar::ScriptParser::new();

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

            let settings = CompileSettings {
                available_fields: None,
                enable_optimisations: false,
            };
            let mut symtab_visitor =
                SymTabVisitor::new(&settings, &mut script, &struct_registry, &mut diagnostics);
            let mut type_visitor = TypeVisitor::new(
                &script.functions,
                &script.extern_functions,
                &script.builtin_functions,
                &struct_registry,
                symtab_visitor.get_symtab(),
            );

            for function in &mut script.functions {
                loop_visitor::visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                let symtab = symtab_visitor.get_symtab();
                type_visitor.visit_function(function, symtab, &mut diagnostics);
            }

            type_visitor.check_global_annotations(
                &script.globals,
                symtab_visitor.get_symtab(),
                &mut diagnostics,
            );

            type_visitor.into_type_table(symtab_visitor.get_symtab(), &mut diagnostics);

            let err_str = diagnostics.pretty_string(false);

            assert_snapshot!(err_str);
        });
    }
}
