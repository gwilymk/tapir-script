use std::{borrow::Cow, collections::HashMap};

use serde::Serialize;

use crate::{
    Trigger,
    ast::{
        self, BinaryOperator, Expression, ExpressionKind, ExternFunctionDefinition, Function,
        FunctionModifiers, FunctionReturn, InternalOrExternalFunctionId, MaybeResolved, SymbolId,
    },
    builtins::BuiltinVariable,
    reporting::{CompilerErrorKind, CountMismatchExtras, Diagnostics},
    tokens::Span,
    types::Type,
};

use super::{CompileSettings, loop_visitor::LoopContainsNoBreak, symtab_visitor::SymTab};

pub struct TypeVisitor<'input> {
    type_table: Vec<Option<(Type, Option<Span>)>>,
    functions: HashMap<InternalOrExternalFunctionId, FunctionInfo<'input>>,

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

impl<'input> TypeVisitor<'input> {
    pub fn new(
        settings: &CompileSettings,
        functions: &[Function<'input>],
        extern_functions: &[ExternFunctionDefinition<'input>],
        symtab: &SymTab<'input>,
    ) -> Self {
        let mut resolved_functions = HashMap::new();

        for function in functions {
            let args = function
                .arguments
                .iter()
                .map(|arg| {
                    let name = match &arg.name {
                        MaybeResolved::Unresolved(s) => Cow::Borrowed(*s),
                        MaybeResolved::Resolved(id) => symtab.name_for_symbol(*id),
                    };
                    FunctionArgumentInfo {
                        name,
                        ty: arg.t.t,
                        span: arg.span,
                    }
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::Internal(*function.meta.get().unwrap()),
                FunctionInfo {
                    name: function.name,
                    span: function.span,
                    args,
                    rets: function.return_types.types.iter().map(|t| t.t).collect(),
                    modifiers: function.modifiers.clone(),
                },
            );
        }

        for function in extern_functions {
            let args = function
                .arguments
                .iter()
                .map(|arg| {
                    let name = match &arg.name {
                        MaybeResolved::Unresolved(s) => Cow::Borrowed(*s),
                        MaybeResolved::Resolved(id) => symtab.name_for_symbol(*id),
                    };
                    FunctionArgumentInfo {
                        name,
                        ty: arg.t.t,
                        span: arg.span,
                    }
                })
                .collect();

            resolved_functions.insert(
                InternalOrExternalFunctionId::External(*function.meta.get().unwrap()),
                FunctionInfo {
                    name: function.name,
                    span: function.span,
                    args,
                    rets: function.return_types.types.iter().map(|t| t.t).collect(),
                    modifiers: FunctionModifiers::default(),
                },
            );
        }

        Self {
            type_table: settings
                .properties
                .iter()
                .map(|prop| Some((prop.ty, None)))
                .collect(),

            functions: resolved_functions,
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

            let error = if let Some(property) = symtab.get_property(symbol_id) {
                CompilerErrorKind::PropertyTypeError {
                    property_name: property.name.clone(),
                    expected: table_type,
                    actual: ty,
                    actual_span: value_span,
                }
            } else {
                CompilerErrorKind::TypeError {
                    expected: table_type,
                    expected_span,
                    actual: ty,
                    actual_span: value_span,
                }
            };

            diagnostics.add_message(error.into_message(value_span));

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
        if let Some(builtin) = BuiltinVariable::from_symbol_id(symbol_id) {
            return builtin.ty();
        }

        match self.type_table.get(symbol_id.0 as usize) {
            Some(Some((ty, _))) => *ty,
            _ => {
                diagnostics.add_message(
                    CompilerErrorKind::UnknownType(symtab.name_for_symbol(symbol_id).into_owned())
                        .into_message(span),
                );

                Type::Error
            }
        }
    }

    pub fn visit_function(
        &mut self,
        function: &mut Function<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
        for argument in &function.arguments {
            let MaybeResolved::Resolved(symbol_id) = argument.name else {
                panic!("Should've resolved the symbol by now")
            };

            self.resolve_type_with_spans(
                symbol_id,
                argument.t.t,
                argument.span,
                argument.span,
                symtab,
                diagnostics,
            );
        }

        if let FunctionModifiers {
            is_event_handler: Some(event_span),
        } = &function.modifiers
            && !function.return_types.types.is_empty()
        {
            diagnostics.add_message(
                CompilerErrorKind::EventFunctionsShouldNotHaveAReturnType {
                    return_type_span: function.return_types.span,
                    function_name: function.name.to_string(),
                    event_span: *event_span,
                }
                .into_message(function.span),
            );

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
            diagnostics.add_message(
                CompilerErrorKind::FunctionDoesNotHaveReturn {
                    name: function.name.to_string(),
                    return_location: function.return_types.span,
                }
                .into_message(function.span),
            );
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
                ast::StatementKind::VariableDeclaration { idents, values }
                | ast::StatementKind::Assignment { idents, values } => {
                    let symbol_ids: &Vec<SymbolId> = statement
                        .meta
                        .get()
                        .expect("Should've been resolved by symbol resolution");

                    // this is _only_ valid if it's a function call on the RHS
                    let value_types_and_spans: Vec<(Type, Span)> = if values.len() == 1
                        && idents.len() > 1
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

                    if value_types_and_spans.len() != idents.len() {
                        let extras = if idents.len() > value_types_and_spans.len() {
                            CountMismatchExtras::Idents(
                                idents[value_types_and_spans.len()..]
                                    .iter()
                                    .map(|i| i.span)
                                    .collect(),
                            )
                        } else {
                            CountMismatchExtras::Expressions(
                                values[idents.len()..].iter().map(|v| v.span).collect(),
                            )
                        };

                        diagnostics.add_message(
                            CompilerErrorKind::CountMismatch {
                                ident_count: idents.len(),
                                expr_count: value_types_and_spans.len(),
                                extras,
                            }
                            .into_message(statement.span),
                        );
                    }

                    for ((symbol, (value_type, value_span)), ident) in symbol_ids
                        .iter()
                        .zip(value_types_and_spans)
                        .zip(idents.iter())
                    {
                        self.resolve_type_with_spans(
                            *symbol,
                            value_type,
                            ident.span,
                            value_span,
                            symtab,
                            diagnostics,
                        );
                    }
                }
                ast::StatementKind::If {
                    condition,
                    true_block,
                    false_block,
                } => {
                    let condition_type = self.type_for_expression(condition, symtab, diagnostics);
                    if !matches!(condition_type, Type::Bool | Type::Error) {
                        diagnostics.add_message(
                            CompilerErrorKind::InvalidTypeForIfCondition {
                                got: condition_type,
                            }
                            .into_message(condition.span),
                        );
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
                        diagnostics.add_message(
                            CompilerErrorKind::IncorrectNumberOfReturnTypes {
                                expected: expected_return_type.types.len(),
                                actual: actual_return_types.len(),
                                function_return_location: expected_return_type.span,
                            }
                            .into_message(statement.span),
                        );
                    }

                    for (i, (actual, expected)) in actual_return_types
                        .iter()
                        .zip(&expected_return_type.types)
                        .enumerate()
                    {
                        if *actual != expected.t
                            && *actual != Type::Error
                            && expected.t != Type::Error
                        {
                            diagnostics.add_message(
                                CompilerErrorKind::MismatchingReturnTypes {
                                    expected: expected.t,
                                    actual: *actual,
                                    expected_location: expected.span,
                                    actual_location: values[i].span,
                                }
                                .into_message(statement.span),
                            );
                        }
                    }

                    return BlockAnalysisResult::AllBranchesReturn;
                }
                ast::StatementKind::Call { name, arguments } => {
                    self.type_for_call(
                        statement.span,
                        name,
                        statement.meta.get().copied(),
                        arguments,
                        symtab,
                        diagnostics,
                    );
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
                            diagnostics.add_message(
                                CompilerErrorKind::TriggerIncorrectArgs {
                                    name: name.to_owned(),
                                    first_definition_span: trigger_info.span,
                                    first_definition_args: trigger_info.ty.clone(),
                                    second_definition_args: trigger_arguments,
                                }
                                .into_message(statement.span),
                            );
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
                diagnostics.add_message(
                    CompilerErrorKind::UnknownType(
                        symtab.name_for_symbol(SymbolId(i as u64)).into_owned(),
                    )
                    .into_message(symtab.span_for_symbol(SymbolId(i as u64))),
                );
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
                let symbol_id: &SymbolId = expression
                    .meta
                    .get()
                    .expect("Should have a symbol id from symbol resolution");
                self.get_type(*symbol_id, expression.span, symtab, diagnostics)
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_type = self.type_for_expression(lhs, symtab, diagnostics);
                let rhs_type = self.type_for_expression(rhs, symtab, diagnostics);

                if lhs_type == Type::Error || rhs_type == Type::Error {
                    // we don't need to report the same error multiple times
                    return Type::Error;
                }

                if lhs_type != rhs_type && *operator != BinaryOperator::Then {
                    diagnostics.add_message(
                        CompilerErrorKind::BinaryOperatorTypeError { lhs_type, rhs_type }
                            .into_message(expression.span),
                    );

                    return Type::Error;
                }

                if !operator.can_handle_type(lhs_type) {
                    diagnostics.add_message(
                        CompilerErrorKind::InvalidTypeForBinaryOperator { type_: lhs_type }
                            .into_message(lhs.span),
                    );

                    return Type::Error;
                }

                operator.update_type_with_lhs(lhs_type);

                operator.resulting_type(lhs_type, rhs_type)
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

                if types.len() != 1 {
                    diagnostics.add_message(
                        CompilerErrorKind::FunctionMustReturnOneValueInThisLocation {
                            actual: types.len(),
                        }
                        .into_message(expression.span),
                    );
                    Type::Error
                } else {
                    types[0]
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
            diagnostics.add_message(
                CompilerErrorKind::CannotCallEventHandler {
                    function_span: function_info.span,
                    function_name: name.to_string(),
                }
                .into_message(span),
            );

            return vec![Type::Error];
        } else if argument_types.len() != function_info.args.len() {
            diagnostics.add_message(
                CompilerErrorKind::IncorrectNumberOfArguments {
                    expected: function_info.args.len(),
                    actual: argument_types.len(),
                    function_span: function_info.span,
                    function_name: name.to_string(),
                }
                .into_message(span),
            );
        } else {
            for ((actual, actual_span), arg_info) in argument_types.iter().zip(&function_info.args)
            {
                if *actual != arg_info.ty && *actual != Type::Error && arg_info.ty != Type::Error {
                    diagnostics.add_message(
                        CompilerErrorKind::FunctionArgumentTypeError {
                            function_name: function_info.name.to_string(),
                            argument_name: arg_info.name.to_string(),
                            expected: arg_info.ty,
                            expected_span: arg_info.span,
                            actual: *actual,
                            actual_span: *actual_span,
                        }
                        .into_message(*actual_span),
                    );
                }
            }
        }

        function_info.rets.clone()
    }
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
    #[cfg(test)]
    pub fn type_for_symbol(&self, symbol_id: SymbolId) -> Type {
        use crate::builtins::BuiltinVariable;

        if let Some(builtin) = BuiltinVariable::from_symbol_id(symbol_id) {
            return builtin.ty();
        }

        self.types[symbol_id.0 as usize]
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
        compile::{Property, loop_visitor, symtab_visitor::SymTabVisitor},
        grammar,
        lexer::Lexer,
        tokens::FileId,
        types::Type,
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

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
                enable_optimisations: false,
            };
            let mut symtab_visitor = SymTabVisitor::new(
                &settings,
                &mut script.functions,
                &mut script.extern_functions,
                &mut diagnostics,
            );

            let mut type_visitor = TypeVisitor::new(
                &settings,
                &script.functions,
                &script.extern_functions,
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

            assert!(
                !diagnostics.has_any(),
                "{} failed with error {}",
                path.display(),
                diagnostics.pretty_string(true)
            );

            let symtab = symtab_visitor.get_symtab();
            let type_table = type_visitor.into_type_table(symtab, &mut diagnostics);

            let all_types = symtab
                .all_symbols()
                .map(|(name, id)| (name, type_table.type_for_symbol(id)))
                .collect::<Vec<_>>();

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

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
                enable_optimisations: false,
            };
            let mut symtab_visitor = SymTabVisitor::new(
                &settings,
                &mut script.functions,
                &mut script.extern_functions,
                &mut diagnostics,
            );
            let mut type_visitor = TypeVisitor::new(
                &settings,
                &script.functions,
                &script.extern_functions,
                symtab_visitor.get_symtab(),
            );

            for function in &mut script.functions {
                loop_visitor::visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                let symtab = symtab_visitor.get_symtab();
                type_visitor.visit_function(function, symtab, &mut diagnostics);
            }

            type_visitor.into_type_table(symtab_visitor.get_symtab(), &mut diagnostics);

            let err_str = diagnostics.pretty_string(false);

            assert_snapshot!(err_str);
        });
    }
}
