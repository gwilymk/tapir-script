use std::collections::HashMap;

use serde::Serialize;

use crate::{
    ast::{
        self, BinaryOperator, Expression, Function, FunctionModifiers, FunctionReturn,
        MaybeResolved, SymbolId,
    },
    reporting::{CompilerErrorKind, Diagnostics},
    tokens::Span,
    types::{FunctionType, Type},
    Trigger,
};

use super::{loop_visitor::LoopContainsNoBreak, symtab_visitor::SymTab, CompileSettings};

pub struct TypeVisitor<'input> {
    type_table: Vec<Option<Type>>,
    functions: HashMap<&'input str, FunctionInfo>,

    trigger_types: HashMap<&'input str, TriggerInfo>,
}

#[derive(Clone, Copy, Debug)]
pub struct TriggerId(pub usize);

struct FunctionInfo {
    span: Span,
    ty: FunctionType,
    modifiers: FunctionModifiers,
}

#[derive(Serialize, Clone, Debug)]
struct TriggerInfo {
    span: Span,
    ty: Vec<Type>,
    index: usize,
}

impl<'input> TypeVisitor<'input> {
    pub fn new(
        settings: &CompileSettings,
        functions: &[Function<'input>],
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut resolved_functions = HashMap::new();

        for function in functions {
            let function_type = FunctionType {
                args: function.arguments.iter().map(|t| t.t.t).collect(),
                rets: function.return_types.types.iter().map(|t| t.t).collect(),
            };

            if let Some(already_resolved) = resolved_functions.insert(
                function.name,
                FunctionInfo {
                    span: function.span,
                    ty: function_type,
                    modifiers: function.modifiers.clone(),
                },
            ) {
                diagnostics.add_message(
                    CompilerErrorKind::FunctionAlreadyDeclared {
                        function_name: function.name.to_string(),
                        old_function_declaration: already_resolved.span,
                        new_function_declaration: function.span,
                    }
                    .into_message(function.span),
                );
            }
        }

        Self {
            type_table: settings
                .properties
                .iter()
                .map(|prop| Some(prop.ty))
                .collect(),

            functions: resolved_functions,
            trigger_types: HashMap::new(),
        }
    }

    fn resolve_type(
        &mut self,
        symbol_id: SymbolId,
        ty: Type,
        span: Span,
        diagnostics: &mut Diagnostics,
    ) {
        if self.type_table.len() <= symbol_id.0 {
            self.type_table.resize(symbol_id.0 + 1, None);
        }

        if self.type_table[symbol_id.0].is_some_and(|table_type| table_type != ty) {
            if ty == Type::Error {
                // the error should already be reported
                return;
            }

            diagnostics.add_message(
                CompilerErrorKind::TypeError {
                    expected: self.type_table[symbol_id.0].unwrap(),
                    actual: ty,
                }
                .into_message(span),
            );

            return;
        }

        self.type_table[symbol_id.0] = Some(ty);
    }

    pub fn get_type(
        &self,
        symbol_id: SymbolId,
        span: Span,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Type {
        match self.type_table.get(symbol_id.0) {
            Some(Some(ty)) => *ty,
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

            self.resolve_type(symbol_id, argument.t.t, argument.span, diagnostics);
        }

        if let FunctionModifiers {
            is_event_handler: Some(event_span),
        } = &function.modifiers
        {
            if !function.return_types.types.is_empty() {
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
                ast::StatementKind::VariableDeclaration { value, .. } => {
                    let ident: &SymbolId = statement
                        .meta
                        .get()
                        .expect("Should've been resolved by symbol resolution");
                    let expr_type = self.type_for_expression(value, symtab, diagnostics);
                    self.resolve_type(*ident, expr_type, statement.span, diagnostics);
                }
                ast::StatementKind::Assignment { value, .. } => {
                    let ident: &SymbolId = statement
                        .meta
                        .get()
                        .expect("Should've been resolved by symbol resolution");

                    let expr_type = self.type_for_expression(value, symtab, diagnostics);
                    self.resolve_type(*ident, expr_type, statement.span, diagnostics);
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
                    self.type_for_call(statement.span, name, arguments, symtab, diagnostics);
                }
                ast::StatementKind::Spawn { name, arguments } => {
                    self.type_for_call(statement.span, name, arguments, symtab, diagnostics);
                }
                ast::StatementKind::Loop { block } => {
                    match self.visit_block(block, symtab, expected_return_type, diagnostics) {
                        BlockAnalysisResult::AllBranchesReturn => {
                            return BlockAnalysisResult::AllBranchesReturn
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
                        .collect::<Vec<_>>();

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
            if let Some(ty) = ty {
                types.push(ty);
            } else {
                diagnostics.add_message(
                    CompilerErrorKind::UnknownType(
                        symtab.name_for_symbol(SymbolId(i)).into_owned(),
                    )
                    .into_message(symtab.span_for_symbol(SymbolId(i))),
                );
            }
        }

        TypeTable {
            types,
            num_function_returns: self
                .functions
                .iter()
                .map(|(name, function)| (*name, function.ty.rets.len()))
                .collect(),

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
                let types =
                    self.type_for_call(expression.span, name, arguments, symtab, diagnostics);

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

    fn type_for_call(
        &mut self,
        span: Span,
        name: &'input str,
        arguments: &mut [Expression<'input>],
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Vec<Type> {
        let argument_types: Vec<_> = arguments
            .iter_mut()
            .map(|arg| (self.type_for_expression(arg, symtab, diagnostics), arg.span))
            .collect();

        if let Some(function_info) = self.functions.get(name) {
            if function_info.modifiers.is_event_handler.is_some() {
                diagnostics.add_message(
                    CompilerErrorKind::CannotCallEventHandler {
                        function_span: function_info.span,
                        function_name: name.to_string(),
                    }
                    .into_message(span),
                );

                return vec![Type::Error];
            } else if argument_types.len() != function_info.ty.args.len() {
                diagnostics.add_message(
                    CompilerErrorKind::IncorrectNumberOfArguments {
                        expected: function_info.ty.args.len(),
                        actual: argument_types.len(),
                        function_span: function_info.span,
                        function_name: name.to_string(),
                    }
                    .into_message(span),
                );
            } else {
                for ((actual, actual_span), expected) in
                    argument_types.iter().zip(&function_info.ty.args)
                {
                    if actual != expected && *actual != Type::Error && *expected != Type::Error {
                        diagnostics.add_message(
                            CompilerErrorKind::TypeError {
                                expected: *expected,
                                actual: *actual,
                            }
                            .into_message(*actual_span),
                        );
                    }
                }
            }

            function_info.ty.rets.clone()
        } else {
            diagnostics.add_message(
                CompilerErrorKind::UnknownFunction {
                    name: name.to_string(),
                }
                .into_message(span),
            );

            vec![Type::Error]
        }
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
    num_function_returns: HashMap<&'input str, usize>,

    triggers: HashMap<&'input str, TriggerInfo>,
}

impl TypeTable<'_> {
    #[cfg(test)]
    pub fn type_for_symbol(&self, symbol_id: SymbolId) -> Type {
        self.types[symbol_id.0]
    }

    pub fn num_function_returns(&self, fn_name: &str) -> usize {
        self.num_function_returns[fn_name]
    }

    pub fn triggers(&self) -> Vec<Trigger> {
        let mut result = vec![];
        result.resize_with(self.triggers.len(), || Trigger {
            name: String::new(),
            arguments: vec![],
        });

        for (name, info) in &self.triggers {
            result[info.index] = Trigger {
                name: name.to_string(),
                arguments: info.ty.clone(),
            };
        }

        result
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{
        compile::{loop_visitor, symtab_visitor::SymTabVisitor, Property},
        grammar,
        lexer::Lexer,
        tokens::FileId,
        types::Type,
    };

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "type_visitor/*_success.tapir", |path| {
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
            let mut symtab_visitor = SymTabVisitor::new(&settings);

            let mut type_visitor = TypeVisitor::new(&settings, &script.functions, &mut diagnostics);

            for function in &mut script.functions {
                loop_visitor::visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);

                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            assert!(!diagnostics.has_any(), "{} failed", path.display());

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
            let mut symtab_visitor = SymTabVisitor::new(&settings);
            let mut type_visitor = TypeVisitor::new(&settings, &script.functions, &mut diagnostics);

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
