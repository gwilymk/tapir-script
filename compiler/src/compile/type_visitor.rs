use std::collections::HashMap;

use serde::Serialize;

use crate::{
    ast::{self, Expression, Function, FunctionReturn, MaybeResolved, SymbolId},
    reporting::{CompilerErrorKind, Diagnostics},
    tokens::Span,
    types::{FunctionType, Type},
};

use super::{symtab_visitor::SymTab, CompileSettings};

pub struct TypeVisitor<'input> {
    type_table: Vec<Option<Type>>,
    functions: HashMap<&'input str, (Span, FunctionType)>,
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

            if let Some((already_resolved_span, _)) =
                resolved_functions.insert(function.name, (function.span, function_type))
            {
                diagnostics.add_message(
                    CompilerErrorKind::FunctionAlreadyDeclared {
                        function_name: function.name.to_string(),
                        old_function_declaration: already_resolved_span,
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

        let block_analysis_result = self.visit_block(
            &function.statements,
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
        ast: &[ast::Statement<'input>],
        symtab: &SymTab,
        expected_return_type: &FunctionReturn,
        diagnostics: &mut Diagnostics,
    ) -> BlockAnalysisResult {
        for statement in ast {
            match &statement.kind {
                ast::StatementKind::Error => {}
                ast::StatementKind::VariableDeclaration { .. } => {
                    unreachable!("Should have been removed by symbol resolution")
                }
                ast::StatementKind::Assignment { .. } => {
                    unreachable!("Should have been removed by symbol resolution")
                }
                ast::StatementKind::Wait => {}
                ast::StatementKind::Nop => {}
                ast::StatementKind::SymbolDeclare { ident, value } => {
                    let expr_type = self.type_for_expression(value, symtab, diagnostics);
                    self.resolve_type(*ident, expr_type, statement.span, diagnostics);
                }
                ast::StatementKind::SymbolAssign { ident, value } => {
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
                    for value in values {
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
                .map(|(name, function)| (*name, function.1.rets.len()))
                .collect(),
        }
    }

    fn type_for_expression(
        &mut self,
        expression: &Expression<'input>,
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Type {
        match &expression.kind {
            ast::ExpressionKind::Integer(_) => Type::Int,
            ast::ExpressionKind::Fix(_) => Type::Fix,
            ast::ExpressionKind::Bool(_) => Type::Bool,
            ast::ExpressionKind::Variable(_) => {
                unreachable!("Should have been removed by symbol resolution")
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_type = self.type_for_expression(lhs, symtab, diagnostics);
                let rhs_type = self.type_for_expression(rhs, symtab, diagnostics);

                if lhs_type == Type::Error || rhs_type == Type::Error {
                    // we don't need to report the same error multiple times
                    return Type::Error;
                }

                if lhs_type != rhs_type {
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

                operator.resulting_type(lhs_type)
            }
            ast::ExpressionKind::Error => Type::Error,
            ast::ExpressionKind::Nop => Type::Error,
            ast::ExpressionKind::Symbol(symbol_id) => {
                self.get_type(*symbol_id, expression.span, symtab, diagnostics)
            }
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
        arguments: &[Expression<'input>],
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) -> Vec<Type> {
        let argument_types: Vec<_> = arguments
            .iter()
            .map(|arg| (self.type_for_expression(arg, symtab, diagnostics), arg.span))
            .collect();

        if let Some((function_span, function_type)) = self.functions.get(name) {
            if argument_types.len() != function_type.args.len() {
                diagnostics.add_message(
                    CompilerErrorKind::IncorrectNumberOfArguments {
                        expected: function_type.args.len(),
                        actual: argument_types.len(),
                        function_span: *function_span,
                        function_name: name.to_string(),
                    }
                    .into_message(span),
                );
            } else {
                for ((actual, actual_span), expected) in
                    argument_types.iter().zip(&function_type.args)
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

            function_type.rets.clone()
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
}

impl TypeTable<'_> {
    pub fn type_for_symbol(&self, symbol_id: SymbolId) -> Type {
        self.types[symbol_id.0]
    }

    pub fn num_function_returns(&self, fn_name: &str) -> usize {
        self.num_function_returns[fn_name]
    }
}

#[cfg(test)]
mod test {
    use std::{fs, iter};

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{
        compile::{symtab_visitor::SymTabVisitor, Property},
        grammar,
        lexer::Lexer,
        tokens::FileId,
        types::Type,
        DiagnosticCache,
    };

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "type_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();

            let mut diagnostics = Diagnostics::new();

            let mut script = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };
            let mut symtab_visitor = SymTabVisitor::new(&settings);

            let mut type_visitor = TypeVisitor::new(&settings, &script.functions, &mut diagnostics);

            for function in &mut script.functions {
                symtab_visitor.visit_function(function, &mut diagnostics);

                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

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

            let mut diagnostics = Diagnostics::new();

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };
            let mut symtab_visitor = SymTabVisitor::new(&settings);
            let mut type_visitor = TypeVisitor::new(&settings, &script.functions, &mut diagnostics);

            for function in &mut script.functions {
                symtab_visitor.visit_function(function, &mut diagnostics);
                let symtab = symtab_visitor.get_symtab();
                type_visitor.visit_function(function, symtab, &mut diagnostics);
            }

            type_visitor.into_type_table(symtab_visitor.get_symtab(), &mut diagnostics);

            let mut err_str = vec![];
            let mut diagnostic_cache = DiagnosticCache::new(iter::once((
                file_id,
                (path.to_string_lossy().to_string(), input.clone()),
            )));
            diagnostics
                .write(&mut err_str, &mut diagnostic_cache, false)
                .unwrap();

            let err_str = String::from_utf8_lossy(&err_str);

            assert_snapshot!(err_str);
        });
    }
}
