use serde::Serialize;

use crate::{
    ast::{self, Expression, SymbolId},
    reporting::{CompilerErrorKind, Diagnostics},
    tokens::Span,
    types::Type,
};

use super::{symtab_visitor::SymTab, CompileSettings};

pub struct TypeVisitor {
    type_table: Vec<Option<Type>>,
}

impl TypeVisitor {
    pub fn new(settings: &CompileSettings) -> Self {
        Self {
            type_table: settings
                .properties
                .iter()
                .map(|prop| Some(prop.ty))
                .collect(),
        }
    }

    pub fn resolve_type(
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
                    CompilerErrorKind::UnknownType(symtab.name_for_symbol(symbol_id))
                        .into_message(span),
                );

                Type::Error
            }
        }
    }

    pub fn visit(
        &mut self,
        ast: &[ast::Statement<'_>],
        symtab: &SymTab,
        diagnostics: &mut Diagnostics,
    ) {
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
                    self.resolve_type(
                        *ident,
                        self.type_for_expression(value, symtab, diagnostics),
                        statement.span,
                        diagnostics,
                    );
                }
                ast::StatementKind::SymbolAssign { ident, value } => {
                    self.resolve_type(
                        *ident,
                        self.type_for_expression(value, symtab, diagnostics),
                        statement.span,
                        diagnostics,
                    );
                }
            }
        }
    }

    pub fn into_type_table(self, symtab: &SymTab, diagnostics: &mut Diagnostics) -> TypeTable {
        let mut types = Vec::with_capacity(self.type_table.len());
        for (i, ty) in self.type_table.into_iter().enumerate() {
            if let Some(ty) = ty {
                types.push(ty);
            } else {
                diagnostics.add_message(
                    CompilerErrorKind::UnknownType(symtab.name_for_symbol(SymbolId(i)))
                        .into_message(symtab.span_for_symbol(SymbolId(i))),
                );
            }
        }

        TypeTable { types }
    }

    fn type_for_expression(
        &self,
        expression: &Expression<'_>,
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

                lhs_type
            }
            ast::ExpressionKind::Error => Type::Error,
            ast::ExpressionKind::Nop => Type::Error,
            ast::ExpressionKind::Symbol(symbol_id) => {
                self.get_type(*symbol_id, expression.span, symtab, diagnostics)
            }
        }
    }
}

#[derive(Clone, Serialize)]
pub struct TypeTable {
    types: Vec<Type>,
}

impl TypeTable {
    pub fn type_for_symbol(&self, symbol_id: SymbolId) -> Type {
        self.types[symbol_id.0]
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

            let mut ast = parser
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

            symtab_visitor.visit(&mut ast, &mut diagnostics);

            let symtab = symtab_visitor.into_symtab();

            let mut type_visitor = TypeVisitor::new(&settings);
            type_visitor.visit(&ast, &symtab, &mut diagnostics);

            let type_table = type_visitor.into_type_table(&symtab, &mut diagnostics);

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

            let mut ast = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };
            let mut symtab_visitor = SymTabVisitor::new(&settings);

            symtab_visitor.visit(&mut ast, &mut diagnostics);

            let symtab = symtab_visitor.into_symtab();

            let mut type_visitor = TypeVisitor::new(&settings);
            type_visitor.visit(&ast, &symtab, &mut diagnostics);

            type_visitor.into_type_table(&symtab, &mut diagnostics);

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
