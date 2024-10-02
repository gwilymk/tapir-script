use serde::Serialize;

use crate::{
    ast::{self, Expression, SymbolId, Visitor},
    reporting::CompilerErrorKind,
    tokens::Span,
    types::Type,
    Message,
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
    ) -> Result<(), Message> {
        if self.type_table.len() <= symbol_id.0 {
            self.type_table.resize(symbol_id.0 + 1, None);
        }

        if self.type_table[symbol_id.0].is_some_and(|table_type| table_type != ty) {
            return Err(CompilerErrorKind::TypeError {
                expected: self.type_table[symbol_id.0].unwrap(),
                actual: ty,
            }
            .into_message(span));
        }

        self.type_table[symbol_id.0] = Some(ty);

        Ok(())
    }

    pub fn get_type(
        &self,
        symbol_id: SymbolId,
        span: Span,
        symtab: &SymTab,
    ) -> Result<Type, Message> {
        match self.type_table.get(symbol_id.0) {
            Some(Some(ty)) => Ok(*ty),
            _ => Err(
                CompilerErrorKind::UnknownType(symtab.name_for_symbol(symbol_id))
                    .into_message(span),
            ),
        }
    }

    pub fn visit(&mut self, ast: &[ast::Statement<'_>], symtab: &SymTab) -> Result<(), Message> {
        for statement in ast {
            match &statement.kind {
                ast::StatementKind::Error(_) => {}
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
                        self.type_for_expression(value, symtab)?,
                        statement.span,
                    )?;
                }
                ast::StatementKind::SymbolAssign { ident, value } => {
                    self.resolve_type(
                        *ident,
                        self.type_for_expression(value, symtab)?,
                        statement.span,
                    )?;
                }
            }
        }

        Ok(())
    }

    pub fn into_type_table(self, symtab: &SymTab) -> Result<TypeTable, Message> {
        let mut types = Vec::with_capacity(self.type_table.len());
        for (i, ty) in self.type_table.into_iter().enumerate() {
            if let Some(ty) = ty {
                types.push(ty);
            } else {
                return Err(
                    CompilerErrorKind::UnknownType(symtab.name_for_symbol(SymbolId(i)))
                        .into_message(symtab.span_for_symbol(SymbolId(i))),
                );
            }
        }

        Ok(TypeTable { types })
    }

    fn type_for_expression(
        &self,
        expression: &Expression<'_>,
        symtab: &SymTab,
    ) -> Result<Type, Message> {
        match &expression.kind {
            ast::ExpressionKind::Integer(_) => Ok(Type::Int),
            ast::ExpressionKind::Fix(_) => Ok(Type::Fix),
            ast::ExpressionKind::Variable(_) => {
                unreachable!("Should have been removed by symbol resolution")
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_type = self.type_for_expression(lhs, symtab)?;
                let rhs_type = self.type_for_expression(rhs, symtab)?;

                if lhs_type != rhs_type {
                    return Err(
                        CompilerErrorKind::BinaryOperatorTypeError { lhs_type, rhs_type }
                            .into_message(expression.span),
                    );
                }

                if !operator.can_handle_type(lhs_type) {
                    return Err(CompilerErrorKind::InvalidTypeForBinaryOperator {
                        type_: lhs_type,
                    }
                    .into_message(lhs.span));
                }

                Ok(lhs_type)
            }
            ast::ExpressionKind::Error(_) => Ok(Type::Error),
            ast::ExpressionKind::Nop => Ok(Type::Error),
            ast::ExpressionKind::Symbol(symbol_id) => {
                self.get_type(*symbol_id, expression.span, symtab)
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

    use ast::Visitable;
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

            let mut ast = parser.parse(FileId::new(0), lexer).unwrap();

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };
            let mut symtab_visitor = SymTabVisitor::new(&settings);

            ast.visit(&mut symtab_visitor).unwrap();

            let symtab = symtab_visitor.into_symtab();

            let mut type_visitor = TypeVisitor::new(&settings);
            type_visitor.visit(&ast, &symtab).unwrap();

            let type_table = type_visitor.into_type_table(&symtab).unwrap();

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

            let mut ast = parser.parse(file_id, lexer).unwrap();

            let settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };
            let mut symtab_visitor = SymTabVisitor::new(&settings);

            ast.visit(&mut symtab_visitor).unwrap();

            let symtab = symtab_visitor.into_symtab();

            let mut type_visitor = TypeVisitor::new(&settings);
            let err = type_visitor.visit(&ast, &symtab).unwrap_err();

            let mut err_str = vec![];
            let mut diagnostic_cache = DiagnosticCache::new(iter::once((
                file_id,
                (path.to_string_lossy().to_string(), input.clone()),
            )));
            err.write_diagnostic(&mut err_str, &mut diagnostic_cache, false)
                .unwrap();

            let err_str = String::from_utf8_lossy(&err_str);

            assert_snapshot!(err_str);
        });
    }
}
