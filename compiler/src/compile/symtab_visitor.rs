use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionKind, StatementKind, SymbolId, VResult, Visitable, Visitor},
    reporting::CompilerErrorKind,
    tokens::Span,
};

use super::{CompileSettings, Property};

pub struct SymTabVisitor {
    symtab: SymTab,

    symbol_names: HashMap<String, SymbolId>,
}

impl SymTabVisitor {
    pub fn new(settings: &CompileSettings) -> Self {
        Self {
            symtab: SymTab::new(settings),
            symbol_names: settings
                .properties
                .iter()
                .enumerate()
                .map(|(i, prop)| (prop.name.clone(), SymbolId(i)))
                .collect(),
        }
    }

    pub fn into_symtab(self) -> SymTab {
        self.symtab
    }
}

impl Visitor<()> for SymTabVisitor {
    fn visit_assignment<'input>(
        &mut self,
        ident: &'input str,
        mut value: Box<Expression<'input>>,
        span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;

        let symbol_id = if let Some(symbol_id) = self.symbol_names.get(ident) {
            *symbol_id
        } else {
            return Ok(StatementKind::Error(
                CompilerErrorKind::UnknownVariable(ident.to_string()).into_message(span),
            ));
        };

        Ok(StatementKind::SymbolAssign {
            ident: symbol_id,
            value,
        })
    }

    fn visit_variable_declaration<'input>(
        &mut self,
        ident: &'input str,
        mut value: Box<Expression<'input>>,
        span: Span,
    ) -> VResult<StatementKind<'input>> {
        value.visit(self)?;

        let symbol_id = self.symtab.new_symbol(ident.to_string(), span);
        self.symbol_names.insert(ident.to_string(), symbol_id);

        Ok(StatementKind::SymbolDeclare {
            ident: symbol_id,
            value,
        })
    }

    fn visit_variable<'input>(
        &mut self,
        ident: &'input str,
        span: Span,
    ) -> VResult<ExpressionKind<'input>> {
        Ok(if let Some(symbol_id) = self.symbol_names.get(ident) {
            ExpressionKind::Symbol(*symbol_id)
        } else {
            ExpressionKind::Error(
                CompilerErrorKind::UnknownVariable(ident.to_string()).into_message(span),
            )
        })
    }
}

pub struct SymTab {
    properties: Vec<Property>,

    symbol_names: Vec<(String, Option<Span>)>,
}

impl SymTab {
    fn new(settings: &CompileSettings) -> Self {
        let properties = settings.properties.clone();
        let symbol_names = properties
            .iter()
            .map(|prop| (prop.name.clone(), None))
            .collect();

        Self {
            properties,
            symbol_names,
        }
    }

    fn new_symbol(&mut self, ident: String, span: Span) -> SymbolId {
        self.symbol_names.push((ident, Some(span)));
        SymbolId(self.symbol_names.len() - 1)
    }

    pub(crate) fn name_for_symbol(&self, symbol_id: SymbolId) -> String {
        self.symbol_names[symbol_id.0].0.clone()
    }

    pub(crate) fn span_for_symbol(&self, symbol_id: SymbolId) -> Span {
        self.symbol_names[symbol_id.0]
            .1
            .expect("Symbol should have a span")
    }

    pub fn all_symbols(&self) -> impl Iterator<Item = (&'_ str, SymbolId)> + '_ {
        self.symbol_names
            .iter()
            .enumerate()
            .map(|(i, (name, _span))| (name.as_ref(), SymbolId(i)))
    }

    pub fn get_property(&self, symbol_id: SymbolId) -> Option<&Property> {
        self.properties.get(symbol_id.0)
    }
}

#[cfg(test)]
mod test {
    use std::{fs, iter};

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{
        grammar, grammar_test::ErrorVisitor, lexer::Lexer, tokens::FileId, types::Type,
        DiagnosticCache,
    };

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "symtab_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();

            let mut ast = parser.parse(FileId::new(0), lexer).unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

            ast.visit(&mut visitor).unwrap();

            assert_ron_snapshot!(ast, {
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

            let mut ast = parser.parse(file_id, lexer).unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

            ast.visit(&mut visitor).unwrap();

            let mut error_visitor = ErrorVisitor::default();
            ast.visit(&mut error_visitor).unwrap();

            let errors = error_visitor.errors;

            let mut output = Vec::new();
            let mut diagnostics_cache = DiagnosticCache::new(iter::once((
                file_id,
                (path.to_string_lossy().to_string(), input.clone()),
            )));

            for error in errors {
                error
                    .write_diagnostic(&mut output, &mut diagnostics_cache, false)
                    .unwrap();
            }

            let error_str = String::from_utf8_lossy(&output);

            assert_snapshot!(error_str);
        });
    }
}
