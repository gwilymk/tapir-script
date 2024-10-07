use std::{collections::HashMap, mem};

use crate::{
    ast::{Expression, ExpressionKind, Statement, StatementKind, SymbolId},
    reporting::{CompilerErrorKind, Diagnostics},
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

    pub fn visit(&mut self, ast: &mut [Statement<'_>], diagnostics: &mut Diagnostics) {
        for statement in ast {
            let kind = mem::take(&mut statement.kind);

            statement.kind = match kind {
                StatementKind::VariableDeclaration { ident, mut value } => {
                    self.visit_expr(&mut value, diagnostics);

                    let symbol_id = self.symtab.new_symbol(ident.to_string(), statement.span);
                    self.symbol_names.insert(ident.to_string(), symbol_id);

                    StatementKind::SymbolDeclare {
                        ident: symbol_id,
                        value,
                    }
                }
                StatementKind::Assignment { ident, mut value } => {
                    self.visit_expr(&mut value, diagnostics);

                    if let Some(symbol_id) = self.symbol_names.get(ident) {
                        StatementKind::SymbolAssign {
                            ident: *symbol_id,
                            value,
                        }
                    } else {
                        diagnostics.add_message(
                            CompilerErrorKind::UnknownVariable(ident.to_string())
                                .into_message(statement.span),
                        );
                        StatementKind::Error
                    }
                }
                StatementKind::Error
                | StatementKind::Wait
                | StatementKind::Nop
                | StatementKind::SymbolDeclare { .. }
                | StatementKind::SymbolAssign { .. } => kind,
            };
        }
    }

    fn visit_expr(&self, expr: &mut Expression<'_>, diagnostics: &mut Diagnostics) {
        let kind = mem::take(&mut expr.kind);

        expr.kind = match kind {
            ExpressionKind::Variable(ident) => {
                if let Some(symbol_id) = self.symbol_names.get(ident) {
                    ExpressionKind::Symbol(*symbol_id)
                } else {
                    diagnostics.add_message(
                        CompilerErrorKind::UnknownVariable(ident.to_string())
                            .into_message(expr.span),
                    );

                    ExpressionKind::Error
                }
            }
            ExpressionKind::BinaryOperation {
                mut lhs,
                operator,
                mut rhs,
            } => {
                self.visit_expr(&mut lhs, diagnostics);
                self.visit_expr(&mut rhs, diagnostics);

                ExpressionKind::BinaryOperation { lhs, operator, rhs }
            }
            ExpressionKind::Integer(_)
            | ExpressionKind::Fix(_)
            | ExpressionKind::Error
            | ExpressionKind::Nop
            | ExpressionKind::Symbol(_) => kind,
        }
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

    use crate::{grammar, lexer::Lexer, tokens::FileId, types::Type, DiagnosticCache};

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "symtab_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();

            let mut diagnostics = Diagnostics::new();

            let mut ast = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

            visitor.visit(&mut ast, &mut diagnostics);

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

            let mut diagnostics = Diagnostics::new();

            let mut ast = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

            visitor.visit(&mut ast, &mut diagnostics);

            let mut output = Vec::new();
            let mut diagnostics_cache = DiagnosticCache::new(iter::once((
                file_id,
                (path.to_string_lossy().to_string(), input.clone()),
            )));

            diagnostics
                .write(&mut output, &mut diagnostics_cache, false)
                .unwrap();

            let error_str = String::from_utf8_lossy(&output);

            assert_snapshot!(error_str);
        });
    }
}
