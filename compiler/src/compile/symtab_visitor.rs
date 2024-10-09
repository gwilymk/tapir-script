use std::{borrow::Cow, collections::HashMap, mem};

use crate::{
    ast::{
        Expression, ExpressionKind, Function, MaybeResolved, Statement, StatementKind, SymbolId,
    },
    reporting::{CompilerErrorKind, Diagnostics},
    tokens::Span,
};

use super::{CompileSettings, Property};

pub struct SymTabVisitor<'input> {
    symtab: SymTab<'input>,

    symbol_names: NameTable<'input>,
}

impl<'input> SymTabVisitor<'input> {
    pub fn new(settings: &CompileSettings) -> Self {
        Self {
            symtab: SymTab::new(settings),
            symbol_names: NameTable::new(settings),
        }
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
            let kind = mem::take(&mut statement.kind);

            statement.kind = match kind {
                StatementKind::VariableDeclaration { ident, mut value } => {
                    self.visit_expr(&mut value, diagnostics);

                    let symbol_id = self.symtab.new_symbol(ident, statement.span);
                    self.symbol_names.insert(ident, symbol_id);

                    StatementKind::SymbolDeclare {
                        ident: symbol_id,
                        value,
                    }
                }
                StatementKind::Assignment { ident, mut value } => {
                    self.visit_expr(&mut value, diagnostics);

                    if let Some(symbol_id) = self.symbol_names.get(ident) {
                        StatementKind::SymbolAssign {
                            ident: symbol_id,
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
                StatementKind::If {
                    mut condition,
                    mut true_block,
                    mut false_block,
                } => {
                    self.visit_expr(&mut condition, diagnostics);

                    self.visit_block(&mut true_block, diagnostics);
                    self.visit_block(&mut false_block, diagnostics);

                    StatementKind::If {
                        condition,
                        true_block,
                        false_block,
                    }
                }
                StatementKind::Error
                | StatementKind::Wait
                | StatementKind::Nop
                | StatementKind::SymbolDeclare { .. }
                | StatementKind::SymbolAssign { .. } => kind,
                StatementKind::Return { mut values } => {
                    for expr in &mut values {
                        self.visit_expr(expr, diagnostics);
                    }

                    StatementKind::Return { values }
                }
            };
        }

        self.symbol_names.pop_scope();
    }

    fn visit_expr(&self, expr: &mut Expression<'_>, diagnostics: &mut Diagnostics) {
        let kind = mem::take(&mut expr.kind);

        expr.kind = match kind {
            ExpressionKind::Variable(ident) => {
                if let Some(symbol_id) = self.symbol_names.get(ident) {
                    ExpressionKind::Symbol(symbol_id)
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
            | ExpressionKind::Symbol(_)
            | ExpressionKind::Bool(_) => kind,
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
            .map(|(i, prop)| (Cow::Owned(prop.name.clone()), SymbolId(i)))
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

    pub fn get(&self, name: &str) -> Option<SymbolId> {
        for nametab in self.names.iter().rev() {
            if let Some(id) = nametab.get(name) {
                return Some(*id);
            }
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
}

impl<'input> SymTab<'input> {
    fn new(settings: &CompileSettings) -> Self {
        let properties = settings.properties.clone();
        let symbol_names = properties
            .iter()
            .map(|prop| (Cow::Owned(prop.name.clone()), None))
            .collect();

        Self {
            properties,
            symbol_names,
        }
    }

    fn new_symbol(&mut self, ident: &'input str, span: Span) -> SymbolId {
        self.symbol_names.push((Cow::Borrowed(ident), Some(span)));
        SymbolId(self.symbol_names.len() - 1)
    }

    pub(crate) fn name_for_symbol(&self, symbol_id: SymbolId) -> Cow<'input, str> {
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
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new();

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

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

            let mut diagnostics = Diagnostics::new();

            let mut script = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let mut visitor = SymTabVisitor::new(&CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            });

            for function in &mut script.functions {
                visitor.visit_function(function, &mut diagnostics);
            }

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
