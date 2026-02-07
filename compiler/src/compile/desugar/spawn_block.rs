use crate::ast::*;
use crate::tokens::Span;

use super::{DesugarPass, walk_expression};

/// Desugars `spawn { ... }` into a synthetic function definition + `spawn name()`.
pub(super) struct SpawnBlockDesugarer<'input> {
    counter: u32,
    synthetic_functions: Vec<Function<'input>>,
}

impl<'input> SpawnBlockDesugarer<'input> {
    pub(super) fn new() -> Self {
        Self {
            counter: 0,
            synthetic_functions: Vec::new(),
        }
    }
}

impl<'input> DesugarPass<'input> for SpawnBlockDesugarer<'input> {
    fn visit_expression(&mut self, expr: &mut Expression<'input>) {
        // Recurse first (bottom-up) so nested spawn blocks are handled
        walk_expression(self, expr);

        if let ExpressionKind::SpawnBlock { body } = &mut expr.kind {
            let name = format!("@spawn_block_{}", self.counter);
            self.counter += 1;

            let span = expr.span;

            let fn_decl = Function {
                name: name.clone(),
                statements: std::mem::take(body),
                arguments: vec![],
                return_types: FunctionReturn {
                    types: vec![],
                    span: Span::new(span.file_id, span.start(), span.start()),
                },
                span,
                kind: FunctionKind::Regular,
                modifiers: FunctionModifiers::default(),
                meta: Metadata::new(),
            };

            self.synthetic_functions.push(fn_decl);

            // Replace the SpawnBlock with a Spawn of the synthetic function
            expr.kind = ExpressionKind::Spawn {
                name,
                arguments: vec![],
            };
        }
    }

    fn finish(self, script: &mut Script<'input>) {
        script.functions.extend(self.synthetic_functions);
    }
}
