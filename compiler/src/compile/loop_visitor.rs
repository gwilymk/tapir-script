use crate::{
    ast::{Function, Statement, StatementKind},
    reporting::{DiagnosticMessage, Diagnostics, ErrorKind},
};

#[derive(Debug, Clone, Copy)]
pub struct LoopContainsNoBreak;

pub fn visit_loop_check(function: &mut Function<'_>, diagnostics: &mut Diagnostics) {
    visit_block_in_loop(&mut function.statements, false, diagnostics);
}

#[derive(Debug, PartialEq, Eq)]
enum LoopReturn {
    ContainsNoBreaks,
    MayBreak,
}

fn visit_block_in_loop(
    block: &mut [Statement<'_>],
    is_in_loop: bool,
    diagnostics: &mut Diagnostics,
) -> LoopReturn {
    let mut result = LoopReturn::ContainsNoBreaks;

    for statement in block {
        match &mut statement.kind {
            StatementKind::Error
            | StatementKind::VariableDeclaration { .. }
            | StatementKind::Assignment { .. }
            | StatementKind::Wait
            | StatementKind::Nop
            | StatementKind::Trigger { .. }
            | StatementKind::Expression { .. } => {}
            StatementKind::Return { .. } => {
                // Not returning to ensure that we catch continues and break outside of loops
            }
            StatementKind::Continue => {
                if !is_in_loop {
                    ErrorKind::BreakOrContinueOutsideOfLoop
                        .at(statement.span, DiagnosticMessage::ThisStatement)
                        .emit(diagnostics);
                }
            }
            StatementKind::Break => {
                if !is_in_loop {
                    ErrorKind::BreakOrContinueOutsideOfLoop
                        .at(statement.span, DiagnosticMessage::ThisStatement)
                        .emit(diagnostics);
                }

                result = LoopReturn::MayBreak;
            }
            StatementKind::Block { block } => {
                if visit_block_in_loop(block, is_in_loop, diagnostics) == LoopReturn::MayBreak {
                    result = LoopReturn::MayBreak;
                }
            }
            StatementKind::If {
                true_block,
                false_block,
                ..
            } => {
                let true_case = visit_block_in_loop(true_block, is_in_loop, diagnostics);
                let false_case = visit_block_in_loop(false_block, is_in_loop, diagnostics);
                if true_case == LoopReturn::MayBreak || false_case == LoopReturn::MayBreak {
                    result = LoopReturn::MayBreak;
                }
            }
            StatementKind::Loop { block } => {
                if visit_block_in_loop(block, true, diagnostics) == LoopReturn::ContainsNoBreaks {
                    statement.meta.set(LoopContainsNoBreak);
                }
            }
        }
    }

    result
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, assert_snapshot, glob};

    use crate::{grammar, lexer::Lexer, tokens::FileId};

    use super::*;

    #[test]
    fn symtab_success_snapshot_tests() {
        glob!("snapshot_tests", "loop_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
            }

            assert_ron_snapshot!(script, {
                ".**.span" => "[span]",
            });
        });
    }

    #[test]
    fn symtab_fail_snapshot_tests() {
        glob!("snapshot_tests", "loop_visitor/*_fail.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
            }

            assert_snapshot!(diagnostics.pretty_string(false));
        });
    }
}
