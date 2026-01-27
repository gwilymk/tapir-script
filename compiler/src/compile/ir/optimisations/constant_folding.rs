use std::collections::HashMap;

use agb_fixnum::Num;

use crate::{
    ast::BinaryOperator,
    compile::{
        ir::{
            Constant, SymbolSpans, TapIr, TapIrFunction, TapIrFunctionBlockIter,
            optimisations::OptimisationResult,
        },
        symtab_visitor::SymTab,
    },
    reporting::{DiagnosticMessage, Diagnostics, WarningKind},
    tokens::Span,
};

/// Errors that can occur during constant folding.
#[derive(Debug, Clone, Copy)]
enum FoldError {
    DivisionByZero,
    IntegerOverflow,
}

impl FoldError {
    fn warning_kind(self) -> WarningKind {
        match self {
            FoldError::DivisionByZero => WarningKind::DivisionByZero,
            FoldError::IntegerOverflow => WarningKind::IntegerOverflow,
        }
    }

    fn secondary_label(self) -> DiagnosticMessage {
        match self {
            FoldError::DivisionByZero => DiagnosticMessage::ReducesToZero,
            FoldError::IntegerOverflow => DiagnosticMessage::CausesOverflow,
        }
    }
}

pub fn constant_folding(
    f: &mut TapIrFunction,
    symtab: &mut SymTab,
    symbol_spans: &SymbolSpans,
    diagnostics: &mut Diagnostics,
) -> OptimisationResult {
    let mut constants = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next(f) {
        for instr in block.instrs() {
            let TapIr::Constant(target, value) = *instr else {
                continue;
            };

            if constants.insert(target, value).is_some() {
                panic!("Should only be assigned once, because SSA");
            }
        }
    }

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        let mut index = 0;
        let instrs = &mut block.instrs;

        while index < instrs.len() {
            let instr = &mut instrs[index];
            index += 1;

            let TapIr::BinOp {
                target,
                lhs,
                op,
                rhs,
            } = instr
            else {
                continue;
            };

            let lhs_constant = constants.get(lhs).copied();
            let rhs_constant = constants.get(rhs).copied();

            use BinaryOperator as B;
            use Constant as C;

            let t = *target;

            let f0 = Num::new(0);

            let take_lhs = TapIr::Move {
                target: t,
                source: *lhs,
            };
            let take_rhs = TapIr::Move {
                target: t,
                source: *rhs,
            };

            // Helper to emit a warning when folding fails
            let emit_warning = |err: FoldError,
                                diagnostics: &mut Diagnostics,
                                target_span: Option<Span>,
                                secondary_span: Option<Span>| {
                if let Some(op_span) = target_span {
                    let mut builder = err
                        .warning_kind()
                        .at(op_span)
                        .label(op_span, DiagnosticMessage::OperationOccursHere);
                    if let Some(secondary_span) = secondary_span {
                        builder = builder.label(secondary_span, err.secondary_label());
                    }
                    builder.emit(diagnostics);
                }
            };

            let target_span = symbol_spans.get(*target);
            let rhs_span = symbol_spans.get(*rhs);

            let replacement = match (lhs_constant, *op, rhs_constant) {
                // ==================
                // Integer operations
                // ==================
                (Some(C::Int(i1)), op, Some(C::Int(i2))) => match int_op(i1, op, i2) {
                    Ok(c) => TapIr::Constant(t, c),
                    Err(err) => {
                        emit_warning(err, diagnostics, target_span, rhs_span);
                        continue;
                    }
                },

                // ==============
                // Fix operations
                // ==============
                (Some(C::Fix(n1)), op, Some(C::Fix(n2))) => match fix_op(n1, op, n2) {
                    Ok(c) => TapIr::Constant(t, c),
                    Err(err) => {
                        emit_warning(err, diagnostics, target_span, rhs_span);
                        continue;
                    }
                },

                // ====================
                // Fix / int operations
                // ====================
                (Some(C::Fix(n1)), op, Some(C::Int(i2))) => match fix_int_op(n1, op, i2) {
                    Ok(c) => TapIr::Constant(t, c),
                    Err(err) => {
                        emit_warning(err, diagnostics, target_span, rhs_span);
                        continue;
                    }
                },

                // ===================
                // Add / subtract zero
                // ===================
                (_, B::Add | B::Sub, Some(C::Int(0))) => take_lhs,
                (Some(C::Int(0)), B::Add | B::Sub, _) => take_rhs,
                (_, B::Add | B::Sub, Some(C::Fix(n))) if n == f0 => take_lhs,
                (Some(C::Fix(n)), B::Add | B::Sub, _) if n == f0 => take_rhs,

                // ================
                // Multiply by zero
                // ================
                (_, B::Mul | B::FixMul, Some(C::Int(0))) => TapIr::Constant(t, C::Int(0)),
                (Some(C::Int(0)), B::Mul | B::FixMul, _) => TapIr::Constant(t, C::Int(0)),

                (_, B::Mul | B::FixMul, Some(C::Fix(n))) if n == f0 => {
                    TapIr::Constant(t, C::Fix(f0))
                }
                (Some(C::Fix(n)), B::Mul | B::FixMul, _) if n == f0 => {
                    TapIr::Constant(t, C::Fix(f0))
                }

                (Some(C::Int(0)), B::Div | B::RealDiv | B::FixDiv, _) => {
                    TapIr::Constant(t, C::Int(0))
                }
                (Some(C::Fix(n)), B::Div | B::RealDiv | B::FixDiv, _) if n == f0 => {
                    TapIr::Constant(t, C::Fix(f0))
                }

                // ======================
                // Multiply / divide by 1
                // ======================
                (_, B::Mul | B::FixMul | B::Div | B::RealDiv | B::FixDiv, Some(C::Int(1))) => {
                    take_lhs
                }
                (Some(C::Int(1)), B::Mul | B::FixMul, _) => take_rhs,

                // ================================
                // Multiply / div by an integer fix
                // ================================
                (_, op @ (B::FixMul | B::FixDiv), Some(C::Fix(n))) if n.frac() == 0 => {
                    let temp = symtab.new_temporary();

                    *instr = TapIr::BinOp {
                        target: t,
                        lhs: *lhs,
                        op: if op == B::FixMul { B::Mul } else { B::Div },
                        rhs: temp,
                    };

                    instrs.insert(index - 1, TapIr::Constant(temp, C::Int(n.floor())));

                    did_something = OptimisationResult::DidSomething;
                    continue;
                }
                (Some(C::Fix(n)), op @ (B::FixMul | B::FixDiv), _) if n.frac() == 0 => {
                    let temp = symtab.new_temporary();

                    *instr = TapIr::BinOp {
                        target: t,
                        lhs: temp,
                        op: if op == B::FixMul { B::Mul } else { B::Div },
                        rhs: *rhs,
                    };

                    instrs.insert(index - 1, TapIr::Constant(temp, C::Int(n.floor())));

                    did_something = OptimisationResult::DidSomething;
                    continue;
                }

                _ => continue,
            };

            did_something = OptimisationResult::DidSomething;
            *instr = replacement;
        }
    }

    did_something
}

fn int_op(i1: i32, op: BinaryOperator, i2: i32) -> Result<Constant, FoldError> {
    use Constant as C;

    match op {
        BinaryOperator::Add => i1
            .checked_add(i2)
            .map(C::Int)
            .ok_or(FoldError::IntegerOverflow),
        BinaryOperator::Sub => i1
            .checked_sub(i2)
            .map(C::Int)
            .ok_or(FoldError::IntegerOverflow),
        BinaryOperator::Mul => i1
            .checked_mul(i2)
            .map(C::Int)
            .ok_or(FoldError::IntegerOverflow),
        BinaryOperator::Div => i1
            .checked_div(i2)
            .map(C::Int)
            .ok_or(FoldError::DivisionByZero),
        BinaryOperator::Mod => {
            if i2 == 0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Int(i1.rem_euclid(i2)))
            }
        }
        BinaryOperator::RealDiv => i1
            .checked_div(i2)
            .map(C::Int)
            .ok_or(FoldError::DivisionByZero),
        BinaryOperator::RealMod => i1
            .checked_rem(i2)
            .map(C::Int)
            .ok_or(FoldError::DivisionByZero),
        BinaryOperator::FixMul => panic!("Should never be fixmuling 2 integers"),
        BinaryOperator::FixDiv => panic!("Should never be fixdivving 2 integers"),
        BinaryOperator::EqEq => Ok(C::Bool(i1 == i2)),
        BinaryOperator::NeEq => Ok(C::Bool(i1 != i2)),
        BinaryOperator::Gt => Ok(C::Bool(i1 > i2)),
        BinaryOperator::GtEq => Ok(C::Bool(i1 >= i2)),
        BinaryOperator::Lt => Ok(C::Bool(i1 < i2)),
        BinaryOperator::LtEq => Ok(C::Bool(i1 <= i2)),
        BinaryOperator::Then => Ok(C::Int(i2)),
        BinaryOperator::And => panic!("Should never be &&ing two integers"),
        BinaryOperator::Or => panic!("Should never be ||ing two integers"),
    }
}

fn fix_op(n1: Num<i32, 8>, op: BinaryOperator, n2: Num<i32, 8>) -> Result<Constant, FoldError> {
    use Constant as C;

    // Note: agb_fixnum doesn't have checked operations, so we can't detect overflow here.
    // Division by zero will still panic, so we check for it explicitly.
    let f0 = Num::new(0);

    match op {
        BinaryOperator::Add => Ok(C::Fix(n1 + n2)),
        BinaryOperator::Sub => Ok(C::Fix(n1 - n2)),
        BinaryOperator::Mul => panic!("Should never multiply fixnums"),
        BinaryOperator::Div => panic!("Should never be div for fixnums"),
        BinaryOperator::Mod => panic!("Should never mod fixnums"),
        BinaryOperator::RealDiv => panic!("Should never realdiv fixnums"),
        BinaryOperator::RealMod => panic!("Should never realmod fixnums"),
        BinaryOperator::FixMul => Ok(C::Fix(n1 * n2)),
        BinaryOperator::FixDiv => {
            if n2 == f0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Fix(n1 / n2))
            }
        }
        BinaryOperator::EqEq => Ok(C::Bool(n1 == n2)),
        BinaryOperator::NeEq => Ok(C::Bool(n1 != n2)),
        BinaryOperator::Gt => Ok(C::Bool(n1 > n2)),
        BinaryOperator::GtEq => Ok(C::Bool(n1 >= n2)),
        BinaryOperator::Lt => Ok(C::Bool(n1 < n2)),
        BinaryOperator::LtEq => Ok(C::Bool(n1 <= n2)),
        BinaryOperator::Then => Ok(C::Fix(n2)),
        BinaryOperator::And => panic!("Should never be &&ing two fixnums"),
        BinaryOperator::Or => panic!("Should never be ||ing two fixnums"),
    }
}

fn fix_int_op(n1: Num<i32, 8>, op: BinaryOperator, i2: i32) -> Result<Constant, FoldError> {
    use Constant as C;

    match op {
        BinaryOperator::Add => Ok(C::Fix(n1 + i2)),
        BinaryOperator::Sub => Ok(C::Fix(n1 - i2)),
        BinaryOperator::Mul => Ok(C::Fix(n1 * i2)),
        BinaryOperator::Div => {
            if i2 == 0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Fix(n1 / i2))
            }
        }
        BinaryOperator::Mod => {
            if i2 == 0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Fix(n1.rem_euclid(i2.into())))
            }
        }
        BinaryOperator::RealDiv => {
            if i2 == 0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Fix(n1 / i2))
            }
        }
        BinaryOperator::RealMod => {
            if i2 == 0 {
                Err(FoldError::DivisionByZero)
            } else {
                Ok(C::Fix(n1 % i2))
            }
        }
        BinaryOperator::Then => Ok(C::Int(i2)),
        _ => panic!("Invalid operation {op} on fix int"),
    }
}
