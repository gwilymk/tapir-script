use crate::{
    ast::{BinaryOperator, UnaryOperator},
    compile::{
        ir::{
            Constant, Operand, TapIr, TapIrFunction, TapIrFunctionBlockIter,
            optimisations::OptimisationResult,
        },
        symtab_visitor::SymTab,
    },
};

/// Tries to encode a raw i32 value as a direct or shifted immediate for Add/Sub.
/// Tries direct first (0..=255), then shifted (multiple of 16, /16 in 0..=255).
fn try_encode_add_sub(value: i32) -> Option<Operand> {
    if (0..=255).contains(&value) {
        return Some(Operand::Immediate(value as u8));
    } else if value % 16 == 0 {
        let shifted = value / 16;

        if (0..=255).contains(&shifted) {
            return Some(Operand::ShiftedImmediate(shifted as u8));
        }
    }

    None
}

/// Tries to encode a raw i32 value as a direct immediate for Mul/Div.
/// Only direct encoding is valid (shifted has different semantics).
fn try_encode_mul_div(value: i32) -> Option<Operand> {
    if (0..=255).contains(&value) {
        Some(Operand::Immediate(value as u8))
    } else {
        None
    }
}

/// Tries to encode a raw i32 value as a shifted immediate for FixMul/FixDiv.
/// Only shifted encoding is valid (direct has different semantics).
fn try_encode_fix_mul_div(value: i32) -> Option<Operand> {
    if value % 16 == 0 {
        let shifted = value / 16;

        if (0..=255).contains(&shifted) {
            return Some(Operand::ShiftedImmediate(shifted as u8));
        }
    }

    None
}

/// Try to encode a constant value as an immediate operand for the given operator.
fn try_encode(value: i32, op: BinaryOperator) -> Option<Operand> {
    // Never encode zero as an immediate for division — it hides the constant from the
    // divide-by-zero warning in constant_folding and would just be a runtime error anyway.
    if value == 0 && matches!(op, BinaryOperator::Div | BinaryOperator::FixDiv) {
        return None;
    }

    match op {
        BinaryOperator::Add | BinaryOperator::Sub => try_encode_add_sub(value),
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Shl | BinaryOperator::Shr => {
            try_encode_mul_div(value)
        }
        BinaryOperator::FixMul | BinaryOperator::FixDiv => try_encode_fix_mul_div(value),
        _ => None,
    }
}

/// Extract the raw i32 value from a constant (works for both Int and Fix).
fn raw_value(c: Constant) -> i32 {
    match c {
        Constant::Int(v) => v,
        Constant::Fix(f) => f.to_raw(),
        Constant::Bool(_) => 0,
    }
}

fn is_commutative(op: BinaryOperator) -> bool {
    matches!(
        op,
        BinaryOperator::Add | BinaryOperator::Mul | BinaryOperator::FixMul
    )
}

pub fn immediate_arithmetic(f: &mut TapIrFunction, symtab: &mut SymTab) -> OptimisationResult {
    let constants = f.constant_map();

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        let instrs = &mut block.instrs;
        let mut index = 0;

        while index < instrs.len() {
            let TapIr::BinOp {
                target,
                lhs,
                op,
                rhs,
            } = &instrs[index]
            else {
                index += 1;
                continue;
            };

            // Only handle Symbol operands (not already converted to immediate)
            let Operand::Symbol(rhs_sym) = *rhs else {
                index += 1;
                continue;
            };

            let target = *target;
            let lhs_sym = *lhs;
            let op = *op;
            let lhs_constant = constants.get(&lhs_sym).copied();
            let rhs_constant = constants.get(&rhs_sym).copied();

            // Case 1: RHS is a constant — try to make it immediate
            if let Some(rhs_val) = rhs_constant {
                let raw = raw_value(rhs_val);

                // Negative normalisation for Add/Sub
                if (op == BinaryOperator::Add || op == BinaryOperator::Sub)
                    && raw < 0
                    && let Some(abs) = raw.checked_neg()
                {
                    let flipped_op = if op == BinaryOperator::Add {
                        BinaryOperator::Sub
                    } else {
                        BinaryOperator::Add
                    };

                    if let Some(operand) = try_encode(abs, flipped_op) {
                        instrs[index] = TapIr::BinOp {
                            target,
                            lhs: lhs_sym,
                            op: flipped_op,
                            rhs: operand,
                        };
                        did_something = OptimisationResult::DidSomething;
                        index += 1;
                        continue;
                    }
                }

                // Direct encoding attempt
                if let Some(operand) = try_encode(raw, op) {
                    instrs[index] = TapIr::BinOp {
                        target,
                        lhs: lhs_sym,
                        op,
                        rhs: operand,
                    };
                    did_something = OptimisationResult::DidSomething;
                    index += 1;
                    continue;
                }
            }

            // Case 2: LHS is a constant and op is commutative — swap and try again
            if let Some(lhs_val) = lhs_constant {
                if is_commutative(op) {
                    let raw = raw_value(lhs_val);

                    // Negative normalisation for Add
                    if op == BinaryOperator::Add
                        && raw < 0
                        && let Some(abs) = raw.checked_neg()
                        && let Some(operand) = try_encode(abs, BinaryOperator::Sub)
                    {
                        instrs[index] = TapIr::BinOp {
                            target,
                            lhs: rhs_sym,
                            op: BinaryOperator::Sub,
                            rhs: operand,
                        };
                        did_something = OptimisationResult::DidSomething;
                        index += 1;
                        continue;
                    }

                    if let Some(operand) = try_encode(raw, op) {
                        instrs[index] = TapIr::BinOp {
                            target,
                            lhs: rhs_sym,
                            op,
                            rhs: operand,
                        };
                        did_something = OptimisationResult::DidSomething;
                        index += 1;
                        continue;
                    }
                }

                // Case 3: LHS is constant, op is Sub — rewrite as neg + add immediate
                if op == BinaryOperator::Sub
                    && let Some(operand) = try_encode(raw_value(lhs_val), BinaryOperator::Add)
                {
                    let neg_tmp = symtab.new_temporary();

                    // Replace the BinOp with: target = neg_tmp + imm
                    instrs[index] = TapIr::BinOp {
                        target,
                        lhs: neg_tmp,
                        op: BinaryOperator::Add,
                        rhs: operand,
                    };

                    // Insert neg before it
                    instrs.insert(
                        index,
                        TapIr::UnaryOp {
                            target: neg_tmp,
                            operand: rhs_sym,
                            op: UnaryOperator::Neg,
                        },
                    );
                    index += 2; // skip both the neg and the rewritten binop

                    did_something = OptimisationResult::DidSomething;
                    continue;
                }
            }

            index += 1;
        }
    }

    did_something
}
