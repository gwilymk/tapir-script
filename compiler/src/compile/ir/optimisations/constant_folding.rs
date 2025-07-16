use std::collections::HashMap;

use agb_fixnum::Num;

use crate::{
    ast::BinaryOperator,
    compile::{
        ir::{
            Constant, TapIr, TapIrFunction, TapIrFunctionBlockIter, TapIrInstr,
            optimisations::OptimisationResult,
        },
        symtab_visitor::SymTab,
    },
};

pub fn constant_folding(f: &mut TapIrFunction, symtab: &mut SymTab) -> OptimisationResult {
    let mut constants = HashMap::new();

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next(f) {
        for instr in block.instrs() {
            let TapIrInstr::Constant(target, value) = instr.instr else {
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

            let TapIrInstr::BinOp {
                target,
                lhs,
                op,
                rhs,
            } = &mut instr.instr
            else {
                continue;
            };

            let lhs_constant = constants.get(lhs).copied();
            let rhs_constant = constants.get(rhs).copied();

            let t = *target;

            use BinaryOperator as B;
            use Constant as C;

            let f0 = Num::new(0);

            let take_lhs = TapIrInstr::Move {
                target: t,
                source: *lhs,
            };
            let take_rhs = TapIrInstr::Move {
                target: t,
                source: *rhs,
            };

            let replacement = match (lhs_constant, *op, rhs_constant) {
                // ==================
                // Integer operations
                // ==================
                (Some(C::Int(i1)), op, Some(C::Int(i2))) => {
                    TapIrInstr::Constant(t, int_op(i1, op, i2))
                }

                // ==============
                // Fix operations
                // ==============
                (Some(C::Fix(n1)), op, Some(C::Fix(n2))) => {
                    TapIrInstr::Constant(t, fix_op(n1, op, n2))
                }

                // ====================
                // Fix / int operations
                // ====================
                (Some(C::Fix(n1)), op, Some(C::Int(i2))) => {
                    TapIrInstr::Constant(t, fix_int_op(n1, op, i2))
                }

                // ===================
                // Add / subtract zero
                // ===================
                (_, B::Add | B::Sub, Some(C::Int(0))) => take_lhs,
                (Some(C::Int(0)), B::Add | B::Sub, _) => take_rhs,

                // ================
                // Multiply by zero
                // ================
                (_, B::Mul | B::FixMul, Some(C::Int(0))) => TapIrInstr::Constant(t, C::Int(0)),
                (Some(C::Int(0)), B::Mul | B::FixMul, _) => TapIrInstr::Constant(t, C::Int(0)),

                (_, B::Mul | B::FixMul, Some(C::Fix(n))) if n == f0 => {
                    TapIrInstr::Constant(t, C::Fix(f0))
                }
                (Some(C::Fix(n)), B::Mul | B::FixMul, _) if n == f0 => {
                    TapIrInstr::Constant(t, C::Fix(f0))
                }

                (Some(C::Int(0)), B::Div | B::RealDiv | B::FixDiv, _) => {
                    TapIrInstr::Constant(t, C::Int(0))
                }
                (Some(C::Fix(n)), B::Div | B::RealDiv | B::FixDiv, _) if n == f0 => {
                    TapIrInstr::Constant(t, C::Fix(f0))
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

                    instr.instr = TapIrInstr::BinOp {
                        target: t,
                        lhs: *lhs,
                        op: if op == B::FixMul { B::Mul } else { B::Div },
                        rhs: temp,
                    };

                    instrs.insert(
                        index - 1,
                        TapIr {
                            instr: TapIrInstr::Constant(temp, C::Int(n.floor())),
                        },
                    );

                    did_something = OptimisationResult::DidSomething;
                    continue;
                }
                (Some(C::Fix(n)), op @ (B::FixMul | B::FixDiv), _) if n.frac() == 0 => {
                    let temp = symtab.new_temporary();

                    instr.instr = TapIrInstr::BinOp {
                        target: t,
                        lhs: temp,
                        op: if op == B::FixMul { B::Mul } else { B::Div },
                        rhs: *rhs,
                    };

                    instrs.insert(
                        index - 1,
                        TapIr {
                            instr: TapIrInstr::Constant(temp, C::Int(n.floor())),
                        },
                    );

                    did_something = OptimisationResult::DidSomething;
                    continue;
                }

                _ => continue,
            };

            did_something = OptimisationResult::DidSomething;
            instr.instr = replacement;
        }
    }

    did_something
}

fn int_op(i1: i32, op: BinaryOperator, i2: i32) -> Constant {
    use Constant as C;

    match op {
        BinaryOperator::Add => C::Int(i1 + i2),
        BinaryOperator::Sub => C::Int(i1 - i2),
        BinaryOperator::Mul => C::Int(i1 * i2),
        BinaryOperator::Div => C::Int(i1 / i2), // FIXME: div_floor
        BinaryOperator::Mod => C::Int(i1.rem_euclid(i2)),
        BinaryOperator::RealDiv => C::Int(i1 / i2),
        BinaryOperator::RealMod => C::Int(i1 % i2),
        BinaryOperator::FixMul => panic!("Should never be fixmuling 2 integers"),
        BinaryOperator::FixDiv => panic!("Should never be fixdivving 2 integers"),
        BinaryOperator::EqEq => C::Bool(i1 == i2),
        BinaryOperator::NeEq => C::Bool(i1 != i2),
        BinaryOperator::Gt => C::Bool(i1 > i2),
        BinaryOperator::GtEq => C::Bool(i1 >= i2),
        BinaryOperator::Lt => C::Bool(i1 < i2),
        BinaryOperator::LtEq => C::Bool(i1 <= i2),
        BinaryOperator::Then => C::Int(i2),
    }
}

fn fix_op(n1: Num<i32, 8>, op: BinaryOperator, n2: Num<i32, 8>) -> Constant {
    use Constant as C;

    match op {
        BinaryOperator::Add => C::Fix(n1 + n2),
        BinaryOperator::Sub => C::Fix(n1 - n2),
        BinaryOperator::Mul => panic!("Should never mulitply fixnums"),
        BinaryOperator::Div => panic!("Should never be div for fixnums"),
        BinaryOperator::Mod => panic!("Should never mod fixnums"),
        BinaryOperator::RealDiv => panic!("Should never realdiv fixnums"),
        BinaryOperator::RealMod => panic!("Should never realmod fixnums"),
        BinaryOperator::FixMul => C::Fix(n1 * n2),
        BinaryOperator::FixDiv => C::Fix(n1 / n2),
        BinaryOperator::EqEq => C::Bool(n1 == n2),
        BinaryOperator::NeEq => C::Bool(n1 != n2),
        BinaryOperator::Gt => C::Bool(n1 > n2),
        BinaryOperator::GtEq => C::Bool(n1 >= n2),
        BinaryOperator::Lt => C::Bool(n1 < n2),
        BinaryOperator::LtEq => C::Bool(n1 <= n2),
        BinaryOperator::Then => C::Fix(n2),
    }
}

fn fix_int_op(n1: Num<i32, 8>, op: BinaryOperator, i2: i32) -> Constant {
    use Constant as C;

    match op {
        BinaryOperator::Add => C::Fix(n1 + i2),
        BinaryOperator::Sub => C::Fix(n1 - i2),
        BinaryOperator::Mul => C::Fix(n1 * i2),
        BinaryOperator::Div => C::Fix(n1 / i2),
        BinaryOperator::Mod => C::Fix(n1.rem_euclid(i2.into())),
        BinaryOperator::RealDiv => C::Fix(n1 / i2),
        BinaryOperator::RealMod => C::Fix(n1 % i2),
        BinaryOperator::Then => C::Int(i2),
        _ => panic!("Invalid operation {op} on fix int"),
    }
}
