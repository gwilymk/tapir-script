use crate::{
    ast::{self, BinaryOperator, Expression, InternalOrExternalFunctionId, SymbolId},
    compile::symtab_visitor::SymTab,
};

use super::{
    BlockExitInstr, BlockId, Constant, FunctionModifiers, TapIr, TapIrBlock, TapIrFunction,
    TapIrInstr,
};

pub fn create_ir(f: &ast::Function<'_>, symtab: &mut SymTab) -> TapIrFunction {
    let block_visitor = BlockVisitor::default();
    let blocks = block_visitor.create_blocks(&f.statements, symtab);

    let id = *f.meta.get().expect("Should have FunctionId by now");
    let modifiers = FunctionModifiers::new(f, symtab);

    let arguments = f
        .arguments
        .iter()
        .map(|a| a.name.symbol_id().expect("Should have resolved arguments"))
        .collect();

    let return_types = f.return_types.types.iter().map(|t| t.t).collect();

    TapIrFunction::new(
        id,
        blocks.into_boxed_slice(),
        modifiers,
        arguments,
        return_types,
    )
}

#[derive(Default)]
struct BlockVisitor {
    blocks: Vec<TapIrBlock>,
    current_block: Vec<TapIr>,

    loop_entries: Vec<LoopEntry>,
    next_free_block_id: usize,
    next_block_id: Option<BlockId>,
}

struct LoopEntry {
    entry: BlockId,
    exit: BlockId,
}

impl BlockVisitor {
    fn create_blocks(
        mut self,
        statements: &[ast::Statement<'_>],
        symtab: &mut SymTab,
    ) -> Vec<TapIrBlock> {
        for statement in statements {
            self.visit_statement(statement, symtab);
        }

        if !self.current_block.is_empty() || self.next_block_id.is_some() {
            self.finalize_block(BlockExitInstr::Return(Box::new([])), None);
        }

        if self.blocks.is_empty() {
            self.finalize_block(BlockExitInstr::Return(Box::new([])), None);
        }

        self.blocks
    }

    fn visit_statement(&mut self, statement: &ast::Statement<'_>, symtab: &mut SymTab) {
        match &statement.kind {
            ast::StatementKind::Error => {
                unreachable!("Shouldn't be creating IR if there is an error")
            }
            ast::StatementKind::Assignment { values, .. }
            | ast::StatementKind::VariableDeclaration { values, .. } => {
                let target_symbols: &Vec<SymbolId> =
                    statement.meta.get().expect("Should've resolved symbols");

                let temps: Vec<SymbolId> = if target_symbols.len() == values.len() {
                    // Paired assignment: evaluate all RHS into temporaries first,
                    // then move to targets (enables swap idiom: a, b = b, a)
                    values
                        .iter()
                        .map(|value| {
                            let temp = symtab.new_temporary();
                            self.blocks_for_expression(value, temp, symtab);
                            temp
                        })
                        .collect()
                } else if values.len() == 1 {
                    // this must be a function call
                    let ast::ExpressionKind::Call { arguments, .. } = &values[0].kind else {
                        panic!(
                            "Type checker should've caught this, got mismatching symbols and no function call"
                        );
                    };

                    let args = arguments
                        .iter()
                        .map(|a| {
                            let symbol = symtab.new_temporary();
                            self.blocks_for_expression(a, symbol, symtab);
                            symbol
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice();

                    let f: InternalOrExternalFunctionId = *values[0]
                        .meta
                        .get()
                        .expect("Should have function IDs by now");

                    let temps = (0..target_symbols.len())
                        .map(|_| symtab.new_temporary())
                        .collect::<Vec<_>>();

                    match f {
                        InternalOrExternalFunctionId::Internal(f) => {
                            self.current_block.push(TapIr {
                                instr: TapIrInstr::Call {
                                    target: temps.clone().into_boxed_slice(),
                                    f,
                                    args,
                                },
                            });
                        }
                        InternalOrExternalFunctionId::External(f) => {
                            self.current_block.push(TapIr {
                                instr: TapIrInstr::CallExternal {
                                    target: temps.clone().into_boxed_slice(),
                                    f,
                                    args,
                                },
                            });
                        }
                    }

                    temps
                } else {
                    panic!("Type checker should've caught the count mismatch");
                };

                for (&target_symbol, temp) in target_symbols.iter().zip(temps) {
                    let expr_target = if symtab.get_property(target_symbol).is_some() {
                        symtab.new_temporary()
                    } else {
                        target_symbol
                    };

                    self.current_block.push(TapIr {
                        instr: TapIrInstr::Move {
                            target: expr_target,
                            source: temp,
                        },
                    });

                    if let Some(property) = symtab.get_property(target_symbol) {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::StoreProp {
                                prop_index: property.index,
                                value: expr_target,
                            },
                        });
                    }
                }
            }
            ast::StatementKind::Wait => {
                self.current_block.push(TapIr {
                    instr: TapIrInstr::Wait,
                });
            }
            ast::StatementKind::Block { block } => {
                for statement in block {
                    self.visit_statement(statement, symtab);
                }
            }
            ast::StatementKind::Continue => {
                let loop_entry = self
                    .loop_entries
                    .last()
                    .expect("Continue should be in loop by this point");
                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry.entry), None);
            }
            ast::StatementKind::Break => {
                let loop_entry = self
                    .loop_entries
                    .last()
                    .expect("Continue should be in loop by this point");
                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry.exit), None);
            }
            ast::StatementKind::Nop => {}
            ast::StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                let condition_target = symtab.new_temporary();
                self.blocks_for_expression(condition, condition_target, symtab);

                let true_block_id = self.next_block_id();
                let false_block_id = self.next_block_id();
                let continue_block_id = self.next_block_id();

                self.finalize_block(
                    BlockExitInstr::ConditionalJump {
                        test: condition_target,
                        if_true: true_block_id,
                        if_false: false_block_id,
                    },
                    Some(true_block_id),
                );

                for statement in true_block {
                    self.visit_statement(statement, symtab);
                }
                self.finalize_block(
                    BlockExitInstr::JumpToBlock(continue_block_id),
                    Some(false_block_id),
                );

                for statement in false_block {
                    self.visit_statement(statement, symtab);
                }
                self.finalize_block(
                    BlockExitInstr::JumpToBlock(continue_block_id),
                    Some(continue_block_id),
                );
            }
            ast::StatementKind::Loop { block } => {
                let loop_entry = self.next_block_id();
                let loop_exit = self.next_block_id();

                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry), Some(loop_entry));

                self.loop_entries.push(LoopEntry {
                    entry: loop_entry,
                    exit: loop_exit,
                });

                for statement in block {
                    self.visit_statement(statement, symtab);
                }

                self.loop_entries.pop();

                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry), Some(loop_exit));
            }
            ast::StatementKind::Call { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                let f: InternalOrExternalFunctionId = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");

                match f {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::Call {
                                target: Box::new([]),
                                f: function_id,
                                args,
                            },
                        });
                    }
                    InternalOrExternalFunctionId::External(external_function_id) => {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::CallExternal {
                                target: Box::new([]),
                                f: external_function_id,
                                args,
                            },
                        });
                    }
                }
            }
            ast::StatementKind::Spawn { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                let f: InternalOrExternalFunctionId = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");

                match f {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::Spawn {
                                f: function_id,
                                args,
                            },
                        });
                    }
                    InternalOrExternalFunctionId::External(_) => {
                        panic!("Shouldn't be able to spawn an external function")
                    }
                }
            }
            ast::StatementKind::Trigger { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                let f = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");
                self.current_block.push(TapIr {
                    instr: TapIrInstr::Trigger { f, args },
                });
            }
            ast::StatementKind::Return { values } => {
                let return_values = values
                    .iter()
                    .map(|v| {
                        let return_symbol = symtab.new_temporary();
                        self.blocks_for_expression(v, return_symbol, symtab);
                        return_symbol
                    })
                    .collect();

                self.finalize_block(BlockExitInstr::Return(return_values), None);
            }
        }
    }

    fn next_block_id(&mut self) -> BlockId {
        self.next_free_block_id += 1;
        BlockId::from_raw(self.next_free_block_id - 1)
    }

    fn finalize_block(&mut self, block_exit: BlockExitInstr, next_block_id: Option<BlockId>) {
        let id = self
            .next_block_id
            .take()
            .unwrap_or_else(|| self.next_block_id());

        self.blocks.push(TapIrBlock::new(
            id,
            std::mem::take(&mut self.current_block),
            block_exit,
        ));

        self.next_block_id = next_block_id;
    }

    /// Sets the value of the expression to the symbol at target_symbol
    fn blocks_for_expression(
        &mut self,
        expr: &Expression<'_>,
        target_symbol: SymbolId,
        symtab: &mut SymTab,
    ) {
        match &expr.kind {
            ast::ExpressionKind::Integer(i) => self.current_block.push(TapIr {
                instr: TapIrInstr::Constant(target_symbol, Constant::Int(*i)),
            }),
            ast::ExpressionKind::Fix(num) => self.current_block.push(TapIr {
                instr: TapIrInstr::Constant(target_symbol, Constant::Fix(*num)),
            }),
            ast::ExpressionKind::Bool(b) => self.current_block.push(TapIr {
                instr: TapIrInstr::Constant(target_symbol, Constant::Bool(*b)),
            }),
            ast::ExpressionKind::Variable(_) => {
                let source = *expr.meta.get().expect("Should've resolved variable");
                if let Some(property) = symtab.get_property(source) {
                    self.current_block.push(TapIr {
                        instr: TapIrInstr::GetProp {
                            target: target_symbol,
                            prop_index: property.index,
                        },
                    });
                } else {
                    self.current_block.push(TapIr {
                        instr: TapIrInstr::Move {
                            target: target_symbol,
                            source,
                        },
                    });
                }
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_target = symtab.new_temporary();

                match operator {
                    BinaryOperator::Then => {
                        self.blocks_for_expression(lhs, lhs_target, symtab);
                        self.blocks_for_expression(rhs, target_symbol, symtab);
                    }
                    BinaryOperator::And | BinaryOperator::Or => {
                        // first calculate the value of the lhs
                        let is_or = *operator == BinaryOperator::Or;

                        self.blocks_for_expression(lhs, lhs_target, symtab);

                        let rhs_block_id = self.next_block_id();
                        let continue_block_id = self.next_block_id();

                        // if it's &&, then we should start by assigning false to the target_symbol and jumping only
                        // if the initial lhs is true. For || it should be the other way around
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::Constant(target_symbol, Constant::Bool(is_or)),
                        });

                        self.finalize_block(
                            BlockExitInstr::ConditionalJump {
                                test: lhs_target,
                                if_true: if is_or {
                                    continue_block_id
                                } else {
                                    rhs_block_id
                                },
                                if_false: if !is_or {
                                    continue_block_id
                                } else {
                                    rhs_block_id
                                },
                            },
                            Some(rhs_block_id),
                        );

                        self.blocks_for_expression(rhs, target_symbol, symtab);
                        self.finalize_block(
                            BlockExitInstr::JumpToBlock(continue_block_id),
                            Some(continue_block_id),
                        );
                    }
                    operator => {
                        let rhs_target = symtab.new_temporary();

                        self.blocks_for_expression(lhs, lhs_target, symtab);
                        self.blocks_for_expression(rhs, rhs_target, symtab);

                        self.current_block.push(TapIr {
                            instr: TapIrInstr::BinOp {
                                target: target_symbol,
                                lhs: lhs_target,
                                rhs: rhs_target,
                                op: *operator,
                            },
                        });
                    }
                }
            }
            ast::ExpressionKind::Error => {
                unreachable!("Shouldn't be creating IR if there is an error");
            }
            ast::ExpressionKind::Nop => {}
            ast::ExpressionKind::Call { arguments, .. } => {
                let function_id: InternalOrExternalFunctionId = *expr
                    .meta
                    .get()
                    .expect("Should've assigned function IDs by now");

                let args = arguments
                    .iter()
                    .map(|arg| {
                        let arg_sym = symtab.new_temporary();
                        self.blocks_for_expression(arg, arg_sym, symtab);
                        arg_sym
                    })
                    .collect();

                match function_id {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::Call {
                                target: Box::new([target_symbol]),
                                f: function_id,
                                args,
                            },
                        });
                    }
                    InternalOrExternalFunctionId::External(external_function_id) => {
                        self.current_block.push(TapIr {
                            instr: TapIrInstr::CallExternal {
                                target: Box::new([target_symbol]),
                                f: external_function_id,
                                args,
                            },
                        });
                    }
                }
            }
        }
    }
}
