use std::{collections::HashSet, fmt::Display};

use agb_fixnum::Num;

#[cfg(test)]
mod pretty_print;
mod ssa;

use crate::{
    ast::{self, BinaryOperator, Expression, FunctionId, SymbolId},
    compile::{symtab_visitor::SymTab, type_visitor::TriggerId},
    EventHandlerArgument, Type,
};

pub struct TapIr {
    pub instr: TapIrInstr,
}

pub enum TapIrInstr {
    Constant(SymbolId, Constant),
    Move {
        target: SymbolId,
        source: SymbolId,
    },
    BinOp {
        target: SymbolId,
        lhs: SymbolId,
        op: BinaryOperator,
        rhs: SymbolId,
    },
    Wait,
    Call {
        target: Box<[SymbolId]>,
        f: FunctionId,
        args: Box<[SymbolId]>,
    },
    Spawn {
        f: FunctionId,
        args: Box<[SymbolId]>,
    },
    Trigger {
        f: TriggerId,
        args: Box<[SymbolId]>,
    },
    GetProp {
        target: SymbolId,
        prop_index: usize,
    },
    StoreProp {
        prop_index: usize,
        value: SymbolId,
    },
}

pub enum Constant {
    Int(i32),
    Fix(Num<i32, 8>),
    Bool(bool),
}

#[derive(Clone)]
pub enum BlockExitInstr {
    JumpToBlock(BlockId),
    ConditionalJump {
        test: SymbolId,
        if_true: BlockId,
        if_false: BlockId,
    },
    Return(Box<[SymbolId]>),
}

/// Blocks have ids which aren't necessarily strictly increasing.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(usize);

pub struct TapIrBlock {
    pub instrs: Vec<TapIr>,
    pub block_exit: BlockExitInstr,
    pub id: BlockId,
}

pub struct TapIrFunction {
    pub id: FunctionId,

    pub blocks: Vec<TapIrBlock>,

    pub modifiers: FunctionModifiers,
    pub arguments: Box<[SymbolId]>,
    pub return_types: Box<[Type]>,
}

pub struct FunctionModifiers {
    pub event_handler: Option<EventHandlerData>,
}

impl FunctionModifiers {
    pub fn new(f: &ast::Function<'_>, symtab: &SymTab) -> Self {
        let event_handler = if f.modifiers.is_event_handler.is_some() {
            Some(EventHandlerData {
                name: f.name.to_string(),
                arg_names: f
                    .arguments
                    .iter()
                    .map(|a| EventHandlerArgument {
                        name: symtab
                            .name_for_symbol(
                                a.name
                                    .symbol_id()
                                    .expect("Should've been resolved by the symbol visitor"),
                            )
                            .to_string(),
                        ty: a.t.t,
                    })
                    .collect(),
            })
        } else {
            None
        };

        Self { event_handler }
    }
}

pub struct EventHandlerData {
    pub name: String,
    pub arg_names: Box<[EventHandlerArgument]>,
}

pub fn create_ir(f: &ast::Function<'_>, symtab: &mut SymTab) -> TapIrFunction {
    let block_visitor = BlockVisitor::default();
    let blocks = block_visitor.create_blocks(&f.statements, symtab);

    TapIrFunction {
        id: *f.meta.get().expect("Should have FunctionId by now"),
        blocks,
        modifiers: FunctionModifiers::new(f, symtab),
        arguments: f
            .arguments
            .iter()
            .map(|a| a.name.symbol_id().expect("Should have resolved arguments"))
            .collect(),
        return_types: f.return_types.types.iter().map(|t| t.t).collect(),
    }
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

        self.blocks
    }

    fn visit_statement(&mut self, statement: &ast::Statement<'_>, symtab: &mut SymTab) {
        match &statement.kind {
            ast::StatementKind::Error => {
                unreachable!("Shouldn't be creating IR if there is an error")
            }
            ast::StatementKind::Assignment { value, .. }
            | ast::StatementKind::VariableDeclaration { value, .. } => {
                let target_symbol = *statement.meta.get().expect("Should've resolved symbols");

                let expr_target = if symtab.get_property(target_symbol).is_some() {
                    symtab.new_temporary()
                } else {
                    target_symbol
                };

                blocks_for_expression(value, expr_target, symtab, &mut self.current_block);

                if let Some(property) = symtab.get_property(target_symbol) {
                    self.current_block.push(TapIr {
                        instr: TapIrInstr::StoreProp {
                            prop_index: property.index,
                            value: expr_target,
                        },
                    });
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
                blocks_for_expression(condition, condition_target, symtab, &mut self.current_block);

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
                        blocks_for_expression(a, symbol, symtab, &mut self.current_block);
                        symbol
                    })
                    .collect();

                let f = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");
                self.current_block.push(TapIr {
                    instr: TapIrInstr::Call {
                        target: Box::new([]),
                        f,
                        args,
                    },
                });
            }
            ast::StatementKind::Spawn { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        blocks_for_expression(a, symbol, symtab, &mut self.current_block);
                        symbol
                    })
                    .collect();

                let f = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");
                self.current_block.push(TapIr {
                    instr: TapIrInstr::Spawn { f, args },
                });
            }
            ast::StatementKind::Trigger { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        blocks_for_expression(a, symbol, symtab, &mut self.current_block);
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
                        blocks_for_expression(v, return_symbol, symtab, &mut self.current_block);
                        return_symbol
                    })
                    .collect();

                self.finalize_block(BlockExitInstr::Return(return_values), None);
            }
        }
    }

    fn next_block_id(&mut self) -> BlockId {
        self.next_free_block_id += 1;
        BlockId(self.next_free_block_id - 1)
    }

    fn finalize_block(&mut self, block_exit: BlockExitInstr, next_block_id: Option<BlockId>) {
        let id = self
            .next_block_id
            .take()
            .unwrap_or_else(|| self.next_block_id());

        self.blocks.push(TapIrBlock {
            instrs: std::mem::take(&mut self.current_block),
            block_exit,
            id,
        });

        self.next_block_id = next_block_id;
    }
}

/// Sets the value of the expression to the symbol at target_symbol
fn blocks_for_expression(
    expr: &Expression<'_>,
    target_symbol: SymbolId,
    symtab: &mut SymTab,
    current_block: &mut Vec<TapIr>,
) {
    match &expr.kind {
        ast::ExpressionKind::Integer(i) => current_block.push(TapIr {
            instr: TapIrInstr::Constant(target_symbol, Constant::Int(*i)),
        }),
        ast::ExpressionKind::Fix(num) => current_block.push(TapIr {
            instr: TapIrInstr::Constant(target_symbol, Constant::Fix(*num)),
        }),
        ast::ExpressionKind::Bool(b) => current_block.push(TapIr {
            instr: TapIrInstr::Constant(target_symbol, Constant::Bool(*b)),
        }),
        ast::ExpressionKind::Variable(_) => {
            let source = *expr.meta.get().expect("Should've resolved variable");
            if let Some(property) = symtab.get_property(source) {
                current_block.push(TapIr {
                    instr: TapIrInstr::GetProp {
                        target: target_symbol,
                        prop_index: property.index,
                    },
                });
            } else {
                current_block.push(TapIr {
                    instr: TapIrInstr::Move {
                        target: target_symbol,
                        source,
                    },
                });
            }
        }
        ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
            let lhs_target = symtab.new_temporary();

            if !matches!(operator, BinaryOperator::Then) {
                let rhs_target = symtab.new_temporary();

                blocks_for_expression(lhs, lhs_target, symtab, current_block);
                blocks_for_expression(rhs, rhs_target, symtab, current_block);

                current_block.push(TapIr {
                    instr: TapIrInstr::BinOp {
                        target: target_symbol,
                        lhs: lhs_target,
                        rhs: rhs_target,
                        op: *operator,
                    },
                });
            } else {
                blocks_for_expression(lhs, lhs_target, symtab, current_block);
                blocks_for_expression(rhs, target_symbol, symtab, current_block);
            }
        }
        ast::ExpressionKind::Error => {
            unreachable!("Shouldn't be creating IR if there is an error");
        }
        ast::ExpressionKind::Nop => {}
        ast::ExpressionKind::Call { arguments, .. } => {
            let function_id = *expr
                .meta
                .get()
                .expect("Should've assigned function IDs by now");

            let args = arguments
                .iter()
                .map(|arg| {
                    let arg_sym = symtab.new_temporary();
                    blocks_for_expression(arg, arg_sym, symtab, current_block);
                    arg_sym
                })
                .collect();

            current_block.push(TapIr {
                instr: TapIrInstr::Call {
                    target: Box::new([target_symbol]),
                    f: function_id,
                    args,
                },
            });
        }
    }
}

impl TapIrBlock {
    fn symbols(&self, symbols: &mut HashSet<SymbolId>) {
        for instr in &self.instrs {
            match &instr.instr {
                TapIrInstr::Constant(symbol_id, ..)
                | TapIrInstr::GetProp {
                    target: symbol_id, ..
                }
                | TapIrInstr::StoreProp {
                    value: symbol_id, ..
                } => {
                    symbols.insert(*symbol_id);
                }
                TapIrInstr::Move { target, source } => {
                    symbols.extend([*target, *source]);
                }
                TapIrInstr::BinOp {
                    target, lhs, rhs, ..
                } => symbols.extend([*target, *lhs, *rhs]),
                TapIrInstr::Wait => {}
                TapIrInstr::Call { target, args, .. } => {
                    symbols.extend(target);
                    symbols.extend(args);
                }
                TapIrInstr::Trigger { args, .. } | TapIrInstr::Spawn { args, .. } => {
                    symbols.extend(args);
                }
            }
        }

        match &self.block_exit {
            BlockExitInstr::JumpToBlock(_) => {}
            BlockExitInstr::ConditionalJump { test, .. } => {
                symbols.insert(*test);
            }
            BlockExitInstr::Return(symbol_ids) => symbols.extend(symbol_ids),
        }
    }
}

impl TapIrFunction {
    pub fn symbols(&self) -> HashSet<SymbolId> {
        let mut symbols = HashSet::new();
        symbols.extend(&self.arguments);

        for block in &self.blocks {
            block.symbols(&mut symbols);
        }

        symbols
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(i) => write!(f, "int {i}"),
            Constant::Fix(num) => write!(f, "fix {num}"),
            Constant::Bool(b) => write!(f, "bool {b}"),
        }
    }
}

mod petgraph_trait_impls {
    use petgraph::visit::{GraphBase, IntoNeighbors, Visitable};

    use super::*;

    impl GraphBase for TapIrFunction {
        type EdgeId = ();
        type NodeId = BlockId;
    }

    impl IntoNeighbors for &TapIrFunction {
        type Neighbors = BlockNeighbours;

        fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
            let block = self
                .blocks
                .iter()
                .find(|b| b.id == a)
                .expect("Should be able to find block");

            BlockNeighbours(block.block_exit.clone(), 0)
        }
    }

    impl Visitable for TapIrFunction {
        type Map = HashSet<Self::NodeId>;

        fn visit_map(&self) -> Self::Map {
            Self::Map::with_capacity(self.blocks.len())
        }

        fn reset_map(&self, map: &mut Self::Map) {
            map.clear();
            map.reserve(self.blocks.len());
        }
    }

    pub struct BlockNeighbours(BlockExitInstr, usize);

    impl Iterator for BlockNeighbours {
        type Item = BlockId;

        fn next(&mut self) -> Option<Self::Item> {
            match &self.0 {
                BlockExitInstr::JumpToBlock(block_id) => {
                    if self.1 == 0 {
                        self.1 += 1;
                        Some(*block_id)
                    } else {
                        None
                    }
                }
                BlockExitInstr::ConditionalJump {
                    test: _,
                    if_true,
                    if_false,
                } => {
                    self.1 += 1;
                    match self.1 {
                        1 => Some(*if_true),
                        2 => Some(*if_false),
                        _ => None,
                    }
                }
                BlockExitInstr::Return(_) => None,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_snapshot, glob};

    use crate::{
        compile::{
            loop_visitor::visit_loop_check, symtab_visitor::SymTabVisitor,
            type_visitor::TypeVisitor,
        },
        grammar,
        lexer::Lexer,
        reporting::Diagnostics,
        tokens::FileId,
        CompileSettings,
    };

    use super::*;

    #[test]
    fn ir_generation_snapshot_tests() {
        glob!("snapshot_tests", "ir/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let compile_settings = CompileSettings {
                properties: Vec::new(),
                enable_optimisations: true,
            };

            let mut symtab_visitor =
                SymTabVisitor::new(&compile_settings, &mut script.functions, &mut diagnostics);
            let mut type_visitor = TypeVisitor::new(&compile_settings, &script.functions);

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            assert!(!diagnostics.has_any());

            let mut symtab = symtab_visitor.into_symtab();

            let irs = script
                .functions
                .iter()
                .map(|f| create_ir(f, &mut symtab))
                .collect::<Vec<_>>();

            let mut output = String::new();

            for ir in irs {
                pretty_print::pretty_print_tapir_function(&ir, &symtab, &mut output).unwrap();
            }

            assert_snapshot!(output);
        });
    }
}
