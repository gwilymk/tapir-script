use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter, slice,
};

use agb_fixnum::Num;
use petgraph::visit::{Bfs, Dfs, DfsPostOrder};

mod petgraph_trait_impls;
#[cfg(test)]
mod pretty_print;
pub mod regalloc;
mod ssa;

pub mod optimisations;

pub use ssa::make_ssa;

use crate::{
    EventHandlerArgument, Type,
    ast::{self, BinaryOperator, Expression, FunctionId, SymbolId},
    compile::{symtab_visitor::SymTab, type_visitor::TriggerId},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TapIr {
    pub instr: TapIrInstr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl TapIr {
    pub fn targets(&self) -> impl Iterator<Item = SymbolId> {
        SymbolIter::new_target(&self.instr)
    }

    pub fn sources(&self) -> impl Iterator<Item = SymbolId> {
        SymbolIter::new_source(&self.instr)
    }

    pub fn targets_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_target(&mut self.instr)
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_source(&mut self.instr)
    }

    /// Returns whether this could be removed if `targets()` are all unused. So technically
    /// there could be a side effect, but the side effect would only be changing the value
    /// of one of the symbols in `targets()`.
    pub fn could_have_side_effects(&self) -> bool {
        match &self.instr {
            TapIrInstr::Constant(..)
            | TapIrInstr::Move { .. }
            | TapIrInstr::BinOp { .. }
            | TapIrInstr::GetProp { .. } => false,
            TapIrInstr::Wait
            | TapIrInstr::Call { .. }
            | TapIrInstr::Spawn { .. }
            | TapIrInstr::Trigger { .. }
            | TapIrInstr::StoreProp { .. } => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl BlockExitInstr {
    pub fn sources(&self) -> impl Iterator<Item = SymbolId> {
        SymbolIter::new_source_exit(self)
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_source_exit(self)
    }

    fn target_blocks(&self) -> impl Iterator<Item = BlockId> {
        match self {
            BlockExitInstr::JumpToBlock(block_id) => [Some(*block_id), None],
            BlockExitInstr::ConditionalJump {
                if_true, if_false, ..
            } => [Some(*if_true), Some(*if_false)],
            BlockExitInstr::Return(_) => [None, None],
        }
        .into_iter()
        .flatten()
    }
}

/// Blocks have ids which aren't necessarily strictly increasing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(usize);

pub struct TapIrBlock {
    id: BlockId,
    instrs: Vec<TapIr>,

    block_entry: Vec<Phi>,
    block_exit: BlockExitInstr,
}

pub struct Phi {
    target: SymbolId,
    sources: Vec<(BlockId, SymbolId)>,
}

pub struct TapIrFunction {
    id: FunctionId,

    blocks: HashMap<BlockId, TapIrBlock>,
    root: BlockId,

    modifiers: FunctionModifiers,
    arguments: Box<[SymbolId]>,
    return_types: Box<[Type]>,
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
            block_entry: vec![],
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

    pub(crate) fn id(&self) -> BlockId {
        self.id
    }

    pub(crate) fn instrs(&self) -> &[TapIr] {
        &self.instrs
    }

    pub(crate) fn instrs_mut(&mut self) -> &mut [TapIr] {
        &mut self.instrs
    }

    pub(crate) fn block_exit(&self) -> &BlockExitInstr {
        &self.block_exit
    }

    pub(crate) fn block_exit_mut(&mut self) -> &mut BlockExitInstr {
        &mut self.block_exit
    }

    fn block_entry(&self) -> &[Phi] {
        &self.block_entry
    }

    fn sources_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        self.block_entry
            .iter_mut()
            .flat_map(|phi| phi.sources.iter_mut().map(|(_, symbol_id)| symbol_id))
            .chain(self.instrs.iter_mut().flat_map(|instr| instr.sources_mut()))
            .chain(SymbolIterMut::new_source_exit(&mut self.block_exit))
    }

    fn targets_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        self.block_entry
            .iter_mut()
            .map(|phi| &mut phi.target)
            .chain(self.instrs.iter_mut().flat_map(|instr| instr.targets_mut()))
    }

    fn sources(&self) -> impl Iterator<Item = SymbolId> {
        self.block_entry
            .iter()
            .flat_map(|phi| phi.sources.iter().map(|(_, symbol_id)| *symbol_id))
            .chain(self.instrs.iter().flat_map(|instr| instr.sources()))
            .chain(SymbolIter::new_source_exit(&self.block_exit))
    }

    /// Iterate through each entry in the block_entry and keeps only the ones where `f` returns true.
    ///
    /// Gives an opportunity to change any of them as you go.
    fn block_entry_retain_mut(&mut self, f: impl FnMut(&mut Phi) -> bool) {
        self.block_entry.retain_mut(f);
    }

    fn remove_block_entries(&mut self) {
        self.block_entry.clear();
    }

    fn block_entry_mut(&mut self) -> &mut [Phi] {
        &mut self.block_entry
    }

    fn instrs_retain(&mut self, f: impl FnMut(&TapIr) -> bool) {
        self.instrs.retain(f)
    }
}

impl TapIrFunction {
    pub fn new(
        id: FunctionId,
        blocks: Box<[TapIrBlock]>,
        modifiers: FunctionModifiers,
        arguments: Box<[SymbolId]>,
        return_types: Box<[Type]>,
    ) -> Self {
        let root = blocks[0].id;

        let blocks = blocks.into_iter().map(|block| (block.id, block)).collect();

        Self {
            id,
            blocks,
            root,
            modifiers,
            arguments,
            return_types,
        }
    }

    pub fn symbols(&self) -> HashSet<SymbolId> {
        let mut symbols = HashSet::new();
        symbols.extend(&self.arguments);

        for block in self.blocks.values() {
            block.symbols(&mut symbols);
        }

        symbols
    }

    pub fn blocks(&self) -> impl Iterator<Item = &TapIrBlock> {
        let mut bfs = Bfs::new(self, self.root);

        iter::from_fn(move || {
            bfs.next(self)
                .map(|block_id| self.blocks.get(&block_id).expect("Invalid block reference"))
        })
    }

    pub fn block_mut(&mut self, block_id: BlockId) -> Option<&mut TapIrBlock> {
        self.blocks.get_mut(&block_id)
    }

    pub(crate) fn return_types(&self) -> &[Type] {
        &self.return_types
    }

    pub(crate) fn modifiers(&self) -> &FunctionModifiers {
        &self.modifiers
    }

    pub(crate) fn arguments(&self) -> &[SymbolId] {
        &self.arguments
    }

    pub(crate) fn id(&self) -> FunctionId {
        self.id
    }

    fn block(&self, block_id: BlockId) -> Option<&TapIrBlock> {
        self.blocks.get(&block_id)
    }
}

pub enum TapIrFunctionBlockIter {
    Dfs(Dfs<BlockId, HashSet<BlockId>>),
    PostOrder(DfsPostOrder<BlockId, HashSet<BlockId>>),
    ReversePostOrder(std::vec::IntoIter<BlockId>),
}

impl TapIrFunctionBlockIter {
    pub fn new_dfs(f: &TapIrFunction) -> Self {
        Self::Dfs(Dfs::new(f, f.root))
    }

    pub fn new_post_order(f: &TapIrFunction) -> Self {
        Self::PostOrder(DfsPostOrder::new(f, f.root))
    }

    pub fn new_reverse_post_order(f: &TapIrFunction) -> Self {
        let mut post_order = DfsPostOrder::new(f, f.root);
        let mut post_order_list = Vec::with_capacity(f.blocks.len());

        while let Some(next) = post_order.next(f) {
            post_order_list.push(next);
        }

        post_order_list.reverse();
        Self::ReversePostOrder(post_order_list.into_iter())
    }

    pub fn next<'a>(&mut self, f: &'a TapIrFunction) -> Option<&'a TapIrBlock> {
        self.next_id(f)
            .map(|block_id| f.block(block_id).expect("Should find the block"))
    }

    pub fn next_mut<'a>(&mut self, f: &'a mut TapIrFunction) -> Option<&'a mut TapIrBlock> {
        self.next_id(f)
            .map(move |block_id| f.block_mut(block_id).expect("Should find the block"))
    }

    pub fn next_id(&mut self, f: &TapIrFunction) -> Option<BlockId> {
        match self {
            TapIrFunctionBlockIter::Dfs(dfs) => dfs.next(f),
            TapIrFunctionBlockIter::PostOrder(dfs_post_order) => dfs_post_order.next(f),
            TapIrFunctionBlockIter::ReversePostOrder(iter) => iter.next(),
        }
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

enum SymbolIter<'a> {
    None,
    One(Option<SymbolId>),
    Two(Option<SymbolId>, Option<SymbolId>),
    Many(slice::Iter<'a, SymbolId>),
}

impl<'a> SymbolIter<'a> {
    pub fn new_source(instr: &'a TapIrInstr) -> Self {
        match instr {
            TapIrInstr::Move { source, .. } | TapIrInstr::StoreProp { value: source, .. } => {
                Self::One(Some(*source))
            }
            TapIrInstr::BinOp { lhs, rhs, .. } => Self::Two(Some(*lhs), Some(*rhs)),
            TapIrInstr::Call { args, .. }
            | TapIrInstr::Trigger { args, .. }
            | TapIrInstr::Spawn { args, .. } => Self::Many(args.iter()),
            _ => Self::None,
        }
    }

    pub fn new_target(instr: &'a TapIrInstr) -> Self {
        match instr {
            TapIrInstr::Constant(target, _)
            | TapIrInstr::Move { target, .. }
            | TapIrInstr::GetProp { target, .. }
            | TapIrInstr::BinOp { target, .. } => Self::One(Some(*target)),
            TapIrInstr::Call { target, .. } => Self::Many(target.iter()),
            _ => Self::None,
        }
    }

    pub fn new_source_exit(instr: &'a BlockExitInstr) -> Self {
        match instr {
            BlockExitInstr::JumpToBlock(_) => Self::None,
            BlockExitInstr::ConditionalJump { test, .. } => Self::One(Some(*test)),
            BlockExitInstr::Return(symbol_ids) => Self::Many(symbol_ids.iter()),
        }
    }
}

impl<'a> Iterator for SymbolIter<'a> {
    type Item = SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SymbolIter::None => None,
            SymbolIter::One(symbol_id) => symbol_id.take(),
            SymbolIter::Two(symbol_id1, symbol_id2) => {
                symbol_id1.take().or_else(|| symbol_id2.take())
            }
            SymbolIter::Many(iter) => iter.next().copied(),
        }
    }
}

enum SymbolIterMut<'a> {
    None,
    One(Option<&'a mut SymbolId>),
    Two(Option<&'a mut SymbolId>, Option<&'a mut SymbolId>),
    Many(slice::IterMut<'a, SymbolId>),
}

impl<'a> SymbolIterMut<'a> {
    pub fn new_source(instr: &'a mut TapIrInstr) -> Self {
        match instr {
            TapIrInstr::Move { source, .. } | TapIrInstr::StoreProp { value: source, .. } => {
                Self::One(Some(source))
            }
            TapIrInstr::BinOp { lhs, rhs, .. } => Self::Two(Some(lhs), Some(rhs)),
            TapIrInstr::Call { args, .. }
            | TapIrInstr::Trigger { args, .. }
            | TapIrInstr::Spawn { args, .. } => Self::Many(args.iter_mut()),
            _ => Self::None,
        }
    }

    pub fn new_target(instr: &'a mut TapIrInstr) -> Self {
        match instr {
            TapIrInstr::Constant(target, _)
            | TapIrInstr::Move { target, .. }
            | TapIrInstr::GetProp { target, .. }
            | TapIrInstr::BinOp { target, .. } => Self::One(Some(target)),
            TapIrInstr::Call { target, .. } => Self::Many(target.iter_mut()),
            _ => Self::None,
        }
    }

    pub fn new_source_exit(instr: &'a mut BlockExitInstr) -> Self {
        match instr {
            BlockExitInstr::JumpToBlock(_) => Self::None,
            BlockExitInstr::ConditionalJump { test, .. } => Self::One(Some(test)),
            BlockExitInstr::Return(symbol_ids) => Self::Many(symbol_ids.iter_mut()),
        }
    }
}

impl<'a> Iterator for SymbolIterMut<'a> {
    type Item = &'a mut SymbolId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SymbolIterMut::None => None,
            SymbolIterMut::One(symbol_id) => symbol_id.take(),
            SymbolIterMut::Two(symbol_id1, symbol_id2) => {
                symbol_id1.take().or_else(|| symbol_id2.take())
            }
            SymbolIterMut::Many(many) => many.next(),
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_snapshot, glob};

    use crate::{
        CompileSettings,
        compile::{
            loop_visitor::visit_loop_check, symtab_visitor::SymTabVisitor,
            type_visitor::TypeVisitor,
        },
        grammar,
        lexer::Lexer,
        reporting::Diagnostics,
        tokens::FileId,
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

    #[test]
    fn test_targets_iter_wait() {
        let instr = TapIr {
            instr: TapIrInstr::Wait,
        };
        assert_eq!(instr.targets().next(), None);
    }

    #[test]
    fn test_targets_iter_constant() {
        let instr = TapIr {
            instr: TapIrInstr::Constant(SymbolId(5), Constant::Bool(true)),
        };

        assert_eq!(instr.targets().collect::<Vec<_>>(), vec![SymbolId(5)]);
    }

    #[test]
    fn test_targets_iter_move() {
        let instr = TapIr {
            instr: TapIrInstr::Move {
                target: SymbolId(5),
                source: SymbolId(6),
            },
        };

        assert_eq!(instr.targets().collect::<Vec<_>>(), vec![SymbolId(5)]);
    }

    #[test]
    fn test_targets_call() {
        let instr = TapIr {
            instr: TapIrInstr::Call {
                target: Box::new([SymbolId(6), SymbolId(8)]),
                f: FunctionId(3),
                args: Box::new([SymbolId(1), SymbolId(2)]),
            },
        };

        assert_eq!(
            instr.targets().collect::<Vec<_>>(),
            vec![SymbolId(6), SymbolId(8)]
        );
    }

    #[test]
    fn test_sources_move() {
        let mut instr = TapIr {
            instr: TapIrInstr::Move {
                target: SymbolId(5),
                source: SymbolId(55),
            },
        };

        for source in instr.sources_mut() {
            source.0 += 5;
        }

        assert_eq!(
            instr.instr,
            TapIrInstr::Move {
                target: SymbolId(5),
                source: SymbolId(60)
            }
        );
    }
}
