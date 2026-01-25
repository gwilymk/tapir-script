use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter,
    ops::Add,
};

use agb_fixnum::Num;
use petgraph::visit::Bfs;

mod lowering;
mod petgraph_trait_impls;
#[cfg(test)]
mod pretty_print;
pub mod regalloc;
mod ssa;
mod symbol_iter;
mod traversal;

pub mod optimisations;

pub use lowering::create_ir;
pub use ssa::make_ssa;
pub use traversal::TapIrFunctionBlockIter;

use symbol_iter::{SymbolIter, SymbolIterMut};

use crate::{
    FunctionArgument, Type,
    ast::{self, BinaryOperator, ExternalFunctionId, FunctionId, SymbolId},
    builtins::BuiltinVariable,
    compile::{symtab_visitor::SymTab, type_visitor::TriggerId},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TapIr {
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
    CallExternal {
        target: Box<[SymbolId]>,
        f: ExternalFunctionId,
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
    GetBuiltin {
        target: SymbolId,
        builtin: BuiltinVariable,
    },
    StoreProp {
        prop_index: usize,
        value: SymbolId,
    },
    GetGlobal {
        target: SymbolId,
        global_index: usize,
    },
    SetGlobal {
        global_index: usize,
        value: SymbolId,
    },
}

impl TapIr {
    pub fn targets(&self) -> impl Iterator<Item = SymbolId> {
        SymbolIter::new_target(self)
    }

    pub fn sources(&self) -> impl Iterator<Item = SymbolId> {
        SymbolIter::new_source(self)
    }

    pub fn targets_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_target(self)
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_source(self)
    }

    /// Returns whether this could be removed if `targets()` are all unused. So technically
    /// there could be a side effect, but the side effect would only be changing the value
    /// of one of the symbols in `targets()`.
    pub fn could_have_side_effects(&self) -> bool {
        match self {
            TapIr::Constant(..)
            | TapIr::Move { .. }
            | TapIr::BinOp { .. }
            | TapIr::GetProp { .. }
            | TapIr::GetBuiltin { .. }
            | TapIr::GetGlobal { .. } => false,
            TapIr::Wait
            | TapIr::Call { .. }
            | TapIr::CallExternal { .. }
            | TapIr::Spawn { .. }
            | TapIr::Trigger { .. }
            | TapIr::StoreProp { .. }
            | TapIr::SetGlobal { .. } => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Int(i32),
    Fix(Num<i32, 8>),
    Bool(bool),
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
    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut SymbolId> {
        SymbolIterMut::new_source_exit(self)
    }
}

/// Blocks have ids which aren't necessarily strictly increasing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(usize);

impl BlockId {
    pub(super) fn from_raw(id: usize) -> Self {
        Self(id)
    }

    pub fn next(self) -> Self {
        self + 1
    }
}

impl Add<usize> for BlockId {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

#[derive(Clone)]
pub struct TapIrBlock {
    id: BlockId,
    instrs: Vec<TapIr>,

    block_entry: Vec<Phi>,
    block_exit: BlockExitInstr,
}

impl TapIrBlock {
    pub(super) fn new(id: BlockId, instrs: Vec<TapIr>, block_exit: BlockExitInstr) -> Self {
        Self {
            id,
            instrs,
            block_entry: vec![],
            block_exit,
        }
    }

    fn symbols(&self, symbols: &mut HashSet<SymbolId>) {
        for instr in &self.instrs {
            match instr {
                TapIr::Constant(symbol_id, ..)
                | TapIr::GetBuiltin {
                    target: symbol_id, ..
                }
                | TapIr::GetProp {
                    target: symbol_id, ..
                }
                | TapIr::StoreProp {
                    value: symbol_id, ..
                } => {
                    symbols.insert(*symbol_id);
                }
                TapIr::Move { target, source } => {
                    symbols.extend([*target, *source]);
                }
                TapIr::BinOp {
                    target, lhs, rhs, ..
                } => symbols.extend([*target, *lhs, *rhs]),
                TapIr::Wait => {}
                TapIr::Call { target, args, .. } | TapIr::CallExternal { target, args, .. } => {
                    symbols.extend(target);
                    symbols.extend(args);
                }
                TapIr::Trigger { args, .. } | TapIr::Spawn { args, .. } => {
                    symbols.extend(args);
                }
                TapIr::GetGlobal {
                    target: symbol_id, ..
                }
                | TapIr::SetGlobal {
                    value: symbol_id, ..
                } => {
                    symbols.insert(*symbol_id);
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

#[derive(Clone)]
pub struct Phi {
    target: SymbolId,
    sources: Vec<(BlockId, SymbolId)>,
}

pub struct TapIrFunction {
    id: FunctionId,

    blocks: HashMap<BlockId, TapIrBlock>,
    pub(super) root: BlockId,

    modifiers: FunctionModifiers,
    arguments: Box<[SymbolId]>,
    return_types: Box<[Type]>,
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

    pub fn next_block_id(&self) -> BlockId {
        BlockId(self.blocks.keys().max().unwrap().0).next()
    }

    /// An iterator over the `FunctionId`s that this function references
    pub fn callees(&self) -> impl Iterator<Item = FunctionId> {
        self.blocks().flat_map(|block| {
            block.instrs().iter().filter_map(|instr| match instr {
                TapIr::Call { f, .. } | TapIr::Spawn { f, .. } => Some(*f),
                _ => None,
            })
        })
    }

    /// An iterator of the `FunctionId`s that this function calls. So not those which are spawned
    pub fn direct_callees(&self) -> impl Iterator<Item = FunctionId> {
        self.blocks().flat_map(|block| {
            block.instrs().iter().filter_map(|instr| match instr {
                TapIr::Call { f, .. } => Some(*f),
                _ => None,
            })
        })
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

    pub(super) fn block(&self, block_id: BlockId) -> Option<&TapIrBlock> {
        self.blocks.get(&block_id)
    }

    fn insert_block(&mut self, block: TapIrBlock) {
        if self.blocks.insert(block.id(), block).is_some() {
            panic!("Shouldn't be inserting a block if one already exists with the same ID");
        }
    }

    fn disjoint_blocks_mut<const N: usize>(
        &mut self,
        block_ids: [&BlockId; N],
    ) -> [&mut TapIrBlock; N] {
        self.blocks
            .get_disjoint_mut(block_ids)
            .map(|block| block.expect("Failed to find block with ID"))
    }
}

pub struct FunctionModifiers {
    pub event_handler: Option<EventHandlerData>,
}

impl FunctionModifiers {
    pub fn new(f: &ast::Function<'_>, _symtab: &SymTab) -> Self {
        let event_handler = if f.modifiers.is_event_handler.is_some() {
            Some(EventHandlerData {
                name: f.name.to_string(),
                arg_names: f
                    .arguments
                    .iter()
                    .map(|a| FunctionArgument {
                        name: a.name().to_string(),
                        ty: a.ty_required().t,
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
    pub arg_names: Box<[FunctionArgument]>,
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
                available_fields: None,
                enable_optimisations: true,
            };

            let mut symtab_visitor =
                SymTabVisitor::new(&compile_settings, &mut script, &mut diagnostics);
            let mut type_visitor = TypeVisitor::new(
                &script.functions,
                &script.extern_functions,
                symtab_visitor.get_symtab(),
            );

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            assert!(
                !diagnostics.has_any(),
                "{}",
                diagnostics.pretty_string(true)
            );

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
        let instr = TapIr::Wait;
        assert_eq!(instr.targets().next(), None);
    }

    #[test]
    fn test_targets_iter_constant() {
        let instr = TapIr::Constant(SymbolId(5), Constant::Bool(true));

        assert_eq!(instr.targets().collect::<Vec<_>>(), vec![SymbolId(5)]);
    }

    #[test]
    fn test_targets_iter_move() {
        let instr = TapIr::Move {
            target: SymbolId(5),
            source: SymbolId(6),
        };

        assert_eq!(instr.targets().collect::<Vec<_>>(), vec![SymbolId(5)]);
    }

    #[test]
    fn test_targets_call() {
        let instr = TapIr::Call {
            target: Box::new([SymbolId(6), SymbolId(8)]),
            f: FunctionId(3),
            args: Box::new([SymbolId(1), SymbolId(2)]),
        };

        assert_eq!(
            instr.targets().collect::<Vec<_>>(),
            vec![SymbolId(6), SymbolId(8)]
        );
    }

    #[test]
    fn test_sources_move() {
        let mut instr = TapIr::Move {
            target: SymbolId(5),
            source: SymbolId(55),
        };

        for source in instr.sources_mut() {
            source.0 += 5;
        }

        assert_eq!(
            instr,
            TapIr::Move {
                target: SymbolId(5),
                source: SymbolId(60)
            }
        );
    }
}
