// Implemented as described by https://c9x.me/compile/bib/braun13cc.pdf

use petgraph::{Direction, prelude::DiGraphMap, visit::IntoNeighbors};

use super::*;

pub fn make_ssa(function: &mut TapIrFunction, symtab: &mut SymTab, symbol_spans: &mut SymbolSpans) {
    let mut converter = SsaConverter::new(function);

    let mut reverse_post_order = TapIrFunctionBlockIter::new_reverse_post_order(function);

    while let Some(block) = reverse_post_order.next_mut(function) {
        let block_id = block.id();

        for instr in block.instrs_mut() {
            for source in instr.sources_mut() {
                let old_source = *source;
                let new_source =
                    converter.read_variable(old_source, block_id, symtab, symbol_spans);
                *source = new_source;
            }

            for target in instr.targets_mut() {
                let old_target = *target;
                let new_target = symtab.new_rename(old_target);
                symbol_spans.alias(new_target, old_target);
                converter.write_variable(old_target, block_id, new_target);
                *target = new_target;
            }
        }

        for source in block.block_exit_mut().sources_mut() {
            let old_source = *source;
            let new_source = converter.read_variable(old_source, block_id, symtab, symbol_spans);
            *source = new_source;
        }

        converter.mark_filled(block.id(), symtab, symbol_spans);
    }

    for (block_id, phis) in converter.into_phis() {
        function
            .block_mut(block_id)
            .expect("Failed to get block")
            .block_entry = phis;
    }
}

struct SsaConverter {
    current_def: HashMap<SymbolId, HashMap<BlockId, SymbolId>>,

    incomplete_phis: HashMap<BlockId, HashMap<SymbolId, SymbolId>>,
    phis: HashMap<BlockId, HashMap<SymbolId, Vec<(BlockId, SymbolId)>>>,

    graph: DiGraphMap<BlockId, ()>,

    filled_blocks: HashSet<BlockId>,
    sealed_blocks: HashSet<BlockId>,
}

impl SsaConverter {
    pub fn new(function: &TapIrFunction) -> Self {
        let mut full_graph = DiGraphMap::new();
        for block in function.blocks() {
            for neighbour in function.neighbors(block.id()) {
                full_graph.add_edge(block.id(), neighbour, ());
            }
        }

        // Put the function arguments as defined variables in the first block
        let mut current_def: HashMap<SymbolId, HashMap<BlockId, SymbolId>> = HashMap::new();

        for argument in function.arguments() {
            current_def
                .entry(*argument)
                .or_default()
                .insert(function.root, *argument);
        }

        Self {
            graph: full_graph,

            incomplete_phis: Default::default(),
            phis: Default::default(),

            current_def,

            filled_blocks: Default::default(),
            sealed_blocks: Default::default(),
        }
    }

    pub fn write_variable(&mut self, variable: SymbolId, block: BlockId, value: SymbolId) {
        self.current_def
            .entry(variable)
            .or_default()
            .insert(block, value);
    }

    pub fn read_variable(
        &mut self,
        variable: SymbolId,
        block: BlockId,
        symtab: &mut SymTab,
        symbol_spans: &mut SymbolSpans,
    ) -> SymbolId {
        if let Some(renamed) = self.current_def.get(&variable).and_then(|d| d.get(&block)) {
            return *renamed;
        }

        self.read_variable_recursive(variable, block, symtab, symbol_spans)
    }

    fn read_variable_recursive(
        &mut self,
        variable: SymbolId,
        block: BlockId,
        symtab: &mut SymTab,
        symbol_spans: &mut SymbolSpans,
    ) -> SymbolId {
        let val = if !self.sealed_blocks.contains(&block) {
            let val = symtab.new_rename(variable);
            symbol_spans.alias(val, variable);
            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(variable, val);
            val
        } else {
            let predecessors = self
                .graph
                .neighbors_directed(block, Direction::Incoming)
                .collect::<Vec<_>>();

            // Optimise for the common case of one predecessor: no phi needed
            if predecessors.len() == 1 {
                self.read_variable(variable, predecessors[0], symtab, symbol_spans)
            } else {
                // Break potential cycles by putting the phi in there already
                let phi_variable = symtab.new_rename(variable);
                symbol_spans.alias(phi_variable, variable);
                self.phis
                    .entry(block)
                    .or_default()
                    .insert(phi_variable, vec![]);

                self.write_variable(variable, block, phi_variable);

                self.add_phi_operands(
                    variable,
                    block,
                    phi_variable,
                    predecessors.into_iter(),
                    symtab,
                    symbol_spans,
                )
            }
        };

        self.write_variable(variable, block, val);

        val
    }

    fn add_phi_operands(
        &mut self,
        variable: SymbolId,
        block: BlockId,
        phi_variable: SymbolId,
        predecessors: impl Iterator<Item = BlockId>,
        symtab: &mut SymTab,
        symbol_spans: &mut SymbolSpans,
    ) -> SymbolId {
        for predecessor in predecessors {
            let value = self.read_variable(variable, predecessor, symtab, symbol_spans);

            self.phis
                .entry(block)
                .or_default()
                .entry(phi_variable)
                .or_default()
                .push((predecessor, value));
        }

        phi_variable
    }

    pub fn mark_filled(
        &mut self,
        block: BlockId,
        symtab: &mut SymTab,
        symbol_spans: &mut SymbolSpans,
    ) {
        self.filled_blocks.insert(block);
        self.maybe_seal_block(block, symtab, symbol_spans);

        let successors = self
            .graph
            .neighbors_directed(block, Direction::Outgoing)
            .collect::<Vec<_>>();

        // filling this block might've made some successor sealed
        for successor in successors {
            self.maybe_seal_block(successor, symtab, symbol_spans);
        }
    }

    fn maybe_seal_block(
        &mut self,
        block: BlockId,
        symtab: &mut SymTab,
        symbol_spans: &mut SymbolSpans,
    ) {
        let predecessors = self
            .graph
            .neighbors_directed(block, Direction::Incoming)
            .collect::<Vec<_>>();

        if predecessors
            .iter()
            .any(|predecessor| !self.filled_blocks.contains(predecessor))
        {
            return; // shouldn't mark this as sealed quite yet
        }

        for (variable, val) in self.incomplete_phis.remove(&block).unwrap_or_default() {
            self.add_phi_operands(
                variable,
                block,
                val,
                predecessors.iter().cloned(),
                symtab,
                symbol_spans,
            );
        }

        self.sealed_blocks.insert(block);
    }

    pub fn into_phis(self) -> impl Iterator<Item = (BlockId, Vec<Phi>)> {
        assert!(self.incomplete_phis.is_empty());
        assert_eq!(self.filled_blocks, self.sealed_blocks);

        self.phis.into_iter().map(|(block, phis)| {
            let mut phis = phis
                .into_iter()
                .map(|(target, sources)| Phi { target, sources })
                .collect::<Vec<_>>();

            // Sort to ensure stable compilation
            phis.sort_unstable_by_key(|phi| phi.target);

            (block, phis)
        })
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
    fn ssa_generation_tests() {
        glob!("snapshot_tests", "ssa/*.tapir", |path| {
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
                diagnostics.pretty_string(false)
            );

            let mut symtab = symtab_visitor.into_symtab();

            let (mut irs, spans_vec): (Vec<_>, Vec<_>) = script
                .functions
                .iter()
                .map(|f| create_ir(f, &mut symtab))
                .unzip();

            let mut symbol_spans = SymbolSpans::new();
            for spans in spans_vec {
                symbol_spans.extend(&spans);
            }

            for f in &mut irs {
                make_ssa(f, &mut symtab, &mut symbol_spans);
            }

            let mut output = String::new();

            for ir in irs {
                pretty_print::pretty_print_tapir_function(&ir, &symtab, &mut output).unwrap();
            }

            assert_snapshot!(output);
        });
    }
}
