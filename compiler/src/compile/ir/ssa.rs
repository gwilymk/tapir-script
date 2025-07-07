// Implemented as described by https://c9x.me/compile/bib/braun13cc.pdf

use petgraph::{
    Direction,
    prelude::DiGraphMap,
    visit::{DfsPostOrder, IntoNeighbors},
};

use super::*;

pub fn make_ssa(function: &mut TapIrFunction, symtab: &mut SymTab) {
    let mut converter = SsaConverter::new(function);

    let mut post_order = DfsPostOrder::new(&converter.graph, function.root);
    let mut post_order_list = Vec::with_capacity(converter.graph.node_count());
    while let Some(next) = post_order.next(&converter.graph) {
        post_order_list.push(next);
    }

    for block_id in post_order_list.into_iter().rev() {
        let block = function.block_mut(block_id).expect("Failed to get block");
        let block_id = block.id();

        for instr in block.instrs_mut() {
            for source in instr.sources_mut() {
                *source = converter.read_variable(*source, block_id, symtab);
            }

            for target in instr.targets_mut() {
                let new_target = symtab.new_rename(*target);
                converter.write_variable(*target, block_id, new_target);
                *target = new_target;
            }
        }

        for source in block.block_exit_mut().sources_mut() {
            *source = converter.read_variable(*source, block_id, symtab);
        }
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

    phis: HashMap<BlockId, HashMap<SymbolId, Vec<SymbolId>>>,
    graph: DiGraphMap<BlockId, ()>,
}

impl SsaConverter {
    pub fn new(function: &TapIrFunction) -> Self {
        let mut full_graph = DiGraphMap::new();
        for block in function.blocks() {
            for neighbour in function.neighbors(block.id()) {
                full_graph.add_edge(block.id(), neighbour, ());
            }
        }

        Self {
            graph: full_graph,
            phis: Default::default(),

            current_def: Default::default(),
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
    ) -> SymbolId {
        if let Some(renamed) = self.current_def.get(&variable).and_then(|d| d.get(&block)) {
            return *renamed;
        }

        self.read_variable_recursive(variable, block, symtab)
    }

    fn read_variable_recursive(
        &mut self,
        variable: SymbolId,
        block: BlockId,
        symtab: &mut SymTab,
    ) -> SymbolId {
        let predecessors = self
            .graph
            .neighbors_directed(block, Direction::Incoming)
            .collect::<Vec<_>>();

        // Optimise for the common case of one predecessor: no phi needed
        if predecessors.len() == 1 {
            let val = self.read_variable(variable, predecessors[0], symtab);
            self.write_variable(variable, block, val);
            return val;
        }

        // Break potential cycles by putting the phi in there already
        let phi_variable = symtab.new_rename(variable);
        self.phis
            .entry(block)
            .or_default()
            .insert(phi_variable, vec![]);

        self.write_variable(variable, block, phi_variable);

        let val = self.add_phi_operands(
            variable,
            block,
            phi_variable,
            predecessors.into_iter(),
            symtab,
        );
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
    ) -> SymbolId {
        for predecessor in predecessors {
            let value = self.read_variable(variable, predecessor, symtab);

            self.phis
                .entry(block)
                .or_default()
                .entry(phi_variable)
                .or_default()
                .push(value);
        }

        phi_variable
    }

    pub fn into_phis(self) -> impl Iterator<Item = (BlockId, Vec<Phi>)> {
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

            let mut irs = script
                .functions
                .iter()
                .map(|f| create_ir(f, &mut symtab))
                .collect::<Vec<_>>();

            for f in &mut irs {
                make_ssa(f, &mut symtab);
            }

            let mut output = String::new();

            for ir in irs {
                pretty_print::pretty_print_tapir_function(&ir, &symtab, &mut output).unwrap();
            }

            assert_snapshot!(output);
        });
    }
}
