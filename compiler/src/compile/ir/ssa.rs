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
    post_order_list.reverse();

    for block_id in post_order_list {
        let block = function.block_mut(block_id).expect("Failed to get block");
        let block_id = block.id();

        for instr in block.instrs_mut() {
            for source in instr.sources_mut() {
                *source = converter.read_variable(*source, block_id, symtab);
            }

            for target in instr.targets_mut() {
                let new_target = symtab.new_rename(*target);
                converter.write_variable(*target, block_id, new_target);
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
