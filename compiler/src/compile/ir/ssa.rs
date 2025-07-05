use petgraph::{Direction, algo::dominators, prelude::DiGraphMap, visit::IntoNeighbors};

use super::*;

pub fn make_ssa(function: &mut TapIrFunction, symtab: &mut SymTab) {
    let root = function.root;

    let dominators = dominators::simple_fast(&*function, root);

    let mut full_graph = DiGraphMap::new();
    for block in function.blocks() {
        for neighbour in function.neighbors(block.id()) {
            full_graph.add_edge(block.id(), neighbour, ());
        }
    }

    let mut dominance_frontiers = HashMap::new();
    for block in function.blocks() {
        let predecessors = full_graph
            .neighbors_directed(block.id(), Direction::Incoming)
            .collect::<Box<[_]>>();

        if predecessors.len() < 2 {
            continue;
        }

        let Some(immediate_dominator) = dominators.immediate_dominator(block.id()) else {
            continue;
        };

        for predecessor in predecessors {
            let mut runner = predecessor;

            while runner != immediate_dominator {
                dominance_frontiers
                    .entry(runner)
                    .or_insert_with(HashSet::new)
                    .insert(block.id());
                let Some(next_dominator) = dominators.immediate_dominator(runner) else {
                    break;
                };
                runner = next_dominator;
            }
        }
    }
}
