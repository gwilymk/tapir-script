use petgraph::{prelude::DiGraphMap, visit::IntoNeighbors};

use crate::compile::ir::{
    BlockExitInstr, TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
};

pub fn simplify_blocks(f: &mut TapIrFunction) -> OptimisationResult {
    let mut did_something = OptimisationResult::DidNothing;

    // we need to know the predecessors of every block unfortunately
    let mut full_graph = DiGraphMap::new();
    for block in f.blocks() {
        for neighbour in f.neighbors(block.id()) {
            full_graph.add_edge(block.id(), neighbour, ());
        }
    }

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block_id) = dfs.next_id(f) {
        let block = f.block(block_id).unwrap();
        let BlockExitInstr::JumpToBlock(next_block_id) = block.block_exit() else {
            // this block doesn't have an unconditional jump in it
            continue;
        };

        let next_block_id = *next_block_id;

        if next_block_id == block_id {
            // this block references itself, so we can't do any shuffling
            continue;
        }

        if full_graph
            .edges_directed(next_block_id, petgraph::Direction::Incoming)
            .count()
            != 1
        {
            // we can't just copy the content of the next block into this one
            continue;
        }

        // copy the content of the next block into this one, since there isn't another block jumping to it
        let next_block = f.block(next_block_id).unwrap();

        if next_block.instrs.is_empty() {
            // nothing to do here since there aren't any instructions in next
            continue;
        }

        if !next_block.block_entry().is_empty() {
            // phis make this a little complex, so don't bother
            continue;
        }

        // Take the instructions from next_block by getting it mutably
        let next_instrs = std::mem::take(&mut f.block_mut(next_block_id).unwrap().instrs);
        f.block_mut(block_id).unwrap().instrs.extend(next_instrs);

        did_something = OptimisationResult::DidSomething;
    }

    did_something
}
