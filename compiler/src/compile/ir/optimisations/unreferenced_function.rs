use petgraph::{prelude::DiGraphMap, visit::Dfs};

use crate::{
    ast::FunctionId,
    compile::ir::{TapIrFunction, TapIrInstr, optimisations::OptimisationResult},
};

pub fn remove_unreferenced_functions(functions: &mut Vec<TapIrFunction>) -> OptimisationResult {
    // step 1. Build a call graph for all the functions, and gather the roots
    let mut call_graph = DiGraphMap::new();
    let mut roots = vec![FunctionId::toplevel()];

    for f in functions.iter() {
        if f.modifiers().event_handler.is_some() {
            roots.push(f.id());
        }

        for block in f.blocks() {
            for instr in block.instrs() {
                let TapIrInstr::Call { f: callee, .. } = instr.instr else {
                    continue;
                };

                call_graph.add_edge(f.id(), callee, ());
            }
        }
    }

    // step 2. Collect all the referenced functions (either from event handlers or the top level)
    let mut dfs = Dfs::empty(&call_graph);
    for root in roots {
        dfs.move_to(root);
        while dfs.next(&call_graph).is_some() {}
    }

    let reachable = dfs.discovered;

    // step 3. Remove all the unreachable functions.
    //
    // If the reachable list is shorter than the list
    // of functions, then we know that there is nothing to do.
    if reachable.len() == functions.len() {
        return OptimisationResult::DidNothing;
    }

    functions.retain(|f| reachable.contains(&f.id()));

    OptimisationResult::DidSomething
}
