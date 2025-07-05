use petgraph::algo::dominators;

use super::*;

pub fn make_ssa(function: &mut TapIrFunction, symtab: &mut SymTab) {
    let root = function.root;

    let dominators = dominators::simple_fast(&*function, root);
}
