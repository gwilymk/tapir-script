use petgraph::algo::dominators;

use super::*;

pub fn make_ssa(function: &mut TapIrFunction) {
    let root = function.blocks[0].id;
    let dominators = dominators::simple_fast(&*function, root);
}
