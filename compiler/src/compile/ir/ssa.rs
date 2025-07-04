use petgraph::algo::dominators;

use super::*;

pub fn dominators(function: &TapIrFunction) {
    let root = function.blocks[0].id;
    dominators::simple_fast(function, root);
}
