use petgraph::visit::{GraphBase, IntoNeighbors, Visitable};

use super::*;

impl GraphBase for TapIrFunction {
    type EdgeId = ();
    type NodeId = BlockId;
}

impl IntoNeighbors for &TapIrFunction {
    type Neighbors = BlockNeighbours;

    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        let block = self.blocks.get(&a).expect("Invalid block ID");

        let (a, b) = match block.block_exit() {
            BlockExitInstr::JumpToBlock(block_id) => (Some(*block_id), None),
            BlockExitInstr::ConditionalJump {
                if_true, if_false, ..
            } => (Some(*if_true), Some(*if_false)),
            BlockExitInstr::Return(_) => (None, None),
        };

        BlockNeighbours(a, b)
    }
}

impl Visitable for TapIrFunction {
    type Map = HashSet<Self::NodeId>;

    fn visit_map(&self) -> Self::Map {
        Self::Map::with_capacity(self.blocks.len())
    }

    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
        map.reserve(self.blocks.len());
    }
}

pub struct BlockNeighbours(Option<BlockId>, Option<BlockId>);

impl Iterator for BlockNeighbours {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.take().or_else(|| self.1.take())
    }
}
