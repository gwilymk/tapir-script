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

        BlockNeighbours(block.block_exit.clone(), 0)
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

pub struct BlockNeighbours(BlockExitInstr, usize);

impl Iterator for BlockNeighbours {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.0 {
            BlockExitInstr::JumpToBlock(block_id) => {
                if self.1 == 0 {
                    self.1 += 1;
                    Some(*block_id)
                } else {
                    None
                }
            }
            BlockExitInstr::ConditionalJump {
                test: _,
                if_true,
                if_false,
            } => {
                self.1 += 1;
                match self.1 {
                    1 => Some(*if_true),
                    2 => Some(*if_false),
                    _ => None,
                }
            }
            BlockExitInstr::Return(_) => None,
        }
    }
}
