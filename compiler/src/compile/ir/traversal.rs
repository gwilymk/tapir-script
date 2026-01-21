use std::collections::HashSet;

use petgraph::visit::{Dfs, DfsPostOrder};

use super::{BlockId, TapIrBlock, TapIrFunction};

pub enum TapIrFunctionBlockIter {
    Dfs(Dfs<BlockId, HashSet<BlockId>>),
    PostOrder(DfsPostOrder<BlockId, HashSet<BlockId>>),
    ReversePostOrder(std::vec::IntoIter<BlockId>),
}

impl TapIrFunctionBlockIter {
    pub fn new_dfs(f: &TapIrFunction) -> Self {
        Self::Dfs(Dfs::new(f, f.root))
    }

    pub fn new_post_order(f: &TapIrFunction) -> Self {
        Self::PostOrder(DfsPostOrder::new(f, f.root))
    }

    pub fn new_reverse_post_order(f: &TapIrFunction) -> Self {
        let mut post_order = DfsPostOrder::new(f, f.root);
        let mut post_order_list = Vec::with_capacity(f.blocks.len());

        while let Some(next) = post_order.next(f) {
            post_order_list.push(next);
        }

        post_order_list.reverse();
        Self::ReversePostOrder(post_order_list.into_iter())
    }

    pub fn next<'a>(&mut self, f: &'a TapIrFunction) -> Option<&'a TapIrBlock> {
        self.next_id(f)
            .map(|block_id| f.block(block_id).expect("Should find the block"))
    }

    pub fn next_mut<'a>(&mut self, f: &'a mut TapIrFunction) -> Option<&'a mut TapIrBlock> {
        self.next_id(f)
            .map(move |block_id| f.block_mut(block_id).expect("Should find the block"))
    }

    pub fn next_id(&mut self, f: &TapIrFunction) -> Option<BlockId> {
        match self {
            TapIrFunctionBlockIter::Dfs(dfs) => dfs.next(f),
            TapIrFunctionBlockIter::PostOrder(dfs_post_order) => dfs_post_order.next(f),
            TapIrFunctionBlockIter::ReversePostOrder(iter) => iter.next(),
        }
    }
}
