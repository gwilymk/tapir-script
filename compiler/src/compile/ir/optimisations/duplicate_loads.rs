use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ast::SymbolId,
    compile::ir::{
        Constant, TapIr, TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
    },
};

pub fn duplicate_loads(f: &mut TapIrFunction) -> OptimisationResult {
    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        let mut constants_in_block: HashMap<Constant, SymbolId> = HashMap::new();

        for instr in block.instrs_mut() {
            if let TapIr::Constant(t, value) = instr {
                let value = *value;
                // Note: We don't merge Fix and Int constants even for zero, because
                // they're used in different type contexts (e.g., f* expects fix operands).
                // The old code tried to merge Fix(n) with Int(n.floor()) when n.frac() == 0,
                // but that was wrong because Fix(2.0) = 512 while Int(2) = 2.

                let entry = constants_in_block.entry(value);
                let target = *t;

                match entry {
                    Entry::Occupied(occupied_entry) => {
                        did_something = OptimisationResult::DidSomething;

                        *instr = TapIr::Move {
                            target,
                            source: *occupied_entry.get(),
                        }
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(target);
                    }
                }
            }
        }
    }

    did_something
}
