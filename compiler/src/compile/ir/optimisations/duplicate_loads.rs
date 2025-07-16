use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ast::SymbolId,
    compile::ir::{
        Constant, TapIrFunction, TapIrFunctionBlockIter, TapIrInstr,
        optimisations::OptimisationResult,
    },
};

pub fn duplicate_loads(f: &mut TapIrFunction) -> OptimisationResult {
    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        let mut constants_in_block: HashMap<Constant, SymbolId> = HashMap::new();

        for instr in block.instrs_mut() {
            if let TapIrInstr::Constant(t, value) = &instr.instr {
                let mut value = *value;
                if let Constant::Fix(n) = value
                    && n.frac() == 0
                {
                    value = Constant::Int(n.floor());
                }

                let entry = constants_in_block.entry(value);
                let target = *t;

                match entry {
                    Entry::Occupied(occupied_entry) => {
                        did_something = OptimisationResult::DidSomething;

                        instr.instr = TapIrInstr::Move {
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
