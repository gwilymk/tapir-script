use crate::compile::ir::{
    Constant, StoreValue, TapIr, TapIrFunction, TapIrFunctionBlockIter,
    optimisations::OptimisationResult,
};

/// Extract the raw i32 value from a constant.
fn raw_value(c: Constant) -> i32 {
    match c {
        Constant::Int(v) => v,
        Constant::Fix(f) => f.to_raw(),
        Constant::Bool(b) => b as i32,
    }
}

/// Convert `StoreProp { value: Symbol(sym) }` and `SetGlobal { value: Symbol(sym) }`
/// to use `StoreValue::Immediate(imm)` when `sym` is a constant that fits in i16.
pub fn immediate_stores(f: &mut TapIrFunction) -> OptimisationResult {
    let constants = f.constant_map();

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(f);
    while let Some(block) = dfs.next_mut(f) {
        for instr in block.instrs_mut() {
            let value = match instr {
                TapIr::StoreProp { value, .. } | TapIr::SetGlobal { value, .. } => value,
                _ => continue,
            };

            let StoreValue::Symbol(sym) = *value else {
                continue;
            };

            if let Some(&constant) = constants.get(&sym) {
                let raw = raw_value(constant);
                if let Ok(imm) = i16::try_from(raw) {
                    *value = StoreValue::Immediate(imm);
                    did_something = OptimisationResult::DidSomething;
                }
            }
        }
    }

    did_something
}
