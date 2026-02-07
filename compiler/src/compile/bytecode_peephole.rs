use std::collections::HashSet;

use bytecode::{Opcode, Type3};

use crate::compile::Bytecode;

pub fn peephole_optimisation(bytecode: &mut Bytecode) {
    let mut i = 0;
    while i < bytecode.data.len() {
        let instr = bytecode.data[i];
        let disassembly = bytecode::opcode(instr).expect("Failed to decode instruction");

        if matches!(disassembly, Opcode::LoadConstant) {
            // these have size 2, so we need to skip over the actual constant value
            i += 1;
        } else if let Some(target) = is_jump(&bytecode.data, i, HashSet::new()) {
            let mut current = Type3::decode(instr);
            current.value = target as u32;
            bytecode.data[i] = current.encode();
        }

        i += 1;
    }
}

/// Returns Some(target) if it's a jump, or None if it isn't
fn is_jump(data: &[u32], location: usize, mut visited: HashSet<usize>) -> Option<usize> {
    visited.insert(location);
    let disassembly = bytecode::opcode(data[location]).expect("Failed to decode instruction");

    if disassembly != Opcode::Jump {
        return None;
    }

    // check the target
    let target = Type3::decode(data[location]).value as usize;

    if visited.contains(&target) {
        return None;
    }

    if let Some(target) = is_jump(data, target, visited) {
        return Some(target);
    }

    Some(target)
}
