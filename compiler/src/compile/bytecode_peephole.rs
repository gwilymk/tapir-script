use bytecode::{Opcode, Type3};

use crate::compile::Bytecode;

pub fn peephole_optimisation(bytecode: &mut Bytecode) {
    let mut i = 0;
    let mut visited = Vec::new();

    while i < bytecode.data.len() {
        visited.clear();

        let instr = bytecode.data[i];
        let disassembly = bytecode::opcode(instr).expect("Failed to decode instruction");

        if matches!(disassembly, Opcode::LoadConstant) {
            // these have size 2, so we need to skip over the actual constant value
            i += 1;
        } else if matches!(disassembly, Opcode::Jump)
            && let Some(target) = jump_target(&bytecode.data, i, &mut visited)
        {
            let mut current = Type3::decode(instr);
            current.value = target as u32;
            bytecode.data[i] = current.encode();
        }

        i += 1;
    }
}

/// Returns the eventual target of the jump, or None if its an infinite loop
fn jump_target(data: &[u32], location: usize, visited: &mut Vec<usize>) -> Option<usize> {
    visited.push(location);

    // check the target
    let target = Type3::decode(data[location]).value as usize;

    if visited.contains(&target) {
        return None;
    }

    if bytecode::opcode(data[target]).expect("failed to decode instruction") == Opcode::Jump
        && let Some(target) = jump_target(data, target, visited)
    {
        return Some(target);
    }

    Some(target)
}
