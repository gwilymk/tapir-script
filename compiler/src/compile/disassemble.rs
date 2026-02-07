use std::fmt::{self, Write};

use bytecode::{Opcode, Type1, Type2, Type3};

fn disassemble_non_constant(instr: u32, target: &mut dyn Write) -> fmt::Result {
    macro_rules! t1 {
        ($text:expr, 0) => {
            writeln!(target, "{}", $text)
        };
        ($text:expr, 1) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}", $text, t1.target)
        }};
        ($text:expr, 2) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}, {}", $text, t1.target, t1.a)
        }};
        ($text:expr, 3) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}, {}, {}", $text, t1.target, t1.a, t1.b)
        }};
    }

    match bytecode::opcode(instr)
        .unwrap_or_else(|| panic!("Invalid opcode for instruction {instr}"))
    {
        bytecode::Opcode::Mov => t1!("mov", 2),
        bytecode::Opcode::Add => t1!("add", 3),
        bytecode::Opcode::Sub => t1!("sub", 3),
        bytecode::Opcode::Mul => t1!("mul", 3),
        bytecode::Opcode::Div => t1!("div", 3),
        bytecode::Opcode::Mod => t1!("mod", 3),
        bytecode::Opcode::RealMod => t1!("realmod", 3),
        bytecode::Opcode::RealDiv => t1!("realdiv", 3),
        bytecode::Opcode::EqEq => t1!("eq", 3),
        bytecode::Opcode::NeEq => t1!("neq", 3),
        bytecode::Opcode::Gt => t1!("gt", 3),
        bytecode::Opcode::GtEq => t1!("gte", 3),
        bytecode::Opcode::Lt => t1!("lt", 3),
        bytecode::Opcode::LtEq => t1!("lte", 3),
        bytecode::Opcode::FixMul => t1!("fixmul", 3),
        bytecode::Opcode::FixDiv => t1!("fixdiv", 3),
        bytecode::Opcode::Shl => t1!("shl", 3),
        bytecode::Opcode::Shr => t1!("shr", 3),
        bytecode::Opcode::BitAnd => t1!("bitand", 3),
        bytecode::Opcode::BitOr => t1!("bitor", 3),
        bytecode::Opcode::AddI => t1!("addi", 3),
        bytecode::Opcode::SubI => t1!("subi", 3),
        bytecode::Opcode::MulI => t1!("muli", 3),
        bytecode::Opcode::DivI => t1!("divi", 3),
        bytecode::Opcode::FixAddI => t1!("fixaddi", 3),
        bytecode::Opcode::FixSubI => t1!("fixsubi", 3),
        bytecode::Opcode::FixMulI => t1!("fixmuli", 3),
        bytecode::Opcode::FixDivI => t1!("fixdivi", 3),
        bytecode::Opcode::ShlI => t1!("shli", 3),
        bytecode::Opcode::ShrI => t1!("shri", 3),
        bytecode::Opcode::Neg => t1!("neg", 2),
        bytecode::Opcode::Not => t1!("not", 2),
        bytecode::Opcode::BitNot => t1!("bitnot", 2),
        bytecode::Opcode::GetProp => t1!("getprop", 2),
        bytecode::Opcode::SetProp => t1!("setprop", 2),
        bytecode::Opcode::Call => t1!("call", 1),
        bytecode::Opcode::ExternCall => t1!("extern call", 2),
        bytecode::Opcode::CallBuiltin => t1!("call builtin", 3),
        bytecode::Opcode::Spawn => t1!("spawn", 3),
        bytecode::Opcode::Trigger => t1!("trigger", 2),
        bytecode::Opcode::JumpIf => t1!("jumpif", 1),
        bytecode::Opcode::Ret => t1!("ret", 0),
        bytecode::Opcode::Wait => t1!("wait", 0),
        bytecode::Opcode::GetGlobal => t1!("getglobal", 2),
        bytecode::Opcode::SetGlobal => t1!("setglobal", 2),
        bytecode::Opcode::StackAlloc => t1!("stackalloc", 1),
        bytecode::Opcode::LoadConstant => unreachable!("Load constant should not be hit here"),

        bytecode::Opcode::LoadI => {
            let t2 = Type2::decode(instr);
            writeln!(target, "loadi {}, {}", t2.reg, t2.imm)
        }
        bytecode::Opcode::SetPropI => {
            let t2 = Type2::decode(instr);
            writeln!(target, "setpropi {}, {}", t2.imm, t2.reg)
        }
        bytecode::Opcode::SetGlobalI => {
            let t2 = Type2::decode(instr);
            writeln!(target, "setglobali {}, {}", t2.imm, t2.reg)
        }

        bytecode::Opcode::Jump => {
            let t3 = Type3::decode(instr);
            writeln!(target, "jmp {}", t3.value)
        }
    }
}

pub fn disassemble(instrs: &[u32], target: &mut dyn Write) -> fmt::Result {
    let mut is_constant = false;

    for (i, instr) in instrs.iter().enumerate() {
        if is_constant {
            writeln!(target, "{instr}")?;
            is_constant = false;
            continue;
        }

        write!(target, "{i:08}: ")?;
        if bytecode::opcode(*instr) == Some(Opcode::LoadConstant) {
            let t1 = Type1::decode(*instr);
            write!(target, "loadk {}, ", t1.target)?;
            is_constant = true;
        } else {
            disassemble_non_constant(*instr, target)?;
        }
    }

    Ok(())
}
