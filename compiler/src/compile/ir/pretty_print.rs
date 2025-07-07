use std::fmt::Write;

use super::*;

fn pretty_print_tapir(ir: &TapIr, symtab: &SymTab<'_>, output: &mut dyn Write) -> std::fmt::Result {
    match &ir.instr {
        TapIrInstr::Constant(symbol_id, constant) => write!(
            output,
            "{} = {constant}",
            symtab.debug_name_for_symbol(*symbol_id)
        ),
        TapIrInstr::Move { target, source } => write!(
            output,
            "{} = {}",
            symtab.debug_name_for_symbol(*target),
            symtab.debug_name_for_symbol(*source)
        ),
        TapIrInstr::BinOp {
            target,
            lhs,
            op,
            rhs,
        } => write!(
            output,
            "{} = {} {op} {}",
            symtab.debug_name_for_symbol(*target),
            symtab.debug_name_for_symbol(*lhs),
            symtab.debug_name_for_symbol(*rhs)
        ),
        TapIrInstr::Wait => write!(output, "wait"),
        TapIrInstr::Call { target, f, args } => {
            let mut targets = target
                .iter()
                .map(|t| symtab.debug_name_for_symbol(*t))
                .collect::<Vec<_>>()
                .join(", ");
            let args = args
                .iter()
                .map(|t| symtab.debug_name_for_symbol(*t))
                .collect::<Vec<_>>()
                .join(", ");

            if !targets.is_empty() {
                targets += " = ";
            }

            write!(output, "{targets}{}({args})", symtab.name_for_function(*f))
        }
        TapIrInstr::Spawn { f, args } => {
            let args = args
                .iter()
                .map(|t| symtab.debug_name_for_symbol(*t))
                .collect::<Vec<_>>()
                .join(", ");

            write!(output, "spawn {}({args})", symtab.name_for_function(*f))
        }
        TapIrInstr::Trigger { f, args } => {
            let args = args
                .iter()
                .map(|t| symtab.debug_name_for_symbol(*t))
                .collect::<Vec<_>>()
                .join(", ");

            write!(output, "trigger {f:?}({args})")
        }
        TapIrInstr::GetProp { target, prop_index } => {
            write!(
                output,
                "getprop {}, {prop_index}",
                symtab.debug_name_for_symbol(*target)
            )
        }
        TapIrInstr::StoreProp { prop_index, value } => {
            write!(
                output,
                "storeprop {}, {prop_index}",
                symtab.debug_name_for_symbol(*value)
            )
        }
    }
}

fn pretty_print_tapir_exit_instr(
    block_exit_instr: &BlockExitInstr,
    symtab: &SymTab<'_>,
    output: &mut dyn Write,
) -> std::fmt::Result {
    match block_exit_instr {
        BlockExitInstr::JumpToBlock(block_id) => write!(output, "jmp {}", block_id.0),
        BlockExitInstr::ConditionalJump {
            test,
            if_true,
            if_false,
        } => write!(
            output,
            "conditional_jump {} {} {}",
            symtab.debug_name_for_symbol(*test),
            if_true.0,
            if_false.0
        ),
        BlockExitInstr::Return(symbol_ids) => {
            let args = symbol_ids
                .iter()
                .map(|t| symtab.debug_name_for_symbol(*t))
                .collect::<Vec<_>>()
                .join(", ");
            write!(output, "return {args}")
        }
    }
}

fn pretty_print_tapir_block(
    block: &TapIrBlock,
    symtab: &SymTab<'_>,
    output: &mut dyn Write,
) -> std::fmt::Result {
    writeln!(output, "---- block {} ----", block.id.0)?;

    for phi in &block.block_entry {
        let phi_inputs = phi
            .sources
            .iter()
            .map(|s| symtab.debug_name_for_symbol(*s))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            output,
            "{} = φ({})",
            symtab.debug_name_for_symbol(phi.target),
            phi_inputs
        )?;
    }

    for instr in &block.instrs {
        pretty_print_tapir(instr, symtab, output)?;
        writeln!(output)?;
    }

    pretty_print_tapir_exit_instr(&block.block_exit, symtab, output)?;
    writeln!(output)?;
    writeln!(output)
}

pub fn pretty_print_tapir_function(
    function: &TapIrFunction,
    symtab: &SymTab<'_>,
    output: &mut dyn Write,
) -> std::fmt::Result {
    let args = function
        .arguments
        .iter()
        .map(|a| symtab.debug_name_for_symbol(*a))
        .collect::<Vec<_>>()
        .join(", ");

    let returns = function
        .return_types
        .iter()
        .map(|r| r.to_string())
        .collect::<Vec<_>>()
        .join(", ");

    writeln!(
        output,
        "=======================\n{}fn {}({args}) -> {returns}\n",
        if function.modifiers.event_handler.is_some() {
            "event "
        } else {
            ""
        },
        symtab.name_for_function(function.id)
    )?;

    for block in function.blocks() {
        pretty_print_tapir_block(block, symtab, output)?;
    }

    Ok(())
}
