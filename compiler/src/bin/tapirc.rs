use std::{env, fs, io::Read, process::ExitCode};

use compiler::{CompileResult, CompileSettings, Diagnostics, disassemble};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: tapirc <file.tapir>");
        eprintln!("       tapirc -  (read from stdin)");
        return ExitCode::from(2);
    }

    let filename = &args[1];
    let (display_name, input) = if filename == "-" {
        let mut input = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut input) {
            eprintln!("Error reading from stdin: {}", e);
            return ExitCode::from(2);
        }
        ("<stdin>".to_string(), input)
    } else {
        match fs::read_to_string(filename) {
            Ok(content) => (filename.clone(), content),
            Err(e) => {
                eprintln!("Error reading file '{}': {}", filename, e);
                return ExitCode::from(2);
            }
        }
    };

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: true,
        enable_prelude: true,
    };

    match compiler::compile(&display_name, &input, settings) {
        Ok(result) => {
            // Print result first
            print_result(&result);
            // Print any warnings at the end
            if result.warnings.has_any() {
                print_diagnostics(result.warnings);
            }
            ExitCode::SUCCESS
        }
        Err(diagnostics) => {
            print_diagnostics(diagnostics);
            ExitCode::FAILURE
        }
    }
}

fn print_result(result: &CompileResult) {
    // Properties
    if !result.properties.is_empty() {
        println!("=== Properties ({}) ===", result.properties.len());
        for prop in result.properties.iter() {
            println!("  [{}] {}: {}", prop.index, prop.name, prop.ty);
        }
        println!();
    }

    // Globals
    if !result.globals.is_empty() {
        println!("=== Globals ({}) ===", result.globals.len());
        for (i, value) in result.globals.iter().enumerate() {
            println!("  [{}] = {}", i, value);
        }
        println!();
    }

    // Event handlers
    if !result.event_handlers.is_empty() {
        println!("=== Event Handlers ({}) ===", result.event_handlers.len());
        for handler in result.event_handlers.iter() {
            let args: Vec<_> = handler
                .arguments
                .iter()
                .map(|a| format!("{}: {}", a.name, a.ty))
                .collect();
            println!(
                "  {}({}) @ offset {}",
                handler.name,
                args.join(", "),
                handler.bytecode_offset
            );
        }
        println!();
    }

    // Triggers
    if !result.triggers.is_empty() {
        println!("=== Triggers ({}) ===", result.triggers.len());
        for (i, trigger) in result.triggers.iter().enumerate() {
            let args: Vec<_> = trigger.arguments.iter().map(|t| t.to_string()).collect();
            println!("  [{}] {}({})", i, trigger.name, args.join(", "));
        }
        println!();
    }

    // Extern functions
    if !result.extern_functions.is_empty() {
        println!(
            "=== Extern Functions ({}) ===",
            result.extern_functions.len()
        );
        for (i, func) in result.extern_functions.iter().enumerate() {
            let args: Vec<_> = func.arguments.iter().map(|t| t.to_string()).collect();
            let returns: Vec<_> = func.returns.iter().map(|t| t.to_string()).collect();
            let ret_str = if returns.is_empty() {
                String::new()
            } else {
                format!(" -> {}", returns.join(", "))
            };
            println!("  [{}] {}({}){}", i, func.name, args.join(", "), ret_str);
        }
        println!();
    }

    // Disassembly
    println!(
        "=== Bytecode ({} instructions, {} bytes) ===",
        result.bytecode.len(),
        result.bytecode.len() * 4
    );
    let mut disasm = String::new();
    disassemble::disassemble(&result.bytecode, &mut disasm).unwrap();
    print!("{}", disasm);
}

fn print_diagnostics(mut diagnostics: Diagnostics) {
    // Use colors if stderr is a terminal
    let use_colors = std::io::IsTerminal::is_terminal(&std::io::stderr());
    eprint!("{}", diagnostics.pretty_string(use_colors));
}
