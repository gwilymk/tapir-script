use std::{io::Read, path::Path, process::ExitCode};

use clap::{Parser, Subcommand};
use compiler::{
    CompileResult, CompileSettings, Diagnostics, FsFileLoader, PRELUDE_PATH, PRELUDE_SOURCE,
    compile_with_loader, disassemble,
};

#[derive(Parser)]
#[command(name = "tapirc")]
#[command(about = "Tapir-script toolchain", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile a tapir-script file
    Compile {
        /// Input file to compile (use - for stdin)
        file: String,

        /// Disable optimizations
        #[arg(long)]
        no_opt: bool,

        /// Disable the prelude
        #[arg(long)]
        no_prelude: bool,
    },
    /// Start the language server (LSP) over stdio
    Lsp,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Command::Compile {
            file,
            no_opt,
            no_prelude,
        } => compile_command(&file, no_opt, no_prelude),
        Command::Lsp => lsp_command(),
    }
}

fn lsp_command() -> ExitCode {
    match tapir_lsp::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("LSP error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn compile_command(file: &str, no_opt: bool, no_prelude: bool) -> ExitCode {
    let loader = FsFileLoader::new();

    // Insert prelude if enabled
    if !no_prelude {
        loader.insert(PRELUDE_PATH, PRELUDE_SOURCE);
    }

    // Handle stdin or file input
    let file_path = if file == "-" {
        // Read from stdin and insert into loader
        let mut input = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut input) {
            eprintln!("Error reading from stdin: {}", e);
            return ExitCode::from(2);
        }
        let path = "<stdin>";
        loader.insert(path, input);
        path
    } else {
        file
    };

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: !no_opt,
        enable_prelude: !no_prelude,
        // CLI doesn't know about Rust attributes, so be permissive
        has_event_type: true,
    };

    match compile_with_loader(Path::new(file_path), &settings, &loader) {
        Ok(result) => {
            print_result(&result);
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
    let use_colors = std::io::IsTerminal::is_terminal(&std::io::stderr());
    eprint!("{}", diagnostics.pretty_string(use_colors));
}
