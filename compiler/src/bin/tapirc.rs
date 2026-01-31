use std::{env, fs, process::ExitCode};

use compiler::{CompileSettings, Diagnostics};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: tapirc <file.tapir>");
        return ExitCode::from(2);
    }

    let filename = &args[1];
    let input = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            return ExitCode::from(2);
        }
    };

    let settings = CompileSettings {
        available_fields: None,
        enable_optimisations: true,
    };

    match compiler::compile(filename, &input, settings) {
        Ok(result) => {
            // Print any warnings
            if result.warnings.has_any() {
                print_diagnostics(result.warnings);
            }
            println!("success");
            ExitCode::SUCCESS
        }
        Err(diagnostics) => {
            print_diagnostics(diagnostics);
            ExitCode::FAILURE
        }
    }
}

fn print_diagnostics(mut diagnostics: Diagnostics) {
    // Use colors if stderr is a terminal
    let use_colors = std::io::IsTerminal::is_terminal(&std::io::stderr());
    eprint!("{}", diagnostics.pretty_string(use_colors));
}
