use std::{env, fs, io::Read, process::ExitCode};

use compiler::{CompileSettings, Diagnostics};

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
    };

    match compiler::compile(&display_name, &input, settings) {
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
