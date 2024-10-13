use std::{
    env, fs,
    path::{Path, PathBuf},
};

use compiler::CompileSettings;
use proc_macro2::TokenStream;
use syn::{parse2, LitStr};

pub fn tapir_script(attr: TokenStream, item: TokenStream) -> TokenStream {
    let filename: LitStr = parse2(attr).expect("Argument to tapir_script must be a file");
    let filename = filename.value();

    let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let filename = Path::new(&root).join(&filename);

    let current_working_directory =
        env::current_dir().expect("Could not calculate current working directory");

    let reduced_filename = if filename.starts_with(&current_working_directory) {
        filename
            .components()
            .skip(current_working_directory.components().count())
            .collect::<PathBuf>()
    } else {
        filename
    };

    let file_content = fs::read_to_string(&reduced_filename)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {e}", reduced_filename.display()));

    let compiled_content = match compiler::compile(
        &reduced_filename,
        &file_content,
        CompileSettings { properties: vec![] },
    ) {
        Ok(content) => content,
        Err(mut diagnostics) => {
            eprintln!("{}", diagnostics.pretty_string(true));
            panic!("Compile error");
        }
    };

    todo!("Failed!")
}
