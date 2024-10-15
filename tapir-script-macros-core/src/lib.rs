use std::{
    env, fs,
    path::{Path, PathBuf},
};

use compiler::CompileSettings;
use proc_macro2::TokenStream;
use syn::{parse2, DeriveInput, LitStr};

pub fn tapir_script_derive(struct_def: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse2(struct_def).unwrap();

    let syn::Data::Struct(data) = ast.data else {
        panic!("Can only be defined on structs");
    };

    let Some(top_level_tapir_attribute) = ast
        .attrs
        .iter()
        .find(|attr| attr.meta.path().is_ident("tapir"))
    else {
        panic!(
            r#"Must have a #[tapir("path/to/my/script.tapir")] attribute before the struct definition"#
        );
    };

    let filename = match &top_level_tapir_attribute.meta {
        syn::Meta::List(meta_list) => meta_list.parse_args::<LitStr>().unwrap_or_else(|_| {
            panic!("tapir must take exactly 1 argument which is a path to the script")
        }),
        _ => panic!(
            r#"#[tapir] macro of incorrect format. Should be #[tapir("path/to/my/script.tapir")]"#
        ),
    };

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
