use std::{
    env, fs,
    path::{Path, PathBuf},
};

use compiler::{CompileSettings, Property, Type};
use proc_macro2::TokenStream;
use quote::quote;
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

    let filename = &top_level_tapir_attribute.parse_args::<LitStr>().unwrap_or_else(|_| {
            panic!(r#"tapir must take exactly 1 argument which is a path to the script, so be of the format #[tapir("path/to/my/script.tapir")]"#)
    });

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

    let properties = if let syn::Fields::Named(named) = &data.fields {
        named
            .named
            .iter()
            .enumerate()
            .map(|(i, field)| {
                let prop_name = field.ident.as_ref().unwrap().to_string();

                let ty = field
                    .attrs
                    .iter()
                    .find(|attr| attr.meta.path().is_ident("tapir"))
                    .map(|attr| {
                        attr.parse_args::<syn::Ident>()
                            .unwrap_or_else(|_| {
                                panic!("tapir attribute on property {prop_name} is invalid",)
                            })
                            .to_string()
                    });

                let ty = match ty.as_deref() {
                    None => {
                        panic!("Must specify the type for every property, missing on {prop_name}")
                    }
                    Some("int") => Type::Int,
                    Some("bool") => Type::Bool,
                    Some("fix") => Type::Fix,
                    Some(unknown) => panic!("Unknown type {unknown} on property {prop_name}"),
                };

                Property {
                    ty,
                    index: i,
                    name: prop_name,
                }
            })
            .collect()
    } else {
        vec![]
    };

    let compiled_content = match compiler::compile(
        &reduced_filename,
        &file_content,
        CompileSettings { properties },
    ) {
        Ok(content) => content,
        Err(mut diagnostics) => {
            eprintln!("{}", diagnostics.pretty_string(true));
            panic!("Compile error");
        }
    };

    let struct_name = ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let (setters, getters): (Vec<_>, Vec<_>) = data
        .fields
        .members()
        .enumerate()
        .map(|(i, member)| {
            let i = i as u8;

            (
                quote! {
                    #i => { self.#member = value; }
                },
                quote! {
                    #i => self.#member
                },
            )
        })
        .unzip();

    quote! {
        unsafe impl #impl_generics ::tapir_script::TapirScript for #struct_name #ty_generics #where_clause {
            fn script(self) -> vm::Script<Self> {
                static BYTECODE: &[u16] = &[#(#compiled_content),*];

                vm::Script::new(self, BYTECODE)
            }

            type EventType = ();
            fn create_event(&self, index: u8, stack: &mut Vec<i32>) -> Self::EventType {}

            fn set_prop(&mut self, index: u8, value: i32) {
                match index {
                    #(#setters)*
                    _ => unreachable!("Invalid index {index}"),
                };
            }

            fn get_prop(&self, index: u8) -> i32 {
                match index {
                    #(#getters),*,
                    _ => unreachable!("Invalid index {index}"),
                }
            }
        }
    }
}
