#![deny(clippy::all)]
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

    let syn::Data::Struct(data) = &ast.data else {
        panic!("Can only be defined on structs");
    };

    let reduced_filename = get_script_path(&ast);

    let file_content = fs::read_to_string(&reduced_filename)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {e}", reduced_filename.display()));

    let properties = if let syn::Fields::Named(named) = &data.fields {
        extract_properties(named)
    } else {
        vec![]
    };

    let compiled_content = match compiler::compile(
        &reduced_filename,
        &file_content,
        CompileSettings {
            properties: properties
                .iter()
                .map(|property| property.property.clone())
                .collect(),
            enable_optimisations: true,
        },
    ) {
        Ok(content) => content,
        Err(mut diagnostics) => {
            eprintln!("{}", diagnostics.pretty_string(true));
            panic!("Compile error");
        }
    };

    let setters = properties.iter().map(|property| &property.setter);
    let getters = properties.iter().map(|property| &property.getter);

    let struct_name = ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let reduced_filename = reduced_filename.canonicalize().unwrap();
    let reduced_filename = reduced_filename.to_string_lossy();

    let bytecode = &compiled_content.bytecode;

    quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::tapir_script::TapirScript for #struct_name #ty_generics #where_clause {
            fn script(self) -> ::tapir_script::Script<Self> {
                static BYTECODE: &[u16] = &[#(#bytecode),*];

                ::tapir_script::Script::new(self, BYTECODE)
            }

            type EventType = ();
            fn create_event(&self, index: u8, stack: &mut Vec<i32>) -> Self::EventType {}

            fn set_prop(&mut self, index: u8, value: i32) {
                match index {
                    #(#setters,)*
                    _ => unreachable!("Invalid index {index}"),
                };
            }

            fn get_prop(&self, index: u8) -> i32 {
                match index {
                    #(#getters,)*
                    _ => unreachable!("Invalid index {index}"),
                }
            }
        }

        const _: &[u8] = include_bytes!(#reduced_filename);
    }
}

fn get_script_path(ast: &DeriveInput) -> PathBuf {
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
    reduced_filename
}

struct DeriveProperty {
    property: Property,
    getter: TokenStream,
    setter: TokenStream,
}

fn extract_properties(named: &syn::FieldsNamed) -> Vec<DeriveProperty> {
    named
        .named
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            let field_ident = field.ident.as_ref().unwrap();
            let prop_name = field_ident.to_string();

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
                Some("skip") => {
                    return None;
                }
                Some(unknown) => panic!("Unknown type {unknown} on property {prop_name}"),
            };

            let i_u8 = i as u8;

            Some(DeriveProperty {
                property: Property {
                    ty,
                    index: i,
                    name: prop_name,
                },
                setter: quote! {
                    #i_u8 => { ::tapir_script::TapirProperty::set_from_i32(&mut self.#field_ident, value); }
                },
                getter: quote! {
                    #i_u8 => ::tapir_script::TapirProperty::to_i32(&self.#field_ident)
                },
            })
        })
        .collect()
}
