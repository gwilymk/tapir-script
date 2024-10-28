#![deny(clippy::all)]
use std::{
    env, fs,
    path::{Path, PathBuf},
};

use compiler::{CompileSettings, Property, Type};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse2, DeriveInput, Ident, LitStr, Token};

pub fn tapir_script_derive(struct_def: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse2(struct_def).unwrap();

    let syn::Data::Struct(data) = &ast.data else {
        panic!("Can only be defined on structs");
    };

    let (reduced_filename, trigger_type) = get_script_path(&ast);

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

    if compiled_content.triggers.len() > 0 && trigger_type.is_none() {
        panic!("Tapir code is calling triggers, but no trigger_type defined");
    }

    let trigger_type = trigger_type
        .map(|t| t.into_token_stream())
        .unwrap_or(quote! { () });

    let triggers = compiled_content
        .triggers
        .iter()
        .enumerate()
        .map(|(trigger_index, trigger)| {
            let ident = format_ident!("{}", trigger.name);

            let (mut args, definitions): (Vec<_>, Vec<_>) = trigger
                .arguments
                .iter()
                .enumerate()
                .rev()
                .map(|(index, ty)| {
                    let arg_name = format_ident!("arg{index}");
                    let pop = quote!(stack.pop().expect("Stack underflow"));
                    let value = match *ty {
                        Type::Int => quote! { #pop },
                        Type::Fix => quote! { ::tapir_script::Fix::from_raw(#pop) },
                        Type::Bool => quote! { #pop != 0 },
                        _ => panic!("Unknown type {ty}"),
                    };

                    (arg_name.clone(), quote! { let #arg_name = #value })
                })
                .unzip();

            let trigger_index = trigger_index as u8;

            let args = if args.len() == 0 {
                quote! {}
            } else {
                args.reverse();
                quote! { (#(#args,)*) }
            };

            quote! {
                #trigger_index => {
                    #(#definitions;)*

                    #trigger_type::#ident #args
                }
            }
        });

    let setters = properties.iter().map(|property| &property.setter);
    let getters = properties.iter().map(|property| &property.getter);

    let struct_name = ast.ident;
    let visibility = ast.vis;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let reduced_filename = reduced_filename.canonicalize().unwrap();
    let reduced_filename = reduced_filename.to_string_lossy();

    let bytecode = &compiled_content.bytecode;
    let event_handlers = compiled_content.event_handlers;

    let (event_handler_trait_fns, event_handler_trait_impls) =
        generate_event_handlers(event_handlers);

    let event_handler_trait_name = format_ident!("{}Events", struct_name);

    quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::tapir_script::TapirScript for #struct_name #ty_generics #where_clause {
            fn script(self) -> ::tapir_script::Script<Self> {
                static BYTECODE: &[u16] = &[#(#bytecode),*];

                ::tapir_script::Script::new(self, BYTECODE)
            }

            type EventType = #trigger_type;
            fn create_event(&self, index: u8, stack: &mut Vec<i32>) -> Self::EventType {
                match index {
                    #(#triggers,)*
                    _ => unreachable!("Invalid index {index}"),
                }
            }

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

        #visibility trait #event_handler_trait_name {
            #(#event_handler_trait_fns;)*
        }

        impl #impl_generics #event_handler_trait_name for ::tapir_script::Script<#struct_name #ty_generics> #where_clause {
            #(#event_handler_trait_impls)*
        }

        const _: &[u8] = include_bytes!(#reduced_filename);
    }
}

fn generate_event_handlers(
    event_handlers: Vec<compiler::EventHandler>,
) -> (Vec<TokenStream>, Vec<TokenStream>) {
    event_handlers
        .iter()
        .map(|event_handler| {
            let arg_definitions = event_handler
                .arguments
                .iter()
                .map(|arg| {
                    let kind = match arg.ty {
                        Type::Int => quote!(i32),
                        Type::Fix => quote!(::tapir_script::Fix),
                        Type::Bool => quote!(bool),
                        Type::Error => panic!("Should not have errors here"),
                    };
                    let name = format_ident!("{}", arg.name);

                    quote!(#name: #kind)
                })
                .collect::<Vec<_>>();

            let event_name = format_ident!("on_{}", event_handler.name);

            let initial_stack_vector = event_handler.arguments.iter().map(|arg| {
                let arg_name = format_ident!("{}", arg.name);
                quote! {
                    initial_stack.push(::tapir_script::TapirProperty::to_i32(&#arg_name))
                }
            });

            let initial_stack_vector_len = event_handler.arguments.len() + 1; // the mandatory return value

            let initial_pc = event_handler.bytecode_offset;

            (
                quote!(fn #event_name(&mut self, #(#arg_definitions,)*)),
                quote! {
                    fn #event_name(&mut self, #(#arg_definitions,)*) {
                        let mut initial_stack = ::tapir_script::Vec::with_capacity(#initial_stack_vector_len);
                        #(#initial_stack_vector;)*

                        unsafe { self.__private_trigger_event(initial_stack, #initial_pc); }
                    }
                },
            )
        })
        .unzip()
}

fn get_script_path(ast: &DeriveInput) -> (PathBuf, Option<syn::Path>) {
    let Some(top_level_tapir_attribute) = ast
        .attrs
        .iter()
        .find(|attr| attr.meta.path().is_ident("tapir"))
    else {
        panic!(
            r#"Must have a #[tapir("path/to/my/script.tapir")] attribute before the struct definition"#
        );
    };

    let top_level_args = top_level_tapir_attribute.parse_args::<TopLevelTapirArgs>()
        .expect(r#"tapir must take exactly 1 argument which is a path to the script, so be of the format #[tapir("path/to/my/script.tapir")]"#);

    let filename = top_level_args.script_name.value();

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

    (reduced_filename, top_level_args.trigger_type)
}

struct TopLevelTapirArgs {
    script_name: LitStr,
    trigger_type: Option<syn::Path>,
}

impl syn::parse::Parse for TopLevelTapirArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let script_name = input.parse()?;
        let mut trigger_type = None;

        if input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            let ident: Ident = input.parse()?;
            let _: Token![=] = input.parse()?;

            match ident.to_string().as_str() {
                "trigger_type" => trigger_type = Some(input.parse()?),
                _ => return Err(input.error("Expected 'trigger_type'")),
            }
        } else if !input.is_empty() {
            return Err(input.error("Expected 'trigger_type =' or nothing"));
        }

        Ok(Self {
            script_name,
            trigger_type,
        })
    }
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
