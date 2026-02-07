#![deny(clippy::all)]
use std::{
    env, fs,
    path::{Path, PathBuf},
};

use compiler::CompileSettings;
use heck::ToUpperCamelCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse2, DeriveInput, Ident, LitStr, Token};

pub fn tapir_script_derive(struct_def: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse2(struct_def).unwrap();

    let syn::Data::Struct(data) = &ast.data else {
        panic!("Can only be defined on structs");
    };

    let script_info = get_script_path(&ast);
    let reduced_filename = script_info.path;
    let trigger_type = script_info.trigger_type;
    let event_type = script_info.event_type;

    let file_content = fs::read_to_string(&reduced_filename)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {e}", reduced_filename.display()));

    let available_fields = if let syn::Fields::Named(named) = &data.fields {
        Some(extract_field_names(named))
    } else {
        None
    };

    let compiled_content = match compiler::compile(
        &reduced_filename,
        &file_content,
        CompileSettings {
            available_fields,
            enable_optimisations: true,
            enable_prelude: true,
            has_event_type: event_type.is_some(),
        },
    ) {
        Ok(mut content) => {
            // Print any warnings
            if content.warnings.has_any() {
                eprintln!("{}", content.warnings.pretty_string(true));
            }
            content
        }
        Err(mut diagnostics) => {
            eprintln!("{}", diagnostics.pretty_string(true));
            panic!("Compile error");
        }
    };

    if !compiled_content.triggers.is_empty() && trigger_type.is_none() {
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

            let arg_names: Vec<_> = (0..trigger.arguments.len())
                .map(|i| format_ident!("arg{i}"))
                .collect();

            let definitions: Vec<_> = arg_names
                .iter()
                .map(|arg_name| {
                    quote! { let #arg_name = ::tapir_script::__private::read_arg(stack, &mut __offset); }
                })
                .collect();

            let trigger_index = trigger_index as u8;

            let args = if arg_names.is_empty() {
                quote! {}
            } else {
                quote! { (#(#arg_names,)*) }
            };

            quote! {
                #trigger_index => {
                    let mut __offset = 0;
                    #(#definitions)*

                    #trigger_type::#ident #args
                }
            }
        });

    let (setters, getters): (Vec<_>, Vec<_>) = compiled_content
        .properties
        .iter()
        .map(|property| {
            let index = property.index as u8;

            if let Some(ref struct_info) = property.struct_info {
                // Struct property - use ConvertBetweenTapir trait
                let field_ident = format_ident!("{}", struct_info.rust_field_name);
                let tuple_position = struct_info.tuple_position;
                let total_fields = struct_info.total_fields();

                // Generate getter - convert struct to i32 buffer, return value at position
                let getter = quote! {
                    #index => {
                        let mut buffer = [0i32; #total_fields];
                        ::tapir_script::ConvertBetweenTapir::write_to_tapir(&self.#field_ident, &mut buffer);
                        buffer[#tuple_position]
                    }
                };

                // Generate setter - convert to buffer, modify value, reconstruct
                let setter = quote! {
                    #index => {
                        let mut buffer = [0i32; #total_fields];
                        ::tapir_script::ConvertBetweenTapir::write_to_tapir(&self.#field_ident, &mut buffer);
                        buffer[#tuple_position] = value;
                        self.#field_ident = ::tapir_script::ConvertBetweenTapir::read_from_tapir(&buffer);
                    }
                };

                (setter, getter)
            } else {
                // Scalar property - use TapirProperty trait
                let field_ident = format_ident!("{}", property.name);

                let setter = quote! {
                    #index => { ::tapir_script::TapirProperty::set_from_i32(&mut self.#field_ident, value); }
                };
                let getter = quote! {
                    #index => ::tapir_script::TapirProperty::to_i32(&self.#field_ident)
                };

                (setter, getter)
            }
        })
        .unzip();

    let extern_functions = compiled_content.extern_functions.iter().enumerate().map(
        |(extern_fn_index, extern_function)| {
            let fn_name_ident = format_ident!("{}", extern_function.name);

            // Generate argument reading code - uses type inference from function signature
            let arg_idents: Vec<_> = (0..extern_function.arguments.len())
                .map(|i| format_ident!("arg{}", i))
                .collect();

            let arg_definitions: Vec<_> = arg_idents
                .iter()
                .map(|arg_ident| {
                    quote! {
                        let #arg_ident = ::tapir_script::__private::read_arg(stack, &mut __offset);
                    }
                })
                .collect();

            let args = quote! { (#(#arg_idents,)*) };

            // Generate return value writing code
            let ret_idents: Vec<_> = (0..extern_function.returns.len())
                .map(|i| format_ident!("ret{}", i))
                .collect();

            let ret_handling: Vec<_> = ret_idents
                .iter()
                .map(|ret_ident| {
                    quote! {
                        ::tapir_script::__private::write_ret(&#ret_ident, stack, &mut __ret_offset);
                    }
                })
                .collect();

            let function_call = if ret_idents.is_empty() {
                quote! { self.#fn_name_ident #args; }
            } else if ret_idents.len() == 1 {
                let ret_ident = &ret_idents[0];
                quote! { let #ret_ident = self.#fn_name_ident #args; }
            } else {
                quote! {
                    let (#(#ret_idents,)*) = self.#fn_name_ident #args;
                }
            };

            quote! {
                #extern_fn_index => {
                    let mut __offset = first_arg;
                    #(#arg_definitions)*

                    #function_call

                    let mut __ret_offset = first_arg;
                    #(#ret_handling)*
                }
            }
        },
    );

    let struct_name = ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let reduced_filename = reduced_filename.canonicalize().unwrap();
    let reduced_filename = reduced_filename.to_string_lossy();

    let bytecode = &compiled_content.bytecode;
    let globals = &compiled_content.globals;
    let event_handlers = compiled_content.event_handlers;

    let handle_event_impl = generate_handle_event(&event_handlers, &event_type);

    // Determine the EventType: use the user's type if specified, otherwise NoEventType
    let event_type_decl = event_type
        .as_ref()
        .map(|t| quote! { #t })
        .unwrap_or_else(|| quote! { ::tapir_script::NoEventType });

    quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::tapir_script::TapirScript for #struct_name #ty_generics #where_clause {
            fn script(self) -> ::tapir_script::Script<Self, Self::EventType> {
                static BYTECODE: &[u32] = &[#(#bytecode),*];
                static GLOBALS: &[i32] = &[#(#globals),*];

                ::tapir_script::Script::new(self, BYTECODE, GLOBALS)
            }

            type TriggerType = #trigger_type;
            type EventType = #event_type_decl;

            fn create_event(&self, index: u8, stack: &[i32]) -> Self::TriggerType {
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

            fn extern_call(&mut self, id: usize, stack: &mut [i32], first_arg: usize) {
                match id {
                    #(#extern_functions,)*
                    _ => unreachable!("Invalid extern function id {id}"),
                }
            }

            #handle_event_impl
        }

        const _: &[u8] = include_bytes!(#reduced_filename);
    }
}

/// Generate the `handle_event` method for the TapirScript trait.
///
/// This method converts an incoming event enum variant to stack data and bytecode offset.
/// If `event_type` is None, generates an empty match on the uninhabited `NoEventType`.
fn generate_handle_event(
    event_handlers: &[compiler::EventHandler],
    event_type: &Option<syn::Path>,
) -> TokenStream {
    let Some(event_type) = event_type else {
        // No event_type specified - generate an empty match on uninhabited type
        return quote! {
            fn handle_event(__event: Self::EventType, _: &mut ::tapir_script::__private::Vec<i32>) -> Option<usize> {
                match __event {} // empty match on uninhabited type - can never be called
            }
        };
    };

    let match_arms: Vec<_> = event_handlers
        .iter()
        .map(|handler| {
            // Convert snake_case event handler name to PascalCase variant name
            let variant_name = format_ident!("{}", handler.name.to_upper_camel_case());

            let arg_names: Vec<_> = (0..handler.arguments.len())
                .map(|i| format_ident!("arg{}", i))
                .collect();

            let stack_writes = arg_names.iter().map(|arg| {
                quote! {
                    ::tapir_script::ConvertBetweenTapir::write_to_tapir_vec(&#arg, __stack);
                }
            });

            let pc = handler.bytecode_offset;

            if arg_names.is_empty() {
                quote! {
                    #event_type::#variant_name => {
                        Some(#pc)
                    }
                }
            } else {
                quote! {
                    #event_type::#variant_name(#(#arg_names),*) => {
                        #(#stack_writes)*
                        Some(#pc)
                    }
                }
            }
        })
        .collect();

    quote! {
        fn handle_event(__event: Self::EventType, __stack: &mut ::tapir_script::__private::Vec<i32>) -> Option<usize> {
            match __event {
                #(#match_arms)*
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
    }
}

struct ScriptPathInfo {
    path: PathBuf,
    trigger_type: Option<syn::Path>,
    event_type: Option<syn::Path>,
}

fn get_script_path(ast: &DeriveInput) -> ScriptPathInfo {
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

    ScriptPathInfo {
        path: reduced_filename,
        trigger_type: top_level_args.trigger_type,
        event_type: top_level_args.event_type,
    }
}

struct TopLevelTapirArgs {
    script_name: LitStr,
    trigger_type: Option<syn::Path>,
    event_type: Option<syn::Path>,
}

impl syn::parse::Parse for TopLevelTapirArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let script_name = input.parse()?;
        let mut trigger_type = None;
        let mut event_type = None;

        while input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                break;
            }
            let ident: Ident = input.parse()?;
            let _: Token![=] = input.parse()?;

            match ident.to_string().as_str() {
                "trigger_type" => trigger_type = Some(input.parse()?),
                "event_type" => event_type = Some(input.parse()?),
                _ => return Err(input.error("Expected 'trigger_type' or 'event_type'")),
            }
        }

        if !input.is_empty() {
            return Err(input.error("Expected 'trigger_type =', 'event_type =', or nothing"));
        }

        Ok(Self {
            script_name,
            trigger_type,
            event_type,
        })
    }
}

/// Extract field names from the struct.
fn extract_field_names(named: &syn::FieldsNamed) -> Vec<String> {
    named
        .named
        .iter()
        .map(|field| field.ident.as_ref().unwrap().to_string())
        .collect()
}

/// Derive macro implementation for ConvertBetweenTapir trait.
/// Generates into_tapir and from_tapir implementations that recursively
/// convert each field.
pub fn convert_between_tapir_derive(struct_def: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse2(struct_def).unwrap();

    let syn::Data::Struct(data) = &ast.data else {
        panic!("ConvertBetweenTapir can only be derived on structs");
    };

    let syn::Fields::Named(named_fields) = &data.fields else {
        panic!("ConvertBetweenTapir requires named fields");
    };

    let struct_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let field_idents: Vec<_> = named_fields
        .named
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect();

    let field_types: Vec<_> = named_fields.named.iter().map(|f| &f.ty).collect();

    // Generate SIZE: sum of all field sizes (compile-time constant)
    let size_computation = if field_types.is_empty() {
        quote! { 0 }
    } else {
        let size_terms = field_types.iter().map(|ty| {
            quote! { <#ty as ::tapir_script::ConvertBetweenTapir>::SIZE }
        });
        quote! { #(#size_terms)+* }
    };

    // Generate write_to_tapir: write each field at its offset
    let into_tapir_writes: Vec<_> = field_idents
        .iter()
        .zip(field_types.iter())
        .map(|(ident, ty)| {
            quote! {
                ::tapir_script::ConvertBetweenTapir::write_to_tapir(&self.#ident, &mut target[index..]);
                index += <#ty as ::tapir_script::ConvertBetweenTapir>::SIZE;
            }
        })
        .collect();

    // Generate read_from_tapir: read each field at its offset
    let from_tapir_reads: Vec<_> = field_idents
        .iter()
        .zip(field_types.iter())
        .map(|(ident, ty)| {
            quote! {
                let #ident = ::tapir_script::ConvertBetweenTapir::read_from_tapir(&values[index..]);
                index += <#ty as ::tapir_script::ConvertBetweenTapir>::SIZE;
            }
        })
        .collect();

    quote! {
        #[automatically_derived]
        impl #impl_generics ::tapir_script::ConvertBetweenTapir for #struct_name #ty_generics #where_clause {
            const SIZE: usize = #size_computation;

            fn write_to_tapir(&self, target: &mut [i32]) {
                let mut index = 0;
                #(#into_tapir_writes)*
            }

            fn read_from_tapir(values: &[i32]) -> Self {
                let mut index = 0;
                #(#from_tapir_reads)*
                Self { #(#field_idents,)* }
            }
        }
    }
}
