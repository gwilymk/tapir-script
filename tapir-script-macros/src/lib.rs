#![deny(clippy::all)]
use proc_macro::TokenStream;

#[proc_macro_derive(TapirScript, attributes(tapir))]
pub fn tapir_script(struct_def: TokenStream) -> TokenStream {
    tapir_script_macros_core::tapir_script_derive(struct_def.into()).into()
}

#[proc_macro_derive(ConvertBetweenTapir)]
pub fn convert_between_tapir(struct_def: TokenStream) -> TokenStream {
    tapir_script_macros_core::convert_between_tapir_derive(struct_def.into()).into()
}
