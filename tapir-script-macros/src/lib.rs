use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn tapir_script(attr: TokenStream, item: TokenStream) -> TokenStream {
    tapir_script_macros_core::tapir_script(attr.into(), item.into()).into()
}
