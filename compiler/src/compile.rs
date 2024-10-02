use symtab_visitor::SymTabVisitor;

use crate::{
    ast::{Statement, Visitable},
    types::Type,
    Message,
};

mod symtab_visitor;

#[derive(Clone, Debug)]
pub struct Property {
    pub ty: Type,
    pub index: usize,
    pub name: String,
}

pub struct CompileSettings {
    pub properties: Vec<Property>,
}

pub fn compile(mut ast: Vec<Statement>, settings: &CompileSettings) -> Result<(), Message> {
    // build the symbol table
    let mut sym_tab_visitor = SymTabVisitor::new(settings);

    // resolve all the identifiers
    ast.visit(&mut sym_tab_visitor)?;

    Ok(())
}
