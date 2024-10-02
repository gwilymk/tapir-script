use symtab_visitor::SymTabVisitor;
use type_visitor::TypeVisitor;

use crate::{
    ast::{Statement, Visitable},
    types::Type,
    Message,
};

mod symtab_visitor;
mod type_visitor;

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
    let symtab = {
        let mut sym_tab_visitor = SymTabVisitor::new(settings);

        // resolve all the identifiers
        ast.visit(&mut sym_tab_visitor)?;

        sym_tab_visitor.into_symtab()
    };

    let type_table = {
        let mut type_visitor = TypeVisitor::new(settings);

        type_visitor.visit(&ast, &symtab)?;

        type_visitor.into_type_table(&symtab)?
    };

    Ok(())
}
