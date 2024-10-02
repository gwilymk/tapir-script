use std::collections::HashMap;

use crate::{
    ast::{Statement, Visitable, Visitor},
    types::Type,
    Message,
};

pub struct Property {
    pub ty: Type,
    pub index: usize,
}

pub struct CompileSettings {
    pub properties: HashMap<String, Property>,
}

pub fn compile(mut ast: Vec<Statement>, settings: &CompileSettings) -> Result<(), Message> {
    // build the symbol table
    let mut sym_tab_visitor = SymTabVisitor::new(settings);

    // resolve all the identifiers
    ast.visit(&mut sym_tab_visitor)?;

    Ok(())
}

struct SymbolId(usize);

struct SymTabVisitor<'a> {
    properties: &'a HashMap<String, Property>,

    symbols: HashMap<String, SymbolId>,
}

impl<'a> SymTabVisitor<'a> {
    pub fn new(settings: &'a CompileSettings) -> Self {
        Self {
            properties: &settings.properties,
            symbols: HashMap::new(),
        }
    }
}

impl<'a> Visitor<()> for SymTabVisitor<'a> {}
