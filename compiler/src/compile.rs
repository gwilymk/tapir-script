use std::{collections::HashMap, path::Path};

use bytecode::{Type1, Type3};
use optimisations::UnusedFunction;
use symtab_visitor::SymTabVisitor;
use type_visitor::{TypeTable, TypeVisitor};

use crate::{
    ast::{FunctionId, SymbolId},
    compile::ir::{create_ir, BlockId, TapIrFunction},
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::FileId,
    types::Type,
    EventHandler, Trigger,
};

mod ir;
mod loop_visitor;
mod optimisations;
mod symtab_visitor;
mod type_visitor;

use bytecode::Opcode;

#[derive(Clone, Debug)]
pub struct Property {
    pub ty: Type,
    pub index: usize,
    pub name: String,
}

pub struct CompileSettings {
    pub properties: Vec<Property>,
    pub enable_optimisations: bool,
}

impl CompileSettings {
    pub(crate) fn is_property(&self, symbol_id: SymbolId) -> bool {
        symbol_id.0 < self.properties.len()
    }

    pub(crate) fn property_symbols(&self) -> impl Iterator<Item = SymbolId> {
        (0..self.properties.len()).map(SymbolId)
    }
}

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> Result<Bytecode, Diagnostics> {
    let file_id = FileId::new(0);

    let mut diagnostics = Diagnostics::new(file_id, filename, input);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let mut ast = match parser.parse(file_id, &mut diagnostics, lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, file_id);
            return Err(diagnostics);
        }
    };

    let mut sym_tab_visitor = SymTabVisitor::new(settings, &mut ast.functions, &mut diagnostics);
    let mut type_visitor = TypeVisitor::new(settings, &ast.functions);

    for function in &mut ast.functions {
        sym_tab_visitor.visit_function(function, &mut diagnostics);
        loop_visitor::visit_loop_check(function, &mut diagnostics);

        type_visitor.visit_function(function, sym_tab_visitor.get_symtab(), &mut diagnostics);
    }

    optimisations::optimise(&mut ast.functions, settings, &mut diagnostics);

    let type_table = type_visitor.into_type_table(sym_tab_visitor.get_symtab(), &mut diagnostics);

    if diagnostics.has_any() {
        return Err(diagnostics);
    }

    let mut compiler = Compiler::new(&type_table);
    let mut symtab = sym_tab_visitor.into_symtab();

    let ir_functions = ast
        .functions
        .iter()
        .filter(|f| !f.meta.has::<UnusedFunction>())
        .map(|f| create_ir(f, &mut symtab))
        .collect::<Vec<_>>();

    for function in ir_functions {
        compiler.compile_function(&function);
    }

    compiler.finalise();

    Ok(compiler.bytecode)
}

struct Compiler {
    stack: Vec<Option<SymbolId>>,

    function_locations: HashMap<FunctionId, Label>,
    jumps: Vec<(FunctionId, Jump)>,

    bytecode: Bytecode,
}

impl Compiler {
    pub fn new(type_table: &TypeTable<'_>) -> Self {
        Self {
            stack: vec![],

            jumps: vec![],
            function_locations: HashMap::from([(FunctionId(0), Label(0))]),
            bytecode: Bytecode::new(type_table.triggers()),
        }
    }

    pub fn compile_function(&mut self, function: &TapIrFunction) {
        let function_id = function.id;

        if function_id != FunctionId(0) {
            // the stack will be arguments, then the return pointer. However, if we're at toplevel, then
            // the stack will be empty to start with
            for argument in &function.arguments {
                self.stack.push(Some(*argument));
            }

            self.stack.push(None);
        }

        self.function_locations
            .insert(function_id, self.bytecode.new_label());

        if let Some(event_handler) = &function.modifiers.event_handler {
            self.bytecode.event_handlers.push(EventHandler {
                name: event_handler.name.clone(),
                bytecode_offset: self.bytecode.offset(),
                arguments: event_handler.arg_names.clone(),
            });
        }

        self.compile_blocks(function);

        // if there is no return value, then no return is required to be compiled so we should add one
        if function.return_types.is_empty() {
            self.bytecode.ret();
        }
    }

    fn compile_blocks(&mut self, function: &TapIrFunction) {
        // Where all the blocks start
        let mut block_entrypoints: HashMap<BlockId, Label> = HashMap::new();
        // Where all the jumps are and to which block they should go. For non-local jumps,
        // these are filled in later as function calls.
        let mut jumps: Vec<(BlockId, Jump)> = vec![];

        for block in &function.blocks {
            let entry_point = self.bytecode.new_label();
            block_entrypoints.insert(block.id, entry_point);
        }
    }

    fn finalise(&mut self) {}
}

pub struct Bytecode {
    pub data: Vec<u32>,

    pub event_handlers: Vec<EventHandler>,
    pub triggers: Box<[Trigger]>,
}

impl Bytecode {
    pub fn new(triggers: Box<[Trigger]>) -> Self {
        Self {
            data: vec![],

            event_handlers: vec![],
            triggers,
        }
    }

    pub fn offset(&self) -> usize {
        self.data.len()
    }

    pub fn new_label(&self) -> Label {
        Label(self.data.len())
    }

    pub fn new_jump(&mut self) -> Jump {
        self.data.push(Type3::invalid_jump().encode());
        Jump(self.data.len() - 1)
    }

    pub fn patch_jump(&mut self, jump: Jump, label: Label) {
        assert_eq!(
            bytecode::opcode(self.data[jump.0]),
            Some(Opcode::Jump),
            "Trying to patch a non-jump instruction"
        );

        self.data[jump.0] =
            Type3::jump(label.0.try_into().expect("Offset bigger than allowed")).encode();
    }
}

impl Bytecode {
    pub fn ret(&mut self) {
        self.data.push(Type1::ret().encode());
    }
}

// The internal usize here is the index in the bytecode where this label sits
#[derive(Clone, Copy)]
struct Label(usize);

// The internal usize here is the index where the jump instruction is in the bytecode
#[derive(Clone, Copy)]
struct Jump(usize);
