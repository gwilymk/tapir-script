use std::{collections::HashMap, path::Path};

use bytecode::{Type1, Type3};
use optimisations::UnusedFunction;
use symtab_visitor::SymTabVisitor;
use type_visitor::{TypeTable, TypeVisitor};

use crate::{
    ast::{BinaryOperator, FunctionId, SymbolId},
    compile::ir::{create_ir, BlockId, TapIrFunction, TapIrInstr},
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::FileId,
    types::Type,
    EventHandler, Trigger,
};

#[cfg(test)]
mod disassemble;
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

    Ok(compiler.finalise())
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

        let mut symbols: Vec<_> = function.symbols().iter().copied().collect();
        symbols.sort_by_key(|sym| sym.0);
        assert!(
            symbols.len() < 200,
            "Need fewer than 200 symbols in a function, but got {}",
            symbols.len()
        );
        let var_locations = symbols
            .iter()
            .enumerate()
            .map(|(i, sym)| {
                if let Some(i) = function.arguments.iter().position(|arg| arg == sym) {
                    (*sym, i as u8 + 1)
                } else {
                    (*sym, i as u8 + function.arguments.len() as u8 + 1)
                }
            })
            .collect::<HashMap<_, _>>();

        let v = move |s: &SymbolId| *var_locations.get(s).unwrap();

        let first_argument = symbols.len() as u8 + function.arguments.len() as u8 + 1;
        let put_args = |bytecode: &mut Bytecode, args: &[SymbolId]| {
            for (i, arg) in args.iter().enumerate() {
                bytecode.mov(i as u8 + first_argument + 1, v(arg));
            }
        };

        for block in &function.blocks {
            let entry_point = self.bytecode.new_label();
            block_entrypoints.insert(block.id, entry_point);

            for instr in &block.instrs {
                match &instr.instr {
                    TapIrInstr::Constant(target, constant) => {
                        let constant = match constant {
                            ir::Constant::Int(i) => *i as u32,
                            ir::Constant::Fix(num) => num.to_raw() as u32,
                            ir::Constant::Bool(b) => *b as u32,
                        };

                        self.bytecode.constant(v(target), constant);
                    }
                    TapIrInstr::Move { target, source } => self.bytecode.mov(v(target), v(source)),
                    TapIrInstr::BinOp {
                        target,
                        lhs,
                        op,
                        rhs,
                    } => {
                        self.bytecode.binop(v(target), v(lhs), *op, v(rhs));
                    }
                    TapIrInstr::Wait => self.bytecode.wait(),
                    TapIrInstr::Call { target, f, args } => {
                        put_args(&mut self.bytecode, args);
                        self.bytecode.call(first_argument);
                        self.jumps.push((*f, self.bytecode.new_jump()));

                        for (i, target) in target.iter().enumerate() {
                            self.bytecode.mov(v(target), i as u8 + first_argument + 1);
                        }
                    }
                    TapIrInstr::Spawn { f, args } => {
                        put_args(&mut self.bytecode, args);
                        self.bytecode.spawn(first_argument, args.len() as u8);
                        self.jumps.push((*f, self.bytecode.new_jump()));
                    }
                    TapIrInstr::Trigger { f, args } => {
                        put_args(&mut self.bytecode, args);
                        self.bytecode.trigger(f.0 as u8, first_argument);
                    }
                    TapIrInstr::GetProp { target, prop_index } => {
                        self.bytecode.get_prop(v(target), *prop_index as u8);
                    }
                    TapIrInstr::StoreProp { prop_index, value } => {
                        self.bytecode.set_prop(v(value), *prop_index as u8);
                    }
                }
            }

            match &block.block_exit {
                ir::BlockExitInstr::JumpToBlock(block_id) => {
                    let jump = self.bytecode.new_jump();
                    jumps.push((*block_id, jump));
                }
                ir::BlockExitInstr::ConditionalJump {
                    test,
                    if_true,
                    if_false,
                } => {
                    self.bytecode.jump_if(v(test));
                    let true_jump = self.bytecode.new_jump();
                    let false_jump = self.bytecode.new_jump();

                    jumps.extend([(*if_true, true_jump), (*if_false, false_jump)]);
                }
                ir::BlockExitInstr::Return(symbol_ids) => {
                    for (i, symbol) in symbol_ids.iter().enumerate() {
                        self.bytecode.mov(i as u8 + 1, v(symbol));
                    }

                    self.bytecode.ret();
                }
            }
        }

        for (block_id, jump) in jumps {
            let label = block_entrypoints
                .get(&block_id)
                .expect("Should jump to a valid block");

            self.bytecode.patch_jump(jump, *label);
        }
    }

    fn finalise(mut self) -> Bytecode {
        for (function_id, jump) in self.jumps {
            let label = self
                .function_locations
                .get(&function_id)
                .expect("Should have defined a function");
            self.bytecode.patch_jump(jump, *label);
        }

        self.bytecode
    }
}

pub struct Bytecode {
    pub data: Vec<u32>,

    pub event_handlers: Vec<EventHandler>,
    pub triggers: Box<[Trigger]>,
}

impl Bytecode {
    fn new(triggers: Box<[Trigger]>) -> Self {
        Self {
            data: vec![],

            event_handlers: vec![],
            triggers,
        }
    }

    fn offset(&self) -> usize {
        self.data.len()
    }

    fn new_label(&self) -> Label {
        Label(self.data.len())
    }

    fn new_jump(&mut self) -> Jump {
        self.data.push(Type3::invalid_jump().encode());
        Jump(self.data.len() - 1)
    }

    fn patch_jump(&mut self, jump: Jump, label: Label) {
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
    fn ret(&mut self) {
        self.data.push(Type1::ret().encode());
    }

    fn mov(&mut self, target: u8, source: u8) {
        self.data.push(Type1::mov(target, source).encode());
    }

    fn wait(&mut self) {
        self.data.push(Type1::wait().encode());
    }

    fn constant(&mut self, target: u8, value: u32) {
        self.data.push(Type1::constant(target).encode());
        self.data.push(value);
    }

    fn call(&mut self, first_arg: u8) {
        self.data.push(Type1::call(first_arg).encode());
    }

    fn spawn(&mut self, first_arg: u8, num_args: u8) {
        self.data.push(Type1::spawn(first_arg, num_args).encode());
    }

    fn trigger(&mut self, id: u8, first_arg: u8) {
        self.data.push(Type1::trigger(id, first_arg).encode());
    }

    fn jump_if(&mut self, target: u8) {
        self.data.push(Type1::jump_if(target).encode());
    }

    fn get_prop(&mut self, target: u8, prop_index: u8) {
        self.data.push(Type1::get_prop(target, prop_index).encode());
    }

    fn set_prop(&mut self, target: u8, prop_index: u8) {
        self.data.push(Type1::set_prop(target, prop_index).encode());
    }

    fn binop(&mut self, target: u8, lhs: u8, binop: BinaryOperator, rhs: u8) {
        let opcode = match binop {
            BinaryOperator::Add => Opcode::Add,
            BinaryOperator::Sub => Opcode::Sub,
            BinaryOperator::Mul => Opcode::Mul,
            BinaryOperator::Div => unreachable!("Shouldn't be compiling div binops"),
            BinaryOperator::Mod => unreachable!("Shouldn't be compiling mod binops"),
            BinaryOperator::RealDiv => Opcode::RealDiv,
            BinaryOperator::RealMod => Opcode::RealMod,
            BinaryOperator::FixMul => Opcode::FixMul,
            BinaryOperator::FixDiv => Opcode::FixDiv,
            BinaryOperator::EqEq => Opcode::EqEq,
            BinaryOperator::NeEq => Opcode::NeEq,
            BinaryOperator::Gt => Opcode::Gt,
            BinaryOperator::GtEq => Opcode::GtEq,
            BinaryOperator::Lt => Opcode::Lt,
            BinaryOperator::LtEq => Opcode::LtEq,
            BinaryOperator::Then => unreachable!("Shouldn't be compiling then binops"),
        };

        self.data
            .push(Type1::binop(opcode, target, lhs, rhs).encode());
    }
}

// The internal usize here is the index in the bytecode where this label sits
#[derive(Clone, Copy)]
struct Label(usize);

// The internal usize here is the index where the jump instruction is in the bytecode
#[derive(Clone, Copy)]
struct Jump(usize);

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_snapshot, glob};

    use super::*;

    #[test]
    fn compiler_snapshot_tests() {
        glob!("snapshot_tests", "compiler/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
                enable_optimisations: false,
            };

            let bytecode = compile(path, &input, &compiler_settings).unwrap();

            let mut decompiled = String::new();
            disassemble::disassemble(&bytecode.data, &mut decompiled).unwrap();

            assert_snapshot!(decompiled);
        });
    }
}
