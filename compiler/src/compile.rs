use std::{collections::HashMap, path::Path};

use bytecode::{Type1, Type3};
use symtab_visitor::SymTabVisitor;
use type_visitor::{TypeTable, TypeVisitor};

use crate::{
    EventHandler, ExternFunction, Trigger,
    ast::{BinaryOperator, FunctionId, SymbolId},
    compile::ir::{
        BlockId, TapIr, TapIrFunction, create_ir, make_ssa,
        regalloc::{self, RegisterAllocations},
    },
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::{FileId, Span},
    types::Type,
};

use self::symtab_visitor::SymTab;

pub mod analyse;
#[cfg(test)]
mod disassemble;
mod ir;
mod loop_visitor;
pub(crate) mod symtab_visitor;
pub(crate) mod type_visitor;

use bytecode::Opcode;

#[derive(Clone, Debug)]
pub struct Property {
    pub ty: Type,
    pub index: usize,
    pub name: String,
    pub span: Span,
}

pub struct CompileSettings {
    /// Field names available in the Rust struct. If `Some`, the compiler validates
    /// that declared properties exist in this list. If `None`, validation is skipped
    /// (useful for LSP/tooling where the Rust struct info isn't available).
    pub available_fields: Option<Vec<String>>,
    pub enable_optimisations: bool,
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

    let mut sym_tab_visitor = SymTabVisitor::new(settings, &mut ast, &mut diagnostics);
    let mut type_visitor = TypeVisitor::new(
        &ast.functions,
        &ast.extern_functions,
        sym_tab_visitor.get_symtab(),
    );

    for function in &mut ast.functions {
        sym_tab_visitor.visit_function(function, &mut diagnostics);
        loop_visitor::visit_loop_check(function, &mut diagnostics);

        type_visitor.visit_function(function, sym_tab_visitor.get_symtab(), &mut diagnostics);
    }

    let type_table = type_visitor.into_type_table(sym_tab_visitor.get_symtab(), &mut diagnostics);

    if diagnostics.has_any() {
        return Err(diagnostics);
    }

    let extern_functions = ast
        .extern_functions
        .iter()
        .map(|extern_function| ExternFunction {
            name: extern_function.name.to_string(),
            arguments: extern_function
                .arguments
                .iter()
                .map(|arg| arg.t.t)
                .collect(),
            returns: extern_function
                .return_types
                .types
                .iter()
                .map(|ret| ret.t)
                .collect(),
        })
        .collect();

    let mut symtab = sym_tab_visitor.into_symtab();
    let mut compiler = Compiler::new(&type_table, extern_functions, &symtab);

    let ir_functions = ast
        .functions
        .iter()
        .map(|f| create_ir(f, &mut symtab))
        .collect::<Vec<_>>();

    let mut ir_functions = ir_functions
        .into_iter()
        .map(|mut ir_fn| {
            make_ssa(&mut ir_fn, &mut symtab);
            ir_fn
        })
        .collect::<Vec<_>>();

    ir::optimisations::optimise(&mut ir_functions, &mut symtab, settings);

    for mut function in ir_functions {
        let registers = regalloc::allocate_registers(&mut function);

        compiler.compile_function(&function, &registers);
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
    pub fn new(
        type_table: &TypeTable<'_>,
        extern_functions: Box<[ExternFunction]>,
        symtab: &SymTab,
    ) -> Self {
        Self {
            stack: vec![],

            jumps: vec![],
            function_locations: HashMap::from([(FunctionId::toplevel(), Label(0))]),
            bytecode: Bytecode::new(type_table.triggers(), extern_functions, symtab),
        }
    }

    pub fn compile_function(
        &mut self,
        function: &TapIrFunction,
        register_allocations: &RegisterAllocations,
    ) {
        let function_id = function.id();

        if !function_id.is_toplevel() {
            // the stack will be arguments, then the return pointer. However, if we're at toplevel, then
            // the stack will be empty to start with
            for argument in function.arguments() {
                self.stack.push(Some(*argument));
            }

            self.stack.push(None);
        }

        self.function_locations
            .insert(function_id, self.bytecode.new_label());

        if let Some(event_handler) = &function.modifiers().event_handler {
            self.bytecode.event_handlers.push(EventHandler {
                name: event_handler.name.clone(),
                bytecode_offset: self.bytecode.offset(),
                arguments: event_handler.arg_names.clone(),
            });
        }

        self.compile_blocks(function, register_allocations);

        // if there is no return value, then no return is required to be compiled so we should add one
        if function.return_types().is_empty() {
            self.bytecode.ret();
        }
    }

    fn compile_blocks(
        &mut self,
        function: &TapIrFunction,
        register_allocations: &RegisterAllocations,
    ) {
        // Where all the blocks start
        let mut block_entrypoints: HashMap<BlockId, Label> = HashMap::new();
        // Where all the jumps are and to which block they should go. For non-local jumps,
        // these are filled in later as function calls.
        let mut jumps: Vec<(BlockId, Jump)> = vec![];

        let v = |s: &SymbolId| register_allocations.register_for_symbol(*s).0;

        let first_argument = register_allocations.first_free_register().0;

        let put_args = |bytecode: &mut Bytecode, args: &[SymbolId], is_for_extern: bool| {
            let arg_offset = if is_for_extern {
                0
            } else {
                // first argument is actually the return location, and we should start populating arguments one off
                1
            };

            for (i, arg) in args.iter().enumerate() {
                bytecode.mov(i as u8 + first_argument + arg_offset, v(arg));
            }
        };

        for block in function.blocks() {
            let entry_point = self.bytecode.new_label();
            block_entrypoints.insert(block.id(), entry_point);

            for instr in block.instrs() {
                match instr {
                    TapIr::Constant(target, constant) => {
                        let constant = match constant {
                            ir::Constant::Int(i) => *i as u32,
                            ir::Constant::Fix(num) => num.to_raw() as u32,
                            ir::Constant::Bool(b) => *b as u32,
                        };

                        self.bytecode.constant(v(target), constant);
                    }
                    TapIr::Move { target, source } => self.bytecode.mov(v(target), v(source)),
                    TapIr::BinOp {
                        target,
                        lhs,
                        op,
                        rhs,
                    } => {
                        self.bytecode.binop(v(target), v(lhs), *op, v(rhs));
                    }
                    TapIr::Wait => self.bytecode.wait(),
                    TapIr::Call { target, f, args } => {
                        put_args(&mut self.bytecode, args, false);
                        self.bytecode.call(first_argument);
                        self.jumps.push((*f, self.bytecode.new_jump()));

                        for (i, target) in target.iter().enumerate() {
                            self.bytecode.mov(v(target), i as u8 + first_argument + 1);
                        }
                    }
                    TapIr::CallExternal { target, f, args } => {
                        put_args(&mut self.bytecode, args, true);
                        self.bytecode.call_external(f.0 as u8, first_argument);

                        for (i, target) in target.iter().enumerate() {
                            self.bytecode.mov(v(target), i as u8 + first_argument);
                        }
                    }
                    TapIr::Spawn { f, args } => {
                        put_args(&mut self.bytecode, args, false);
                        self.bytecode.spawn(first_argument, args.len() as u8);
                        self.jumps.push((*f, self.bytecode.new_jump()));
                    }
                    TapIr::Trigger { f, args } => {
                        put_args(&mut self.bytecode, args, false);
                        self.bytecode.trigger(f.0 as u8, first_argument);
                    }
                    TapIr::GetProp { target, prop_index } => {
                        self.bytecode.get_prop(v(target), *prop_index as u8);
                    }
                    TapIr::StoreProp { prop_index, value } => {
                        self.bytecode.set_prop(v(value), *prop_index as u8);
                    }
                    TapIr::GetBuiltin { target, builtin } => {
                        self.bytecode.get_builtin(v(target), builtin.id());
                    }
                    TapIr::GetGlobal {
                        target,
                        global_index,
                    } => {
                        self.bytecode.get_global(v(target), *global_index as u8);
                    }
                    TapIr::SetGlobal {
                        global_index,
                        value,
                    } => {
                        self.bytecode.set_global(v(value), *global_index as u8);
                    }
                }
            }

            match block.block_exit() {
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

pub struct BytecodeParts {
    pub bytecode: Box<[u32]>,
    pub globals: Box<[i32]>,
    pub properties: Box<[crate::PropertyInfo]>,
    pub event_handlers: Box<[EventHandler]>,
    pub triggers: Box<[Trigger]>,
    pub extern_functions: Box<[ExternFunction]>,
}

pub struct Bytecode {
    data: Vec<u32>,
    globals: Box<[i32]>,
    properties: Box<[crate::PropertyInfo]>,

    pub event_handlers: Vec<EventHandler>,
    pub triggers: Box<[Trigger]>,
    pub extern_functions: Box<[ExternFunction]>,
}

impl Bytecode {
    fn new(
        triggers: Box<[Trigger]>,
        extern_functions: Box<[ExternFunction]>,
        symtab: &SymTab,
    ) -> Self {
        let globals = symtab.globals().iter().map(|g| g.initial_value).collect();
        let properties = symtab
            .properties()
            .iter()
            .map(|p| crate::PropertyInfo {
                name: p.name.clone(),
                ty: p.ty,
                index: p.index,
                span: p.span,
            })
            .collect();

        Self {
            data: vec![],
            globals,
            properties,

            event_handlers: vec![],
            triggers,
            extern_functions,
        }
    }

    /// Finalize and decompose the bytecode into its parts
    pub fn into_parts(self) -> BytecodeParts {
        BytecodeParts {
            bytecode: self.data.into_boxed_slice(),
            globals: self.globals,
            properties: self.properties,
            event_handlers: self.event_handlers.into_boxed_slice(),
            triggers: self.triggers,
            extern_functions: self.extern_functions,
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
        if source != target {
            self.data.push(Type1::mov(target, source).encode());
        }
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

    fn call_external(&mut self, extern_id: u8, first_arg: u8) {
        self.data
            .push(Type1::extern_call(extern_id, first_arg).encode());
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

    fn get_builtin(&mut self, target: u8, id: u8) {
        self.data.push(Type1::get_builtin(target, id).encode());
    }

    fn get_global(&mut self, target: u8, global_index: u8) {
        self.data
            .push(Type1::get_global(target, global_index).encode());
    }

    fn set_global(&mut self, value: u8, global_index: u8) {
        self.data
            .push(Type1::set_global(value, global_index).encode());
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
            BinaryOperator::And | BinaryOperator::Or => {
                unreachable!("Shouldn't be compiling binary && or ||")
            }
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

            let enable_optimisations = input.starts_with("# optimise\n");

            let compiler_settings = CompileSettings {
                available_fields: None,
                enable_optimisations,
            };

            let bytecode = compile(path, &input, &compiler_settings).unwrap();

            let mut decompiled = String::new();
            disassemble::disassemble(&bytecode.data, &mut decompiled).unwrap();

            assert_snapshot!(decompiled);
        });
    }
}
