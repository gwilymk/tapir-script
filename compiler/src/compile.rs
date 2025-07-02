use std::{collections::HashMap, path::Path};

use optimisations::UnusedFunction;
use symtab_visitor::SymTabVisitor;
use type_visitor::{TypeTable, TypeVisitor};

use crate::{
    ast::{FunctionId, SymbolId},
    compile::ir::{create_ir, TapIrFunction},
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

use opcodes::Opcode;

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

    function_calls: Vec<(FunctionId, Jump)>,
    function_locations: HashMap<FunctionId, Label>,

    bytecode: Bytecode,
}

impl Compiler {
    pub fn new(type_table: &TypeTable<'_>) -> Self {
        Self {
            stack: vec![],

            function_calls: vec![],
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
                bytecode_offset: self.bytecode.length,
                arguments: event_handler.arg_names.clone(),
            });
        }

        self.compile_blocks(function);

        // if there is no return value, then no return is required to be compiled so we should add one
        if function.return_types.is_empty() {
            self.bytecode.add_opcode(Opcode::Return {
                args: function.arguments.len() as u8,
                rets: 0,
                shift: 0, // can be zero because we will have dropped after the block was compiled
            });
        }
    }

    fn compile_blocks(&mut self, function: &TapIrFunction) {
        for block in &function.blocks {}
    }

    fn finalise(&mut self) {
        for (fname, jump) in &self.function_calls {
            let label = self.function_locations[fname];
            self.bytecode.patch_jump(*jump, label);
        }
    }
}

pub mod opcodes {
    use std::fmt::Display;

    use serde::Serialize;

    use crate::ast::BinaryOperator;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum Opcode {
        Push8(i8),
        Push32(i32),
        Dup(u8),
        Drop(u8),
        GetProp(u8),
        SetProp(u8),
        Wait,
        Move(u8),
        MathsOp(MathsOp),
        JumpIfFalse(u16),
        Jump(u16),
        Call(u16),
        Spawn { args: u8, target: u16 },
        Return { args: u8, rets: u8, shift: u8 },
        Trigger(u8),
    }

    impl Display for Opcode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Opcode::Push8(v) => write!(f, "push8\t{v}"),
                Opcode::Push32(v) => write!(f, "push32\t{v}"),
                Opcode::Dup(v) => write!(f, "dup\t{v}"),
                Opcode::Drop(v) => write!(f, "drop\t{v}"),
                Opcode::GetProp(i) => write!(f, "getprop\t{i}"),
                Opcode::SetProp(i) => write!(f, "setprop\t{i}"),
                Opcode::Wait => write!(f, "wait"),
                Opcode::Move(i) => write!(f, "move\t{i}"),
                Opcode::MathsOp(maths_op) => write!(
                    f,
                    "{}",
                    match maths_op {
                        MathsOp::Add => "add",
                        MathsOp::Sub => "sub",
                        MathsOp::Mul => "mul",
                        MathsOp::RealMod => "realmod",
                        MathsOp::RealDiv => "realdiv",
                        MathsOp::EqEq => "==",
                        MathsOp::NeEq => "!=",
                        MathsOp::Gt => ">",
                        MathsOp::GtEq => ">=",
                        MathsOp::Lt => "<",
                        MathsOp::LtEq => "<=",
                        MathsOp::FixMul => "fmul",
                        MathsOp::FixDiv => "fdiv",
                    }
                ),
                Opcode::JumpIfFalse(target) => write!(f, "jif\t{target}"),
                Opcode::Jump(target) => write!(f, "j\t{target}"),
                Opcode::Call(target) => write!(f, "call\t{target}"),
                Opcode::Return { args, rets, shift } => {
                    write!(f, "ret\targs={args} rets={rets} shift={shift}")
                }
                Opcode::Spawn { args, target } => write!(f, "spawn\t{args} {target}"),
                Opcode::Trigger(index) => write!(f, "trigger\t{index}"),
            }
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum MathsOp {
        Add,
        Sub,
        Mul,
        RealMod,
        RealDiv,

        EqEq,
        NeEq,
        Gt,
        GtEq,
        Lt,
        LtEq,
        FixMul,
        FixDiv,
    }

    impl From<MathsOp> for bytecode::MathsOp {
        fn from(value: MathsOp) -> Self {
            macro_rules! arm {
                ($($kind:ident),*) => {
                    match value {
                        $(
                            MathsOp::$kind => bytecode::MathsOp::$kind,
                        )*
                    }
                };
            }

            arm!(Add, Sub, Mul, RealMod, RealDiv, EqEq, NeEq, Gt, GtEq, Lt, LtEq, FixMul, FixDiv)
        }
    }

    impl Opcode {
        pub fn size(self) -> usize {
            match self {
                Self::JumpIfFalse(_)
                | Self::Jump(_)
                | Self::Call(_)
                | Self::Return { .. }
                | Self::Spawn { .. } => 2,
                Self::Push32(_) => 3,
                _ => 1,
            }
        }
    }

    impl From<BinaryOperator> for MathsOp {
        fn from(value: BinaryOperator) -> Self {
            use MathsOp::*;

            match value {
                BinaryOperator::Add => Add,
                BinaryOperator::Sub => Sub,
                BinaryOperator::Mul => Mul,
                BinaryOperator::Div => panic!("Shouldn't be compiling div binops"),
                BinaryOperator::Mod => panic!("Shouldn't be compiling mod binops"),
                BinaryOperator::RealDiv => RealDiv,
                BinaryOperator::RealMod => RealMod,
                BinaryOperator::EqEq => EqEq,
                BinaryOperator::NeEq => NeEq,
                BinaryOperator::Gt => Gt,
                BinaryOperator::GtEq => GtEq,
                BinaryOperator::Lt => Lt,
                BinaryOperator::LtEq => LtEq,
                BinaryOperator::FixMul => FixMul,
                BinaryOperator::FixDiv => FixDiv,
                BinaryOperator::Then => panic!("Shouldn't be compiling then binops"),
            }
        }
    }
}

pub struct Bytecode {
    data: Vec<Opcode>,
    /// opcodes have different sizes, this is the current size of the _compiled_ code
    length: usize,

    pub event_handlers: Vec<EventHandler>,
    pub triggers: Vec<Trigger>,
}

impl Bytecode {
    pub fn new(triggers: Vec<Trigger>) -> Self {
        Self {
            data: vec![],
            length: 0,
            event_handlers: vec![],
            triggers,
        }
    }

    pub fn add_opcode(&mut self, opcode: Opcode) {
        self.data.push(opcode);
        self.length += opcode.size();
    }

    fn new_label(&mut self) -> Label {
        Label(
            self.length
                .try_into()
                .expect("Offset bigger than 16 bit maximum"),
        )
    }

    fn new_jump(&mut self) -> Jump {
        self.add_opcode(Opcode::Jump(0));
        Jump(self.data.len() - 1)
    }

    fn new_jump_if_false(&mut self) -> Jump {
        self.add_opcode(Opcode::JumpIfFalse(0));
        Jump(self.data.len() - 1)
    }

    fn new_call(&mut self) -> Jump {
        self.add_opcode(Opcode::Call(0));
        Jump(self.data.len() - 1)
    }

    fn new_spawn(&mut self, args: u8) -> Jump {
        self.add_opcode(Opcode::Spawn { args, target: 0 });
        Jump(self.data.len() - 1)
    }

    fn patch_jump(&mut self, jump: Jump, label: Label) {
        match &mut self.data[jump.0] {
            Opcode::Jump(target)
            | Opcode::JumpIfFalse(target)
            | Opcode::Call(target)
            | Opcode::Spawn { target, .. } => *target = label.0,
            opcode => panic!("Tried to patch {opcode:?} which isn't a jump"),
        }
    }

    pub fn compile(&self) -> Vec<u16> {
        let mut result = Vec::with_capacity(self.length);

        macro_rules! one_arg {
            ($kind:ident, $value:expr) => {
                result.push(((bytecode::Instruction::$kind as u16) << 8) | (($value as u8) as u16));
            };
        }

        for opcode in &self.data {
            match *opcode {
                Opcode::Push8(value) => {
                    one_arg!(Push8, value);
                }
                Opcode::Push32(value) => {
                    one_arg!(Push32, 0);
                    let bytes = value.to_le_bytes();
                    result.push(u16::from_le_bytes([bytes[0], bytes[1]]));
                    result.push(u16::from_le_bytes([bytes[2], bytes[3]]));
                }
                Opcode::Dup(amount) => {
                    one_arg!(Dup, amount);
                }
                Opcode::Drop(amount) => {
                    one_arg!(Drop, amount);
                }
                Opcode::GetProp(index) => {
                    one_arg!(GetProp, index);
                }
                Opcode::SetProp(index) => {
                    one_arg!(SetProp, index);
                }
                Opcode::Wait => {
                    one_arg!(Wait, 0);
                }
                Opcode::Move(amount) => {
                    one_arg!(Move, amount);
                }
                Opcode::MathsOp(op) => {
                    one_arg!(MathsOp, bytecode::MathsOp::from(op));
                }
                Opcode::JumpIfFalse(target) => {
                    one_arg!(JumpIfFalse, 0);
                    result.push(target);
                }
                Opcode::Jump(target) => {
                    one_arg!(Jump, 0);
                    result.push(target);
                }
                Opcode::Call(target) => {
                    one_arg!(Call, 0);
                    result.push(target);
                }
                Opcode::Spawn { args, target } => {
                    one_arg!(Spawn, args);
                    result.push(target);
                }
                Opcode::Return { args, rets, shift } => {
                    one_arg!(Return, args);
                    result.push(u16::from_be_bytes([rets, shift]));
                }
                Opcode::Trigger(index) => {
                    one_arg!(Trigger, index);
                }
            }
        }

        result
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Label(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Jump(usize);

#[cfg(test)]
mod test {
    use std::{fmt::Write, fs};

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
            let decompiled = print_opcodes(&bytecode.data);

            assert_snapshot!(decompiled);
        });
    }

    fn print_opcodes(opcodes: &[Opcode]) -> String {
        let mut result = String::new();

        let mut current_pos = 0;
        for opcode in opcodes {
            writeln!(&mut result, "{current_pos:08}: {opcode}").unwrap();
            current_pos += opcode.size();
        }

        result
    }
}
