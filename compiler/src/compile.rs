use symtab_visitor::{SymTab, SymTabVisitor};
use type_visitor::TypeVisitor;

use crate::{
    ast::{self, SymbolId},
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::FileId,
    types::Type,
};

mod symtab_visitor;
mod type_visitor;

use opcodes::{MathsOp, Opcode};

#[derive(Clone, Debug)]
pub struct Property {
    pub ty: Type,
    pub index: usize,
    pub name: String,
}

pub struct CompileSettings {
    pub properties: Vec<Property>,
}

pub fn compile(input: &str, settings: &CompileSettings) -> Result<Bytecode, Diagnostics> {
    let mut diagnostics = Diagnostics::new();

    let file_id = FileId::new(0);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let mut ast = match parser.parse(file_id, &mut diagnostics, lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, file_id);
            return Err(diagnostics);
        }
    };

    // build the symbol table
    let symtab = {
        let mut sym_tab_visitor = SymTabVisitor::new(settings);

        // resolve all the identifiers
        sym_tab_visitor.visit(&mut ast, &mut diagnostics);

        sym_tab_visitor.into_symtab()
    };

    let _type_table = {
        let mut type_visitor = TypeVisitor::new(settings);

        type_visitor.visit(&ast, &symtab, &mut diagnostics);

        type_visitor.into_type_table(&symtab, &mut diagnostics)
    };

    if diagnostics.has_any() {
        return Err(diagnostics);
    }

    let mut compiler = Compiler::new();
    compiler.compile_block(&ast, &symtab);

    Ok(compiler.bytecode)
}

struct Compiler {
    stack: Vec<Option<SymbolId>>,
    bytecode: Bytecode,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            bytecode: Bytecode::new(),
        }
    }

    pub fn compile_block(&mut self, block: &[ast::Statement], symtab: &SymTab) {
        let previous_stack_size = self.stack.len();
        for statement in block {
            self.compile_statement(statement, symtab);
        }

        self.compile_drop_to(previous_stack_size);
    }

    fn compile_statement(&mut self, statement: &ast::Statement, symtab: &SymTab) {
        match &statement.kind {
            ast::StatementKind::Error => panic!("Should never have to compile an error"),
            ast::StatementKind::VariableDeclaration { .. } => {
                panic!("Should have resolved this in symbol visiting")
            }
            ast::StatementKind::Assignment { .. } => {
                panic!("Should have resolved this in symbol visiting")
            }
            ast::StatementKind::Wait => {
                self.bytecode.add_opcode(Opcode::Wait);
            }
            ast::StatementKind::Nop => {}
            ast::StatementKind::SymbolDeclare { ident, value } => {
                self.compile_expression(value, symtab);
                self.stack.pop();
                self.stack.push(Some(*ident)); // this is now on the stack at this location
            }
            ast::StatementKind::SymbolAssign { ident, value } => {
                self.compile_expression(value, symtab);

                if let Some(property) = symtab.get_property(*ident) {
                    self.bytecode
                        .add_opcode(Opcode::SetProp(property.index as u8));
                    self.stack.pop();
                } else {
                    let offset = self.get_offset(*ident);
                    self.bytecode.add_opcode(Opcode::Move(offset as u8 - 1));
                    self.stack.pop();
                }
            }
            ast::StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                let stack_depth_before_if = self.stack.len();

                self.compile_expression(condition, symtab);
                let if_false_jump = self.bytecode.new_jump_if_false();

                self.compile_block(true_block, symtab);
                self.compile_drop_to(stack_depth_before_if);

                let if_true_jump = self.bytecode.new_jump();

                let false_target = self.bytecode.new_label();
                self.bytecode.patch_jump(if_false_jump, false_target);

                // insert a fake additional stack value for the bool that will be left
                self.stack.push(None);

                self.compile_block(false_block, symtab);

                self.compile_drop_to(stack_depth_before_if);
                let end_target = self.bytecode.new_label();
                self.bytecode.patch_jump(if_true_jump, end_target);
            }
        }
    }

    fn compile_expression(&mut self, value: &ast::Expression, symtab: &SymTab) {
        match &value.kind {
            ast::ExpressionKind::Integer(i) => {
                self.bytecode.add_opcode(Opcode::Push8(*i as i8));
                self.stack.push(None);
            }
            ast::ExpressionKind::Fix(_) => todo!("Fixnum compilation"),
            ast::ExpressionKind::Bool(value) => {
                self.bytecode
                    .add_opcode(Opcode::Push8(if *value { 1 } else { 0 }));
                self.stack.push(None);
            }
            ast::ExpressionKind::Variable(_) => {
                unreachable!("Should have resolved this in symbol visiting")
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                self.compile_expression(lhs, symtab);
                self.compile_expression(rhs, symtab);
                self.stack.pop();
                self.stack.pop();
                self.stack.push(None);

                self.bytecode
                    .add_opcode(Opcode::MathsOp(MathsOp::from(*operator)));
            }
            ast::ExpressionKind::Error => panic!("Should never have to compile an error"),
            ast::ExpressionKind::Nop => {}
            ast::ExpressionKind::Symbol(symbol_id) => {
                if let Some(property) = symtab.get_property(*symbol_id) {
                    self.bytecode
                        .add_opcode(Opcode::GetProp(property.index as u8));
                } else {
                    let offset = self.get_offset(*symbol_id);
                    self.bytecode.add_opcode(Opcode::Dup(offset as u8 - 1));
                }

                self.stack.push(Some(*symbol_id));
            }
        }
    }

    fn compile_drop_to(&mut self, desired_stack_size: usize) {
        match self.stack.len().cmp(&desired_stack_size) {
            std::cmp::Ordering::Less => {
                panic!("Trying to drop the stack to a size bigger than the current")
            }
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Greater => {
                self.bytecode
                    .add_opcode(Opcode::Drop((self.stack.len() - desired_stack_size) as u8));
                self.stack.truncate(desired_stack_size);
            }
        }
    }

    fn get_offset(&self, symbol_id: SymbolId) -> usize {
        self.stack.len()
            - self
                .stack
                .iter()
                .rposition(|stack_sym| stack_sym == &Some(symbol_id))
                .unwrap()
    }
}

pub mod opcodes {
    use std::fmt::Display;

    use serde::Serialize;

    use crate::ast::BinaryOperator;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum Opcode {
        Push8(i8),
        Push24(u32),
        Dup(u8),
        Drop(u8),
        GetProp(u8),
        SetProp(u8),
        Wait,
        Move(u8),
        MathsOp(MathsOp),
        JumpIfFalse(u16),
        Jump(u16),
    }

    impl Display for Opcode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Opcode::Push8(v) => write!(f, "push8\t{v}"),
                Opcode::Push24(v) => write!(f, "push24\t{v}"),
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
                    }
                ),
                Opcode::JumpIfFalse(target) => write!(f, "jif\t{target}"),
                Opcode::Jump(target) => write!(f, "j\t{target}"),
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

            arm!(Add, Sub, Mul, RealMod, RealDiv, EqEq)
        }
    }

    impl Opcode {
        pub fn size(self) -> usize {
            match self {
                Self::Push24(_) | Self::JumpIfFalse(_) | Self::Jump(_) => 2,
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
            }
        }
    }
}

pub struct Bytecode {
    data: Vec<Opcode>,
    length: usize,
}

impl Bytecode {
    pub fn new() -> Self {
        Self {
            data: vec![],
            length: 0,
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

    fn patch_jump(&mut self, jump: Jump, label: Label) {
        match &mut self.data[jump.0] {
            Opcode::Jump(target) | Opcode::JumpIfFalse(target) => *target = label.0,
            opcode => panic!("Tried to patch {opcode:?} which isn't a jump"),
        }
    }

    pub fn compile(&self) -> Vec<u16> {
        let mut result = Vec::with_capacity(self.length);

        macro_rules! one_arg {
            ($kind:ident, $value:expr) => {
                result.push(((bytecode::Instruction::$kind as u16) << 8) | $value as u16);
            };
        }

        for opcode in &self.data {
            match *opcode {
                Opcode::Push8(value) => {
                    one_arg!(Push8, value);
                }
                Opcode::Push24(value) => {
                    let bytes = value.to_be_bytes();
                    one_arg!(Push24, bytes[1]);
                    result.push(u16::from_be_bytes(bytes[2..].try_into().unwrap()));
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
            }
        }

        result
    }
}

struct Label(u16);
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
            };

            let bytecode = compile(&input, &compiler_settings).unwrap();
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
