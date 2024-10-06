use symtab_visitor::{SymTab, SymTabVisitor};
use type_visitor::TypeVisitor;

use crate::{
    ast::{self, Statement, SymbolId},
    types::Type,
    Message,
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

pub fn compile(mut ast: Vec<Statement>, settings: &CompileSettings) -> Result<Bytecode, Message> {
    // build the symbol table
    let symtab = {
        let mut sym_tab_visitor = SymTabVisitor::new(settings);

        // resolve all the identifiers
        sym_tab_visitor.visit(&mut ast);

        sym_tab_visitor.into_symtab()
    };

    let type_table = {
        let mut type_visitor = TypeVisitor::new(settings);

        type_visitor.visit(&ast, &symtab)?;

        type_visitor.into_type_table(&symtab)?
    };

    let mut compiler = Compiler::new();

    for statement in ast {
        compiler.compile_statement(&statement, &symtab)?;
    }

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

    pub fn compile_statement(
        &mut self,
        statement: &ast::Statement<'_>,
        symtab: &SymTab,
    ) -> Result<(), Message> {
        match &statement.kind {
            ast::StatementKind::Error(message) => return Err(message.clone()),
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
                self.compile_expression(value, symtab)?;
                self.stack.pop();
                self.stack.push(Some(*ident)); // this is now on the stack at this location
            }
            ast::StatementKind::SymbolAssign { ident, value } => {
                self.compile_expression(value, symtab)?;

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
        }

        Ok(())
    }

    fn compile_expression(
        &mut self,
        value: &ast::Expression<'_>,
        symtab: &SymTab,
    ) -> Result<(), Message> {
        match &value.kind {
            ast::ExpressionKind::Integer(i) => {
                self.bytecode.add_opcode(Opcode::Push8(*i as i8 as u8));
                self.stack.push(None);
            }
            ast::ExpressionKind::Fix(num) => todo!(),
            ast::ExpressionKind::Variable(_) => {
                unreachable!("Should have resolved this in symbol visiting")
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                self.compile_expression(lhs, symtab)?;
                self.compile_expression(rhs, symtab)?;
                self.stack.pop();
                self.stack.pop();
                self.stack.push(None);

                self.bytecode
                    .add_opcode(Opcode::MathsOp(MathsOp::from(*operator)));
            }
            ast::ExpressionKind::Error(message) => return Err(message.clone()),
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

        Ok(())
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
    use serde::Serialize;

    use crate::ast::BinaryOperator;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum Opcode {
        Push8(u8),
        Push24(u32),
        Dup(u8),
        Drop(u8),
        GetProp(u8),
        SetProp(u8),
        Nop,
        Wait,
        Move(u8),
        MathsOp(MathsOp),
        JumpIfFalse(u16),
        Jump(u16),
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum MathsOp {
        Add,
        Sub,
        Mul,
        RShiftConst,
        LShiftConst,
        RealMod,
        RealDiv,
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

    pub fn new_label(&mut self) -> Label {
        Label(self.length as u16)
    }

    pub fn new_jump(&mut self) -> Jump {
        self.add_opcode(Opcode::Jump(0));
        Jump(self.length - 1)
    }

    pub fn patch_jump(&mut self, jump: Jump, label: Label) {
        match &mut self.data[jump.0] {
            Opcode::Jump(target) | Opcode::JumpIfFalse(target) => *target = label.0,
            opcode => panic!("Tried to patch {opcode:?} which isn't a jump"),
        }
    }

    pub fn get_opcodes(&self) -> &[Opcode] {
        &self.data
    }
}

struct Label(u16);
struct Jump(usize);

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, glob};

    use crate::{grammar, lexer::Lexer, tokens::FileId};

    use super::*;

    #[test]
    fn compiler_snapshot_tests() {
        glob!("snapshot_tests", "compiler/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();

            let ast = parser.parse(FileId::new(0), lexer).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
            };

            let bytecode = compile(ast, &compiler_settings).unwrap();

            assert_ron_snapshot!(bytecode.get_opcodes());
        });
    }
}
