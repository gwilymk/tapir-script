use std::borrow::Cow;

use crate::{
    ast::{self, BinaryOperator, Expression, InternalOrExternalFunctionId, MethodCallInfo, SymbolId},
    compile::{
        symtab_visitor::{FunctionArgumentSymbols, GlobalId, SymTab},
        type_visitor::{CallReturnInfo, FieldAccessInfo, FieldAssignmentInfo},
    },
    types::{StructRegistry, Type},
};

use super::{
    BlockExitInstr, BlockId, Constant, FunctionModifiers, SymbolSpans, TapIr, TapIrBlock,
    TapIrFunction,
};

pub fn create_ir(
    f: &ast::Function<'_>,
    symtab: &mut SymTab,
    struct_registry: &StructRegistry,
) -> (TapIrFunction, SymbolSpans) {
    // First, expand struct parameters so they're available during block creation
    let arg_symbols = f
        .meta
        .get::<FunctionArgumentSymbols>()
        .expect("Should have resolved arguments");

    for (arg, &symbol_id) in f.arguments.iter().zip(arg_symbols.0.iter()) {
        if let Some(ref type_with_loc) = arg.ty {
            let arg_type = type_with_loc.resolved();
            if let crate::types::Type::Struct(struct_id) = arg_type {
                let arg_name = symtab.name_for_symbol(symbol_id).to_string();
                symtab.expand_struct_symbol(
                    symbol_id,
                    &arg_name,
                    struct_id,
                    arg.ident.span,
                    struct_registry,
                );
            }
        }
    }

    let block_visitor = BlockVisitor::new(struct_registry);
    let (blocks, symbol_spans) = block_visitor.create_blocks(&f.statements, symtab);

    let id = *f.meta.get().expect("Should have FunctionId by now");
    let modifiers = FunctionModifiers::new(f, symtab);

    // Expand struct arguments into their leaf symbols
    let mut arguments = Vec::new();
    for (arg, &symbol_id) in f.arguments.iter().zip(arg_symbols.0.iter()) {
        if let Some(ref type_with_loc) = arg.ty {
            let arg_type = type_with_loc.resolved();
            if let crate::types::Type::Struct(_) = arg_type {
                // Struct parameter: use expanded leaf symbols
                collect_leaf_symbols(symbol_id, symtab, &mut arguments);
            } else {
                // Scalar parameter
                arguments.push(symbol_id);
            }
        } else {
            // No type annotation - treat as scalar
            arguments.push(symbol_id);
        }
    }

    // Expand struct return types into their leaf types
    let mut return_types = Vec::new();
    for ret_type in f.return_types.types.iter().map(|t| t.resolved()) {
        if let crate::types::Type::Struct(struct_id) = ret_type {
            collect_leaf_types(struct_id, struct_registry, &mut return_types);
        } else {
            return_types.push(ret_type);
        }
    }

    (
        TapIrFunction::new(
            id,
            blocks.into_boxed_slice(),
            modifiers,
            arguments.into_boxed_slice(),
            return_types.into_boxed_slice(),
        ),
        symbol_spans,
    )
}

/// Collect all leaf (scalar) symbols from a struct expansion recursively.
fn collect_leaf_symbols(symbol_id: SymbolId, symtab: &SymTab, out: &mut Vec<SymbolId>) {
    if let Some(expansion) = symtab.get_struct_expansion(symbol_id) {
        for &field_symbol in &expansion.fields {
            collect_leaf_symbols(field_symbol, symtab, out);
        }
    } else {
        out.push(symbol_id);
    }
}

/// Collect all leaf (scalar) types from a struct type recursively.
fn collect_leaf_types(
    struct_id: crate::types::StructId,
    registry: &StructRegistry,
    out: &mut Vec<crate::types::Type>,
) {
    let def = registry.get(struct_id);
    for field in &def.fields {
        if let crate::types::Type::Struct(nested_id) = field.ty {
            collect_leaf_types(nested_id, registry, out);
        } else {
            out.push(field.ty);
        }
    }
}

struct BlockVisitor<'a> {
    blocks: Vec<TapIrBlock>,
    current_block: Vec<TapIr>,

    loop_entries: Vec<LoopEntry>,
    next_free_block_id: usize,
    next_block_id: Option<BlockId>,

    symbol_spans: SymbolSpans,
    struct_registry: &'a StructRegistry,
}

struct LoopEntry {
    entry: BlockId,
    exit: BlockId,
}

impl<'a> BlockVisitor<'a> {
    fn new(struct_registry: &'a StructRegistry) -> Self {
        Self {
            blocks: Vec::new(),
            current_block: Vec::new(),
            loop_entries: Vec::new(),
            next_free_block_id: 0,
            next_block_id: None,
            symbol_spans: SymbolSpans::new(),
            struct_registry,
        }
    }

    fn create_blocks(
        mut self,
        statements: &[ast::Statement<'_>],
        symtab: &mut SymTab,
    ) -> (Vec<TapIrBlock>, SymbolSpans) {
        for statement in statements {
            self.visit_statement(statement, symtab);
        }

        if !self.current_block.is_empty() || self.next_block_id.is_some() {
            self.finalize_block(BlockExitInstr::Return(Box::new([])), None);
        }

        if self.blocks.is_empty() {
            self.finalize_block(BlockExitInstr::Return(Box::new([])), None);
        }

        (self.blocks, self.symbol_spans)
    }

    fn visit_statement(&mut self, statement: &ast::Statement<'_>, symtab: &mut SymTab) {
        match &statement.kind {
            ast::StatementKind::Error => {
                unreachable!("Shouldn't be creating IR if there is an error")
            }
            ast::StatementKind::Assignment { values, .. }
            | ast::StatementKind::VariableDeclaration { values, .. } => {
                let target_symbols: &Vec<SymbolId> =
                    statement.meta.get().expect("Should've resolved symbols");
                let field_info: Option<&FieldAssignmentInfo> = statement.meta.get();

                let temps: Vec<SymbolId> = if target_symbols.len() == values.len() {
                    // Paired assignment: evaluate all RHS into temporaries first,
                    // then move to targets (enables swap idiom: a, b = b, a)
                    //
                    // Special case: struct constructors assign directly to target fields
                    // since their arguments don't depend on the targets (no swap issue).
                    values
                        .iter()
                        .zip(target_symbols.iter())
                        .map(|(value, &target)| {
                            // Check if this is a struct constructor - if so, assign directly
                            if let ast::ExpressionKind::Call { .. } = &value.kind
                                && let Some(InternalOrExternalFunctionId::StructConstructor(_)) =
                                    value.meta.get()
                            {
                                // Struct constructor: assign directly to target
                                self.blocks_for_expression(value, target, symtab);
                                return target; // No temp needed, already assigned to target
                            }

                            // Non-struct: use temporary as before
                            let temp = symtab.new_temporary();
                            self.blocks_for_expression(value, temp, symtab);
                            temp
                        })
                        .collect()
                } else if values.len() == 1 {
                    // this must be a function call
                    let ast::ExpressionKind::Call { arguments, .. } = &values[0].kind else {
                        panic!(
                            "Type checker should've caught this, got mismatching symbols and no function call"
                        );
                    };

                    let args = arguments
                        .iter()
                        .map(|a| {
                            let symbol = symtab.new_temporary();
                            self.blocks_for_expression(a, symbol, symtab);
                            symbol
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice();

                    let f: InternalOrExternalFunctionId = *values[0]
                        .meta
                        .get()
                        .expect("Should have function IDs by now");

                    let temps = (0..target_symbols.len())
                        .map(|_| symtab.new_temporary())
                        .collect::<Vec<_>>();

                    match f {
                        InternalOrExternalFunctionId::Internal(f) => {
                            self.current_block.push(TapIr::Call {
                                target: temps.clone().into_boxed_slice(),
                                f,
                                args,
                            });
                        }
                        InternalOrExternalFunctionId::External(f) => {
                            self.current_block.push(TapIr::CallExternal {
                                target: temps.clone().into_boxed_slice(),
                                f,
                                args,
                            });
                        }
                        InternalOrExternalFunctionId::Builtin(f) => {
                            // Builtins return exactly one value
                            debug_assert_eq!(temps.len(), 1);
                            self.current_block.push(TapIr::CallBuiltin {
                                target: temps[0],
                                f,
                                args,
                            });
                        }
                        InternalOrExternalFunctionId::StructConstructor(_) => {
                            // Struct constructors return a single value, so if we're here
                            // (multi-return branch), it means the code tried to assign a
                            // struct to multiple variables. This should be caught by the
                            // type checker.
                            panic!(
                                "Struct constructor in multi-return context - type checker should have caught this"
                            );
                        }
                    }

                    temps
                } else {
                    panic!("Type checker should've caught the count mismatch");
                };

                for (idx, (&root_symbol, temp)) in target_symbols.iter().zip(temps).enumerate() {
                    // Check if root_symbol is a struct property base
                    if SymTab::is_struct_property_base_symbol(root_symbol) {
                        // Struct property field assignment
                        if let Some(field_info) = field_info
                            && let Some(Some((_struct_id, field_indices))) = field_info.0.get(idx)
                        {
                            // Build the property path from root_symbol and field indices
                            let base_name = symtab.name_for_symbol(root_symbol).to_string();
                            let prop_path = self.build_struct_property_path(
                                &base_name,
                                root_symbol,
                                field_indices,
                            );

                            if let Some(prop) = symtab.get_property_by_path(&prop_path) {
                                // Scalar field - generate StoreProp directly
                                self.current_block.push(TapIr::StoreProp {
                                    prop_index: prop.index,
                                    value: temp,
                                });
                            } else {
                                // Nested struct assignment - store all scalar fields
                                self.store_struct_property_fields(
                                    &prop_path,
                                    temp,
                                    symtab,
                                );
                            }
                        }
                        continue;
                    }

                    // Resolve the actual target symbol - may be a field if this is a field assignment
                    let target_symbol = if let Some(field_info) = field_info {
                        if let Some(Some((struct_id, field_indices))) = field_info.0.get(idx) {
                            // Field assignment - resolve to the actual field symbol
                            self.resolve_field_symbol(
                                root_symbol,
                                *struct_id,
                                field_indices,
                                statement.span,
                                symtab,
                            )
                        } else {
                            root_symbol
                        }
                    } else {
                        root_symbol
                    };

                    // Skip if already assigned directly (e.g., struct constructor)
                    if temp == target_symbol {
                        continue;
                    }

                    // Check if temp has a struct expansion - if so, copy fields instead of single move
                    if let Some(temp_expansion) = symtab.get_struct_expansion(temp).cloned() {
                        // Struct-to-struct assignment: expand target and copy all fields
                        let target_name = symtab.name_for_symbol(target_symbol).to_string();
                        let target_expansion = symtab.expand_struct_symbol(
                            target_symbol,
                            &target_name,
                            temp_expansion.struct_id,
                            statement.span,
                            self.struct_registry,
                        );
                        self.copy_struct_fields(&temp_expansion, &target_expansion, symtab);
                    } else {
                        // Scalar assignment
                        let is_property = symtab.get_property(target_symbol).is_some();
                        let global_id = GlobalId::from_symbol_id(target_symbol);

                        let expr_target = if is_property || global_id.is_some() {
                            symtab.new_temporary()
                        } else {
                            target_symbol
                        };

                        self.current_block.push(TapIr::Move {
                            target: expr_target,
                            source: temp,
                        });

                        if let Some(property) = symtab.get_property(target_symbol) {
                            self.current_block.push(TapIr::StoreProp {
                                prop_index: property.index,
                                value: expr_target,
                            });
                        } else if let Some(global_id) = global_id {
                            self.current_block.push(TapIr::SetGlobal {
                                global_index: global_id.0,
                                value: expr_target,
                            });
                        }
                    }
                }
            }
            ast::StatementKind::Wait => {
                self.current_block.push(TapIr::Wait);
            }
            ast::StatementKind::Block { block } => {
                for statement in block {
                    self.visit_statement(statement, symtab);
                }
            }
            ast::StatementKind::Continue => {
                let loop_entry = self
                    .loop_entries
                    .last()
                    .expect("Continue should be in loop by this point");
                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry.entry), None);
            }
            ast::StatementKind::Break => {
                let loop_entry = self
                    .loop_entries
                    .last()
                    .expect("Continue should be in loop by this point");
                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry.exit), None);
            }
            ast::StatementKind::Nop => {}
            ast::StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                let condition_target = symtab.new_temporary();
                self.blocks_for_expression(condition, condition_target, symtab);

                let true_block_id = self.next_block_id();
                let false_block_id = self.next_block_id();
                let continue_block_id = self.next_block_id();

                self.finalize_block(
                    BlockExitInstr::ConditionalJump {
                        test: condition_target,
                        if_true: true_block_id,
                        if_false: false_block_id,
                    },
                    Some(true_block_id),
                );

                for statement in true_block {
                    self.visit_statement(statement, symtab);
                }
                self.finalize_block(
                    BlockExitInstr::JumpToBlock(continue_block_id),
                    Some(false_block_id),
                );

                for statement in false_block {
                    self.visit_statement(statement, symtab);
                }
                self.finalize_block(
                    BlockExitInstr::JumpToBlock(continue_block_id),
                    Some(continue_block_id),
                );
            }
            ast::StatementKind::Loop { block } => {
                let loop_entry = self.next_block_id();
                let loop_exit = self.next_block_id();

                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry), Some(loop_entry));

                self.loop_entries.push(LoopEntry {
                    entry: loop_entry,
                    exit: loop_exit,
                });

                for statement in block {
                    self.visit_statement(statement, symtab);
                }

                self.loop_entries.pop();

                self.finalize_block(BlockExitInstr::JumpToBlock(loop_entry), Some(loop_exit));
            }
            ast::StatementKind::Expression { expression } => {
                // Evaluate the expression for side effects, discard the result
                let target = symtab.new_temporary();
                self.blocks_for_expression(expression, target, symtab);
            }
            ast::StatementKind::Spawn { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                let f: InternalOrExternalFunctionId = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");

                match f {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        self.current_block.push(TapIr::Spawn {
                            f: function_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::External(_) => {
                        panic!("Shouldn't be able to spawn an external function")
                    }
                    InternalOrExternalFunctionId::Builtin(_) => {
                        panic!("Shouldn't be able to spawn a builtin function")
                    }
                    InternalOrExternalFunctionId::StructConstructor(_) => {
                        panic!("Shouldn't be able to spawn a struct constructor")
                    }
                }
            }
            ast::StatementKind::Trigger { arguments, .. } => {
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                let f = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");
                self.current_block.push(TapIr::Trigger { f, args });
            }
            ast::StatementKind::Return { values } => {
                let mut return_values = Vec::new();
                for v in values {
                    let return_symbol = symtab.new_temporary();
                    self.blocks_for_expression(v, return_symbol, symtab);

                    // If this is a struct, expand to leaf symbols
                    collect_leaf_symbols(return_symbol, symtab, &mut return_values);
                }

                self.finalize_block(
                    BlockExitInstr::Return(return_values.into_boxed_slice()),
                    None,
                );
            }
        }
    }

    fn next_block_id(&mut self) -> BlockId {
        self.next_free_block_id += 1;
        BlockId::from_raw(self.next_free_block_id - 1)
    }

    fn finalize_block(&mut self, block_exit: BlockExitInstr, next_block_id: Option<BlockId>) {
        let id = self
            .next_block_id
            .take()
            .unwrap_or_else(|| self.next_block_id());

        self.blocks.push(TapIrBlock::new(
            id,
            std::mem::take(&mut self.current_block),
            block_exit,
        ));

        self.next_block_id = next_block_id;
    }

    /// Resolves a field path to the actual field symbol.
    /// Starts from root_symbol and navigates through field_indices to find the target field.
    fn resolve_field_symbol(
        &self,
        root_symbol: SymbolId,
        struct_id: crate::types::StructId,
        field_indices: &[usize],
        span: crate::tokens::Span,
        symtab: &mut SymTab,
    ) -> SymbolId {
        // Ensure the root symbol has a struct expansion
        if symtab.get_struct_expansion(root_symbol).is_none() {
            let root_name = symtab.name_for_symbol(root_symbol).to_string();
            symtab.expand_struct_symbol(
                root_symbol,
                &root_name,
                struct_id,
                span,
                self.struct_registry,
            );
        }

        // Navigate through field indices to find the target field
        let mut current_symbol = root_symbol;
        for &field_index in field_indices {
            let expansion = symtab
                .get_struct_expansion(current_symbol)
                .expect("Field path should have struct expansion");
            current_symbol = expansion.fields[field_index];
        }

        current_symbol
    }

    /// Sets the value of the expression to the symbol at target_symbol
    fn blocks_for_expression(
        &mut self,
        expr: &Expression<'_>,
        target_symbol: SymbolId,
        symtab: &mut SymTab,
    ) {
        // Record the span where this symbol's value is defined
        self.symbol_spans.define(target_symbol, expr.span);

        match &expr.kind {
            ast::ExpressionKind::Integer(i) => {
                self.current_block
                    .push(TapIr::Constant(target_symbol, Constant::Int(*i)));
            }
            ast::ExpressionKind::Fix(num) => {
                self.current_block
                    .push(TapIr::Constant(target_symbol, Constant::Fix(*num)));
            }
            ast::ExpressionKind::Bool(b) => {
                self.current_block
                    .push(TapIr::Constant(target_symbol, Constant::Bool(*b)));
            }
            ast::ExpressionKind::Variable(_) => {
                let source = *expr.meta.get().expect("Should've resolved variable");
                if let Some(property) = symtab.get_property(source) {
                    self.current_block.push(TapIr::GetProp {
                        target: target_symbol,
                        prop_index: property.index,
                    });
                } else if let Some(global_id) = GlobalId::from_symbol_id(source) {
                    self.current_block.push(TapIr::GetGlobal {
                        target: target_symbol,
                        global_index: global_id.0,
                    });
                } else if let Some(source_expansion) = symtab.get_struct_expansion(source).cloned()
                {
                    // Struct-typed variable: copy all fields to target's expansion
                    let target_name = symtab.name_for_symbol(target_symbol).to_string();
                    let target_expansion = symtab.expand_struct_symbol(
                        target_symbol,
                        &target_name,
                        source_expansion.struct_id,
                        expr.span,
                        self.struct_registry,
                    );

                    self.copy_struct_fields(&source_expansion, &target_expansion, symtab);
                } else {
                    self.current_block.push(TapIr::Move {
                        target: target_symbol,
                        source,
                    });
                }
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                let lhs_target = symtab.new_temporary();

                match operator {
                    BinaryOperator::Then => {
                        self.blocks_for_expression(lhs, lhs_target, symtab);
                        self.blocks_for_expression(rhs, target_symbol, symtab);
                    }
                    BinaryOperator::And | BinaryOperator::Or => {
                        // first calculate the value of the lhs
                        let is_or = *operator == BinaryOperator::Or;

                        self.blocks_for_expression(lhs, lhs_target, symtab);

                        let rhs_block_id = self.next_block_id();
                        let continue_block_id = self.next_block_id();

                        // if it's &&, then we should start by assigning false to the target_symbol and jumping only
                        // if the initial lhs is true. For || it should be the other way around
                        self.current_block
                            .push(TapIr::Constant(target_symbol, Constant::Bool(is_or)));

                        self.finalize_block(
                            BlockExitInstr::ConditionalJump {
                                test: lhs_target,
                                if_true: if is_or {
                                    continue_block_id
                                } else {
                                    rhs_block_id
                                },
                                if_false: if !is_or {
                                    continue_block_id
                                } else {
                                    rhs_block_id
                                },
                            },
                            Some(rhs_block_id),
                        );

                        self.blocks_for_expression(rhs, target_symbol, symtab);
                        self.finalize_block(
                            BlockExitInstr::JumpToBlock(continue_block_id),
                            Some(continue_block_id),
                        );
                    }
                    operator => {
                        let rhs_target = symtab.new_temporary();

                        self.blocks_for_expression(lhs, lhs_target, symtab);
                        self.blocks_for_expression(rhs, rhs_target, symtab);

                        self.current_block.push(TapIr::BinOp {
                            target: target_symbol,
                            lhs: lhs_target,
                            rhs: rhs_target,
                            op: *operator,
                        });
                    }
                }
            }
            ast::ExpressionKind::Error => {
                unreachable!("Shouldn't be creating IR if there is an error");
            }
            ast::ExpressionKind::Nop => {}
            ast::ExpressionKind::Call { arguments, .. } => {
                let function_id: InternalOrExternalFunctionId = *expr
                    .meta
                    .get()
                    .expect("Should've assigned function IDs by now");

                // Struct constructors: expand args and target, copy leaf-to-leaf
                if let InternalOrExternalFunctionId::StructConstructor(struct_id) = function_id {
                    self.handle_struct_constructor(
                        target_symbol,
                        struct_id,
                        arguments,
                        expr.span,
                        symtab,
                    );
                    return;
                }

                // For non-struct-constructor calls, evaluate and expand struct args to leaf symbols
                let mut args = Vec::new();
                for arg in arguments {
                    let arg_sym = symtab.new_temporary();
                    self.blocks_for_expression(arg, arg_sym, symtab);
                    // Expand struct args to leaf symbols
                    collect_leaf_symbols(arg_sym, symtab, &mut args);
                }
                let args = args.into_boxed_slice();

                // Get return type info to know if we need to expand target for struct return
                let return_info: Option<&CallReturnInfo> = expr.meta.get();

                match function_id {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        // Expand target if function returns a struct
                        if let Some(CallReturnInfo(ret_types)) = return_info
                            && let Some(Type::Struct(struct_id)) = ret_types.first()
                        {
                            let target_name = symtab.name_for_symbol(target_symbol).to_string();
                            symtab.expand_struct_symbol(
                                target_symbol,
                                &target_name,
                                *struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                        }
                        // Collect target leaf symbols
                        let mut targets = Vec::new();
                        collect_leaf_symbols(target_symbol, symtab, &mut targets);
                        self.current_block.push(TapIr::Call {
                            target: targets.into_boxed_slice(),
                            f: function_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::External(external_function_id) => {
                        // Expand target if function returns a struct
                        if let Some(CallReturnInfo(ret_types)) = return_info
                            && let Some(Type::Struct(struct_id)) = ret_types.first()
                        {
                            let target_name = symtab.name_for_symbol(target_symbol).to_string();
                            symtab.expand_struct_symbol(
                                target_symbol,
                                &target_name,
                                *struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                        }
                        // Collect target leaf symbols
                        let mut targets = Vec::new();
                        collect_leaf_symbols(target_symbol, symtab, &mut targets);
                        self.current_block.push(TapIr::CallExternal {
                            target: targets.into_boxed_slice(),
                            f: external_function_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::Builtin(builtin_id) => {
                        // Builtins don't take or return structs
                        self.current_block.push(TapIr::CallBuiltin {
                            target: target_symbol,
                            f: builtin_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::StructConstructor(_) => {
                        // Handled above with early return
                        unreachable!("Struct constructors are handled separately");
                    }
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                // Get the resolved function ID from type checking
                let info: &MethodCallInfo = expr
                    .meta
                    .get()
                    .expect("MethodCallInfo should be set during type checking");
                let function_id = info.function_id;

                // Evaluate the receiver - this becomes the first argument (self)
                let receiver_sym = symtab.new_temporary();
                self.blocks_for_expression(receiver, receiver_sym, symtab);

                // Collect receiver as first arg (expand if struct)
                let mut args = Vec::new();
                collect_leaf_symbols(receiver_sym, symtab, &mut args);

                // Evaluate and collect the explicit arguments
                for arg in arguments {
                    let arg_sym = symtab.new_temporary();
                    self.blocks_for_expression(arg, arg_sym, symtab);
                    collect_leaf_symbols(arg_sym, symtab, &mut args);
                }
                let args = args.into_boxed_slice();

                // Get return type info to know if we need to expand target for struct return
                let return_info: Option<&CallReturnInfo> = expr.meta.get();

                match function_id {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        // Expand target if function returns a struct
                        if let Some(CallReturnInfo(ret_types)) = return_info
                            && let Some(Type::Struct(struct_id)) = ret_types.first()
                        {
                            let target_name = symtab.name_for_symbol(target_symbol).to_string();
                            symtab.expand_struct_symbol(
                                target_symbol,
                                &target_name,
                                *struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                        }
                        // Collect target leaf symbols
                        let mut targets = Vec::new();
                        collect_leaf_symbols(target_symbol, symtab, &mut targets);
                        self.current_block.push(TapIr::Call {
                            target: targets.into_boxed_slice(),
                            f: function_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::External(external_function_id) => {
                        // Expand target if function returns a struct
                        if let Some(CallReturnInfo(ret_types)) = return_info
                            && let Some(Type::Struct(struct_id)) = ret_types.first()
                        {
                            let target_name = symtab.name_for_symbol(target_symbol).to_string();
                            symtab.expand_struct_symbol(
                                target_symbol,
                                &target_name,
                                *struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                        }
                        // Collect target leaf symbols
                        let mut targets = Vec::new();
                        collect_leaf_symbols(target_symbol, symtab, &mut targets);
                        self.current_block.push(TapIr::CallExternal {
                            target: targets.into_boxed_slice(),
                            f: external_function_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::Builtin(builtin_id) => {
                        // Builtins don't take or return structs
                        self.current_block.push(TapIr::CallBuiltin {
                            target: target_symbol,
                            f: builtin_id,
                            args,
                        });
                    }
                    InternalOrExternalFunctionId::StructConstructor(_) => {
                        unreachable!("Struct constructors cannot be called as methods")
                    }
                }
            }
            ast::ExpressionKind::FieldAccess { base, field } => {
                // Get the field info from type checking
                let info = expr
                    .meta
                    .get::<FieldAccessInfo>()
                    .expect("FieldAccess should have FieldAccessInfo");

                // Check if the base is a struct property base (e.g., "pos" in "property pos: Point;")
                if let ast::ExpressionKind::Variable(_) = &base.kind
                    && let Some(base_symbol) = base.meta.get::<SymbolId>()
                    && SymTab::is_struct_property_base_symbol(*base_symbol)
                {
                    // This is a struct property field access - generate GetProp directly
                    // Build the full path (e.g., "pos.x") and look up the expanded property
                    let base_name = symtab.name_for_symbol(*base_symbol);
                    let full_path = format!("{}.{}", base_name, field.ident);

                    if let Some(prop) = symtab.get_property_by_path(&full_path) {
                        self.current_block.push(TapIr::GetProp {
                            target: target_symbol,
                            prop_index: prop.index,
                        });
                    } else {
                        // Property path not found - this shouldn't happen if type checking passed
                        // Handle nested struct property access by building the full path
                        self.handle_struct_property_field_access(
                            target_symbol,
                            &base_name,
                            field.ident,
                            info,
                            expr.span,
                            symtab,
                        );
                    }
                    return;
                }

                // Regular field access (not a struct property base)
                // Evaluate the base into a temp (handles any expression uniformly)
                let base_temp = symtab.new_temporary();
                self.blocks_for_expression(base, base_temp, symtab);

                // Ensure the base is expanded. If not (e.g., function call result),
                // expand it using the struct type from FieldAccessInfo.
                if symtab.get_struct_expansion(base_temp).is_none() {
                    let base_name = symtab.name_for_symbol(base_temp).to_string();
                    symtab.expand_struct_symbol(
                        base_temp,
                        &base_name,
                        info.base_struct_id,
                        expr.span,
                        self.struct_registry,
                    );
                }

                let base_expansion = symtab
                    .get_struct_expansion(base_temp)
                    .expect("Base should be expanded")
                    .clone();

                // Get the field's symbol from the base expansion
                let field_symbol = base_expansion.fields[info.field_index];

                // Check if this field is struct-typed (has its own expansion)
                if let Some(field_expansion) = symtab.get_struct_expansion(field_symbol).cloned() {
                    // Field is struct-typed: create expansion for target and copy
                    let target_name = symtab.name_for_symbol(target_symbol).to_string();
                    let target_expansion = symtab.expand_struct_symbol(
                        target_symbol,
                        &target_name,
                        field_expansion.struct_id,
                        expr.span,
                        self.struct_registry,
                    );

                    self.copy_struct_fields(&field_expansion, &target_expansion, symtab);
                } else {
                    // Scalar field: simple move
                    self.current_block.push(TapIr::Move {
                        target: target_symbol,
                        source: field_symbol,
                    });
                }
            }
        }
    }

    /// Handle struct constructor calls.
    /// A struct constructor is essentially an identity function - it just returns its args.
    /// We expand both args and target to leaves, then copy leaf-to-leaf.
    fn handle_struct_constructor(
        &mut self,
        target_symbol: SymbolId,
        struct_id: crate::types::StructId,
        arguments: &[Expression<'_>],
        span: crate::tokens::Span,
        symtab: &mut SymTab,
    ) {
        // Expand target
        let target_name = symtab.name_for_symbol(target_symbol).to_string();
        symtab.expand_struct_symbol(
            target_symbol,
            &target_name,
            struct_id,
            span,
            self.struct_registry,
        );

        // Evaluate args and collect expanded leaves
        let mut arg_leaves = Vec::new();
        for arg in arguments {
            let arg_sym = symtab.new_temporary();
            self.blocks_for_expression(arg, arg_sym, symtab);
            collect_leaf_symbols(arg_sym, symtab, &mut arg_leaves);
        }

        // Collect target leaves
        let mut target_leaves = Vec::new();
        collect_leaf_symbols(target_symbol, symtab, &mut target_leaves);

        // Copy leaf-to-leaf
        for (&target, &source) in target_leaves.iter().zip(&arg_leaves) {
            self.current_block.push(TapIr::Move { target, source });
        }
    }

    /// Recursively copy fields from one struct expansion to another.
    fn copy_struct_fields(
        &mut self,
        source: &crate::compile::symtab_visitor::ExpandedStruct,
        target: &crate::compile::symtab_visitor::ExpandedStruct,
        symtab: &SymTab,
    ) {
        for (&source_field, &target_field) in source.fields.iter().zip(&target.fields) {
            // Check if this field is struct-typed (has nested expansion)
            if let Some(source_nested) = symtab.get_struct_expansion(source_field) {
                let target_nested = symtab
                    .get_struct_expansion(target_field)
                    .expect("Target should have matching expansion");
                self.copy_struct_fields(source_nested, target_nested, symtab);
            } else {
                // Scalar field
                self.current_block.push(TapIr::Move {
                    target: target_field,
                    source: source_field,
                });
            }
        }
    }

    /// Handle nested struct property field access.
    /// For deeply nested paths like `pos.origin.x`, recursively builds the path
    /// and generates GetProp for scalar fields.
    fn handle_struct_property_field_access(
        &mut self,
        target_symbol: SymbolId,
        base_path: &Cow<'_, str>,
        field_name: &str,
        info: &FieldAccessInfo,
        span: crate::tokens::Span,
        symtab: &mut SymTab,
    ) {
        // Build the full path
        let full_path = format!("{}.{}", base_path, field_name);

        // Check if this is a scalar property
        if let Some(prop) = symtab.get_property_by_path(&full_path) {
            self.current_block.push(TapIr::GetProp {
                target: target_symbol,
                prop_index: prop.index,
            });
            return;
        }

        // This must be a nested struct field - we need to recursively get all scalar fields
        // and copy them to the target's expansion
        let struct_def = self.struct_registry.get(info.base_struct_id);
        let field_def = &struct_def.fields[info.field_index];

        if let Type::Struct(nested_struct_id) = field_def.ty {
            // Create expansion for target
            let target_name = symtab.name_for_symbol(target_symbol).to_string();
            symtab.expand_struct_symbol(
                target_symbol,
                &target_name,
                nested_struct_id,
                span,
                self.struct_registry,
            );

            // Get all leaf properties for this path and copy them
            self.copy_struct_property_fields_to_expansion(
                target_symbol,
                &full_path,
                nested_struct_id,
                symtab,
            );
        }
    }

    /// Copy scalar property fields matching a path prefix to an expanded struct symbol.
    fn copy_struct_property_fields_to_expansion(
        &mut self,
        target_symbol: SymbolId,
        path_prefix: &str,
        struct_id: crate::types::StructId,
        symtab: &SymTab,
    ) {
        let target_expansion = symtab
            .get_struct_expansion(target_symbol)
            .expect("Target should be expanded")
            .clone();

        let struct_def = self.struct_registry.get(struct_id);

        for (i, field) in struct_def.fields.iter().enumerate() {
            let field_path = format!("{}.{}", path_prefix, field.name);
            let target_field = target_expansion.fields[i];

            match field.ty {
                Type::Struct(nested_id) => {
                    // Recursively copy nested struct fields
                    self.copy_struct_property_fields_to_expansion(
                        target_field,
                        &field_path,
                        nested_id,
                        symtab,
                    );
                }
                _ => {
                    // Scalar field - get from property
                    if let Some(prop) = symtab.get_property_by_path(&field_path) {
                        self.current_block.push(TapIr::GetProp {
                            target: target_field,
                            prop_index: prop.index,
                        });
                    }
                }
            }
        }
    }

    /// Build a property path from a base name and field indices.
    fn build_struct_property_path(
        &self,
        base_name: &str,
        base_symbol: SymbolId,
        field_indices: &[usize],
    ) -> String {
        // Get the struct ID from the base symbol
        let Some(struct_id) = SymTab::struct_id_from_base_symbol(base_symbol) else {
            return base_name.to_string();
        };

        let mut path = base_name.to_string();
        let mut current_struct_id = struct_id;

        for &field_index in field_indices {
            let struct_def = self.struct_registry.get(current_struct_id);
            let field = &struct_def.fields[field_index];
            path = format!("{}.{}", path, field.name);

            // Update current_struct_id if the field is a struct
            if let Type::Struct(nested_id) = field.ty {
                current_struct_id = nested_id;
            }
        }

        path
    }

    /// Store all scalar fields from a source symbol to struct property paths.
    fn store_struct_property_fields(
        &mut self,
        path_prefix: &str,
        source_symbol: SymbolId,
        symtab: &SymTab,
    ) {
        // Get the struct expansion for the source
        let Some(expansion) = symtab.get_struct_expansion(source_symbol).cloned() else {
            return;
        };

        let struct_def = self.struct_registry.get(expansion.struct_id);

        for (i, field) in struct_def.fields.iter().enumerate() {
            let field_path = format!("{}.{}", path_prefix, field.name);
            let source_field = expansion.fields[i];

            match field.ty {
                Type::Struct(_) => {
                    // Recursively store nested struct fields
                    self.store_struct_property_fields(&field_path, source_field, symtab);
                }
                _ => {
                    // Scalar field - generate StoreProp
                    if let Some(prop) = symtab.get_property_by_path(&field_path) {
                        self.current_block.push(TapIr::StoreProp {
                            prop_index: prop.index,
                            value: source_field,
                        });
                    }
                }
            }
        }
    }
}
