use crate::{
    ast::{
        self, BinaryOperator, Expression, InternalOrExternalFunctionId, MethodCallInfo,
        OperatorOverloadInfo, SymbolId,
    },
    compile::{
        symtab_visitor::{
            FieldAccessor, FunctionArgumentSymbols, GlobalId, StructLayout, SymTab, SymbolStorage,
        },
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
                symtab.ensure_local_layout(symbol_id, struct_id, arg.ident.span, struct_registry);
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

/// Collect all leaf (scalar) symbols from a struct layout recursively.
fn collect_leaf_symbols(symbol_id: SymbolId, symtab: &SymTab, out: &mut Vec<SymbolId>) {
    if let Some(layout) = symtab.get_struct_layout(symbol_id) {
        out.extend(layout.leaf_symbols());
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
                    values
                        .iter()
                        .map(|value| {
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
                            assert_eq!(temps.len(), 1);
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
                    if symtab.is_property_base(root_symbol) {
                        // Struct property field assignment
                        if let Some(field_info) = field_info
                            && let Some(Some((_struct_id, field_indices))) = field_info.0.get(idx)
                        {
                            // Navigate to the field using the layout structure
                            let prop_layout = symtab
                                .get_struct_layout(root_symbol)
                                .expect("Property base should have a layout")
                                .clone();

                            match prop_layout.navigate(field_indices) {
                                Some(FieldAccessor::Property(prop_index)) => {
                                    // Scalar field - generate StoreProp directly
                                    self.current_block.push(TapIr::StoreProp {
                                        prop_index: *prop_index,
                                        value: temp,
                                    });
                                }
                                Some(FieldAccessor::Nested(_, dst_layout)) => {
                                    // Nested struct assignment - copy all fields
                                    let src_layout = symtab
                                        .get_struct_layout(temp)
                                        .expect("Temp should have a layout")
                                        .clone();
                                    let dst_layout = dst_layout.clone();
                                    self.copy_struct_layout(&src_layout, &dst_layout, symtab);
                                }
                                // Exhaustive matching ensures compile-time errors if new variants are added
                                Some(FieldAccessor::Local(_)) => {
                                    unreachable!("Property layouts cannot contain Local accessors")
                                }
                                Some(FieldAccessor::Global(_)) => {
                                    unreachable!("Property layouts cannot contain Global accessors")
                                }
                                None => unreachable!("Field path should be valid"),
                            }
                        } else {
                            // Direct assignment to entire struct property (e.g., position = fix2(...))
                            let src_layout = symtab
                                .get_struct_layout(temp)
                                .expect("Temp should have a layout for struct assignment")
                                .clone();
                            let dst_layout = symtab
                                .get_struct_layout(root_symbol)
                                .expect("Property base should have a layout")
                                .clone();
                            self.copy_struct_layout(&src_layout, &dst_layout, symtab);
                        }
                        continue;
                    }

                    // Check if root_symbol is a global struct field assignment
                    if GlobalId::from_symbol_id(root_symbol).is_some()
                        && let Some(field_info) = field_info
                        && let Some(Some((_struct_id, field_indices))) = field_info.0.get(idx)
                    {
                        // Navigate to the field using the layout structure
                        let global_layout = symtab
                            .get_struct_layout(root_symbol)
                            .expect("Global should have a layout")
                            .clone();

                        match global_layout.navigate(field_indices) {
                            Some(FieldAccessor::Global(global_id)) => {
                                // Scalar field - generate SetGlobal directly
                                self.current_block.push(TapIr::SetGlobal {
                                    global_index: global_id.0,
                                    value: temp,
                                });
                            }
                            Some(FieldAccessor::Nested(_, dst_layout)) => {
                                // Nested struct assignment - copy all fields
                                let src_layout = symtab
                                    .get_struct_layout(temp)
                                    .expect("Temp should have a layout")
                                    .clone();
                                let dst_layout = dst_layout.clone();
                                self.copy_struct_layout(&src_layout, &dst_layout, symtab);
                            }
                            // Exhaustive matching ensures compile-time errors if new variants are added
                            Some(FieldAccessor::Local(_)) => {
                                unreachable!("Global layouts cannot contain Local accessors")
                            }
                            Some(FieldAccessor::Property(_)) => {
                                unreachable!("Global layouts cannot contain Property accessors")
                            }
                            None => unreachable!("Field path should be valid"),
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

                    // Check if temp has a struct layout - if so, copy fields instead of single move
                    if let Some(temp_layout) = symtab.get_struct_layout(temp).cloned() {
                        // Get or create the target layout
                        let target_layout = symtab.ensure_local_layout(
                            target_symbol,
                            temp_layout.struct_id,
                            statement.span,
                            self.struct_registry,
                        );

                        self.copy_struct_layout(&temp_layout, &target_layout, symtab);
                    } else {
                        // Scalar assignment to any storage class
                        self.emit_store_symbol(target_symbol, temp, symtab);
                    }
                }
            }
            ast::StatementKind::Wait { frames } => {
                let frames_symbol = frames.as_ref().map(|expr| {
                    let symbol = symtab.new_temporary();
                    self.blocks_for_expression(expr, symbol, symtab);
                    symbol
                });
                self.current_block.push(TapIr::Wait {
                    frames: frames_symbol,
                });
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
            ast::StatementKind::Trigger { arguments, .. } => {
                // Evaluate arguments and expand struct types to leaf symbols
                let mut args = Vec::new();
                for arg in arguments {
                    let arg_sym = symtab.new_temporary();
                    self.blocks_for_expression(arg, arg_sym, symtab);
                    collect_leaf_symbols(arg_sym, symtab, &mut args);
                }

                let f = *statement
                    .meta
                    .get()
                    .expect("Should have function IDs by now");
                self.current_block.push(TapIr::Trigger {
                    f,
                    args: args.into_boxed_slice(),
                });
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
        // Ensure the root symbol has a layout and navigate to find the target field
        let layout = symtab.ensure_local_layout(root_symbol, struct_id, span, self.struct_registry);
        layout
            .navigate_to_symbol(field_indices)
            .expect("Field path should be valid")
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
                if let Some(source_layout) = symtab.get_struct_layout(source).cloned() {
                    // Struct-typed variable: copy all fields to target's layout
                    let target_layout = symtab.ensure_local_layout(
                        target_symbol,
                        source_layout.struct_id,
                        expr.span,
                        self.struct_registry,
                    );
                    self.copy_struct_layout(&source_layout, &target_layout, symtab);
                } else {
                    // Scalar: load from any storage class (property, global, or local)
                    self.emit_load_symbol(target_symbol, source, symtab);
                }
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                // Check if this is an overloaded operator (resolved during type checking)
                if let Some(overload_info) = expr.meta.get::<OperatorOverloadInfo>() {
                    // Lower as a function call
                    let lhs_symbol = symtab.new_temporary();
                    self.blocks_for_expression(lhs, lhs_symbol, symtab);

                    let rhs_symbol = symtab.new_temporary();
                    self.blocks_for_expression(rhs, rhs_symbol, symtab);

                    // Collect arguments (expand structs if needed)
                    let mut args = Vec::new();
                    collect_leaf_symbols(lhs_symbol, symtab, &mut args);
                    collect_leaf_symbols(rhs_symbol, symtab, &mut args);
                    let args = args.into_boxed_slice();

                    let return_info: Option<&CallReturnInfo> = expr.meta.get();
                    self.emit_call(
                        overload_info.function_id,
                        target_symbol,
                        args,
                        return_info,
                        expr.span,
                        symtab,
                    );
                    return;
                }

                // Not an overloaded operator - use the default primitive handling
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
            ast::ExpressionKind::UnaryOperation { operator, operand } => {
                let operand_target = symtab.new_temporary();
                self.blocks_for_expression(operand, operand_target, symtab);

                self.current_block.push(TapIr::UnaryOp {
                    target: target_symbol,
                    operand: operand_target,
                    op: *operator,
                });
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

                let return_info: Option<&CallReturnInfo> = expr.meta.get();
                self.emit_call(
                    function_id,
                    target_symbol,
                    args,
                    return_info,
                    expr.span,
                    symtab,
                );
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

                let return_info: Option<&CallReturnInfo> = expr.meta.get();
                self.emit_call(
                    function_id,
                    target_symbol,
                    args,
                    return_info,
                    expr.span,
                    symtab,
                );
            }
            ast::ExpressionKind::FieldAccess { base, field: _ } => {
                // Get the field info from type checking
                let info = expr
                    .meta
                    .get::<FieldAccessInfo>()
                    .expect("FieldAccess should have FieldAccessInfo");

                // Check if the base is a struct property base (e.g., "pos" in "property pos: Point;")
                if let ast::ExpressionKind::Variable(_) = &base.kind
                    && let Some(base_symbol) = base.meta.get::<SymbolId>()
                    && symtab.is_property_base(*base_symbol)
                {
                    // This is a struct property field access - use layout navigation
                    let prop_layout = symtab
                        .get_struct_layout(*base_symbol)
                        .expect("Property base should have a layout")
                        .clone();

                    match prop_layout.field(info.field_index) {
                        Some(FieldAccessor::Property(prop_index)) => {
                            // Scalar field - generate GetProp directly
                            self.current_block.push(TapIr::GetProp {
                                target: target_symbol,
                                prop_index: *prop_index,
                            });
                        }
                        Some(FieldAccessor::Nested(_, src_layout)) => {
                            // Nested struct field - copy all fields to target
                            let dst_layout = symtab.ensure_local_layout(
                                target_symbol,
                                src_layout.struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                            let src_layout = src_layout.clone();
                            self.copy_struct_layout(&src_layout, &dst_layout, symtab);
                        }
                        // Exhaustive matching ensures compile-time errors if new variants are added
                        Some(FieldAccessor::Local(_)) => {
                            unreachable!("Property layouts cannot contain Local accessors")
                        }
                        Some(FieldAccessor::Global(_)) => {
                            unreachable!("Property layouts cannot contain Global accessors")
                        }
                        None => unreachable!("Field index should be valid"),
                    }
                    return;
                }

                // Check if the base is a struct-typed global variable (e.g., "g" in "global g: Point;")
                // Global structs are expanded into multiple scalar globals, so we use GetGlobal
                // to read the specific field's global.
                if let ast::ExpressionKind::Variable(_) = &base.kind
                    && let Some(base_symbol) = base.meta.get::<SymbolId>()
                    && GlobalId::from_symbol_id(*base_symbol).is_some()
                {
                    // Use layout navigation to access the field
                    let global_layout = symtab
                        .get_struct_layout(*base_symbol)
                        .expect("Global should have a layout")
                        .clone();

                    match global_layout.field(info.field_index) {
                        Some(FieldAccessor::Global(global_id)) => {
                            // Scalar field - generate GetGlobal directly
                            self.current_block.push(TapIr::GetGlobal {
                                target: target_symbol,
                                global_index: global_id.0,
                            });
                        }
                        Some(FieldAccessor::Nested(_, src_layout)) => {
                            // Nested struct field - copy all fields to target
                            let dst_layout = symtab.ensure_local_layout(
                                target_symbol,
                                src_layout.struct_id,
                                expr.span,
                                self.struct_registry,
                            );
                            let src_layout = src_layout.clone();
                            self.copy_struct_layout(&src_layout, &dst_layout, symtab);
                        }
                        // Exhaustive matching ensures compile-time errors if new variants are added
                        Some(FieldAccessor::Local(_)) => {
                            unreachable!("Global layouts cannot contain Local accessors")
                        }
                        Some(FieldAccessor::Property(_)) => {
                            unreachable!("Global layouts cannot contain Property accessors")
                        }
                        None => unreachable!("Field index should be valid"),
                    }
                    return;
                }

                // Regular field access (not a struct property base or global struct)
                // Evaluate the base into a temp (handles any expression uniformly)
                let base_temp = symtab.new_temporary();
                self.blocks_for_expression(base, base_temp, symtab);

                // Ensure the base has a layout (may not have one for function call results)
                let base_layout = symtab.ensure_local_layout(
                    base_temp,
                    info.base_struct_id,
                    expr.span,
                    self.struct_registry,
                );

                // Get the field's symbol from the base layout
                let field_symbol = base_layout
                    .field_symbol(info.field_index)
                    .expect("Field should have symbol");

                // Check if this field is struct-typed (has its own layout)
                if let Some(FieldAccessor::Nested(_, field_layout)) =
                    base_layout.field(info.field_index)
                {
                    // Field is struct-typed: create layout for target and copy
                    let target_layout = symtab.ensure_local_layout(
                        target_symbol,
                        field_layout.struct_id,
                        expr.span,
                        self.struct_registry,
                    );
                    self.copy_struct_layout(field_layout, &target_layout, symtab);
                } else {
                    // Scalar field: simple move
                    self.current_block.push(TapIr::Move {
                        target: target_symbol,
                        source: field_symbol,
                    });
                }
            }
            ast::ExpressionKind::Spawn { arguments, .. } => {
                // Evaluate arguments
                let args = arguments
                    .iter()
                    .map(|a| {
                        let symbol = symtab.new_temporary();
                        self.blocks_for_expression(a, symbol, symtab);
                        symbol
                    })
                    .collect();

                // Get the function ID from the expression's metadata
                let f: InternalOrExternalFunctionId =
                    *expr.meta.get().expect("Should have function IDs by now");

                match f {
                    InternalOrExternalFunctionId::Internal(function_id) => {
                        self.current_block.push(TapIr::Spawn {
                            target: target_symbol,
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
        // Evaluate args into temps and collect their leaf symbols
        let mut arg_leaves = Vec::new();
        for arg in arguments {
            let arg_sym = symtab.new_temporary();
            self.blocks_for_expression(arg, arg_sym, symtab);
            collect_leaf_symbols(arg_sym, symtab, &mut arg_leaves);
        }

        // Create target layout and copy args directly to target leaves
        let target_layout =
            symtab.ensure_local_layout(target_symbol, struct_id, span, self.struct_registry);
        let target_leaves = target_layout.leaf_symbols();

        for (&target, &source) in target_leaves.iter().zip(&arg_leaves) {
            self.current_block.push(TapIr::Move { target, source });
        }
    }

    /// Emit a function call, handling struct return expansion and target collection.
    /// This is the common path for regular calls, method calls, and operator overloads.
    fn emit_call(
        &mut self,
        function_id: InternalOrExternalFunctionId,
        target_symbol: SymbolId,
        args: Box<[SymbolId]>,
        return_info: Option<&CallReturnInfo>,
        span: crate::tokens::Span,
        symtab: &mut SymTab,
    ) {
        match function_id {
            InternalOrExternalFunctionId::Internal(f) => {
                // Create layout for target if function returns a struct
                if let Some(CallReturnInfo(ret_types)) = return_info
                    && let Some(Type::Struct(struct_id)) = ret_types.first()
                {
                    symtab.ensure_local_layout(
                        target_symbol,
                        *struct_id,
                        span,
                        self.struct_registry,
                    );
                }
                let mut targets = Vec::new();
                collect_leaf_symbols(target_symbol, symtab, &mut targets);
                self.current_block.push(TapIr::Call {
                    target: targets.into_boxed_slice(),
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::External(f) => {
                // Create layout for target if function returns a struct
                if let Some(CallReturnInfo(ret_types)) = return_info
                    && let Some(Type::Struct(struct_id)) = ret_types.first()
                {
                    symtab.ensure_local_layout(
                        target_symbol,
                        *struct_id,
                        span,
                        self.struct_registry,
                    );
                }
                let mut targets = Vec::new();
                collect_leaf_symbols(target_symbol, symtab, &mut targets);
                self.current_block.push(TapIr::CallExternal {
                    target: targets.into_boxed_slice(),
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::Builtin(f) => {
                // Builtins return single values, no struct handling needed
                self.current_block.push(TapIr::CallBuiltin {
                    target: target_symbol,
                    f,
                    args,
                });
            }
            InternalOrExternalFunctionId::StructConstructor(_) => {
                unreachable!("Struct constructors should be handled separately")
            }
        }
    }

    /// Unified struct copy using StructLayout.
    /// Copies all fields from source layout to destination layout,
    /// handling all 9 storage class combinations uniformly.
    fn copy_struct_layout(&mut self, src: &StructLayout, dst: &StructLayout, symtab: &mut SymTab) {
        assert_eq!(
            src.struct_id, dst.struct_id,
            "Source and destination must have same struct type"
        );
        assert_eq!(
            src.fields.len(),
            dst.fields.len(),
            "Source and destination must have same number of fields"
        );

        for (src_field, dst_field) in src.fields.iter().zip(&dst.fields) {
            self.copy_field_accessor(src_field, dst_field, symtab);
        }
    }

    /// Load a scalar value from a symbol into a target local.
    /// Dispatches based on the source symbol's storage class.
    fn emit_load_symbol(&mut self, target: SymbolId, source: SymbolId, symtab: &SymTab) {
        match symtab.get_storage(source) {
            SymbolStorage::Property(idx) => {
                self.current_block.push(TapIr::GetProp {
                    target,
                    prop_index: idx,
                });
            }
            SymbolStorage::Global(idx) => {
                self.current_block.push(TapIr::GetGlobal {
                    target,
                    global_index: idx,
                });
            }
            SymbolStorage::Local => {
                self.current_block.push(TapIr::Move { target, source });
            }
        }
    }

    /// Store a value into a symbol.
    /// Dispatches based on the destination symbol's storage class.
    fn emit_store_symbol(&mut self, dest: SymbolId, value: SymbolId, symtab: &SymTab) {
        match symtab.get_storage(dest) {
            SymbolStorage::Property(idx) => {
                self.current_block.push(TapIr::StoreProp {
                    prop_index: idx,
                    value,
                });
            }
            SymbolStorage::Global(idx) => {
                self.current_block.push(TapIr::SetGlobal {
                    global_index: idx,
                    value,
                });
            }
            SymbolStorage::Local => {
                self.current_block.push(TapIr::Move {
                    target: dest,
                    source: value,
                });
            }
        }
    }

    /// Load a scalar value from a FieldAccessor into a local symbol.
    fn emit_load(&mut self, target: SymbolId, src: &FieldAccessor) {
        match src {
            FieldAccessor::Local(sym) => {
                self.current_block.push(TapIr::Move {
                    target,
                    source: *sym,
                });
            }
            FieldAccessor::Property(idx) => {
                self.current_block.push(TapIr::GetProp {
                    target,
                    prop_index: *idx,
                });
            }
            FieldAccessor::Global(id) => {
                self.current_block.push(TapIr::GetGlobal {
                    target,
                    global_index: id.0,
                });
            }
            FieldAccessor::Nested(..) => {
                unreachable!(
                    "emit_load is for scalars only; use copy_struct_layout for nested structs"
                )
            }
        }
    }

    /// Store a local symbol value into a FieldAccessor.
    fn emit_store(&mut self, dst: &FieldAccessor, value: SymbolId) {
        match dst {
            FieldAccessor::Local(sym) => {
                self.current_block.push(TapIr::Move {
                    target: *sym,
                    source: value,
                });
            }
            FieldAccessor::Property(idx) => {
                self.current_block.push(TapIr::StoreProp {
                    prop_index: *idx,
                    value,
                });
            }
            FieldAccessor::Global(id) => {
                self.current_block.push(TapIr::SetGlobal {
                    global_index: id.0,
                    value,
                });
            }
            FieldAccessor::Nested(..) => {
                unreachable!(
                    "emit_store is for scalars only; use copy_struct_layout for nested structs"
                )
            }
        }
    }

    /// Copy a single field accessor from source to destination.
    /// Always goes through a temp for simplicity - optimizer will clean up redundant moves.
    fn copy_field_accessor(
        &mut self,
        src: &FieldAccessor,
        dst: &FieldAccessor,
        symtab: &mut SymTab,
    ) {
        match (src, dst) {
            (FieldAccessor::Nested(_, src_nested), FieldAccessor::Nested(_, dst_nested)) => {
                self.copy_struct_layout(src_nested, dst_nested, symtab);
            }
            _ => {
                let temp = symtab.new_temporary();
                self.emit_load(temp, src);
                self.emit_store(dst, temp);
            }
        }
    }
}
