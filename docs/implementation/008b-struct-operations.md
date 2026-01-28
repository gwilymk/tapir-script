# Struct Operations (Part 2 of 3)

## Overview

This document covers struct operations: symbol expansion, constructor IR lowering, field access, field assignment, struct-to-struct assignment, and function parameters/returns. After completing this part, structs are fully usable within tapir-script.

**Prerequisites**: [008a-struct-types.md](008a-struct-types.md) must be completed first.

See also:
- [008c-struct-rust-integration.md](008c-struct-rust-integration.md) - Properties, macro updates

## Implementation Plan

Each phase leaves the project in a compilable, testable state.

### Phase 6: Symbol Table Field Expansion

**Files**: `compiler/src/compile/symtab_visitor.rs`

When a struct-typed variable is declared, expand it into component symbols:

```rust
pub struct ExpandedStruct {
    pub fields: HashMap<String, SymbolId>,
    pub leaf_symbols: Vec<SymbolId>,
}

impl SymTab {
    pub fn expand_struct_symbol(
        &mut self,
        base_name: &str,
        struct_id: StructId,
        span: Span,
        registry: &StructRegistry,
    ) -> ExpandedStruct {
        let mut fields = HashMap::new();
        let mut leaf_symbols = Vec::new();

        self.expand_struct_recursive(
            base_name,
            struct_id,
            span,
            registry,
            &mut fields,
            &mut leaf_symbols,
        );

        ExpandedStruct { fields, leaf_symbols }
    }

    fn expand_struct_recursive(
        &mut self,
        path: &str,
        struct_id: StructId,
        span: Span,
        registry: &StructRegistry,
        fields: &mut HashMap<String, SymbolId>,
        leaf_symbols: &mut Vec<SymbolId>,
    ) {
        let def = registry.get(struct_id);

        for field in &def.fields {
            let field_path = format!("{}.{}", path, field.name);

            match field.ty {
                Type::Struct(nested_id) => {
                    // Recurse for nested structs
                    self.expand_struct_recursive(
                        &field_path,
                        nested_id,
                        span,
                        registry,
                        fields,
                        leaf_symbols,
                    );
                }
                _ => {
                    // Leaf field - create a symbol
                    let symbol_id = self.new_symbol(&field_path, span);
                    fields.insert(field_path, symbol_id);
                    leaf_symbols.push(symbol_id);
                }
            }
        }
    }
}
```

Track which symbols are struct expansions so IR lowering can find them.

**Tests**:

- Struct variable `p` creates symbols `p.x`, `p.y`
- Nested struct creates recursive symbols (e.g., `r.origin.x`, `r.origin.y`, `r.size.x`, `r.size.y`)
- Existing tests still pass

**Compilable**: Yes - symbols created, but not yet used in codegen

---

### Phase 7: IR Lowering - Constructors

**Files**: `compiler/src/compile/ir/lowering.rs`

Implement constructor call lowering - each argument assigns to a field symbol:

```rust
ResolvedFunctionId::StructConstructor(struct_id) => {
    let expansion = symtab.get_struct_expansion(target_symbol);
    let def = registry.get(struct_id);

    // For each argument, lower it to the corresponding field symbol
    for (arg, field) in call.arguments.iter().zip(&def.fields) {
        match field.ty {
            Type::Struct(nested_id) => {
                // Argument is a struct - get its expansion and copy
                let arg_symbol = /* resolve argument */;
                let arg_expansion = symtab.get_struct_expansion(arg_symbol);
                let field_path = format!("{}.{}", base_path, field.name);
                let field_expansion = /* get expansion for this field */;

                for (src, dst) in arg_expansion.leaf_symbols.iter()
                    .zip(&field_expansion.leaf_symbols)
                {
                    self.current_block.push(TapIr::Move {
                        target: *dst,
                        source: *src,
                    });
                }
            }
            _ => {
                // Scalar field - direct assignment
                let field_symbol = expansion.fields[&field.name];
                self.lower_expression(arg, field_symbol, ...);
            }
        }
    }
}
```

**Tests**:

- IR snapshot: `var p = Point(1, 2)` produces assignments to `p.x`, `p.y`
- Nested constructor: `Rect(Point(0,0), Point(1,1))` works
- Existing tests still pass

**Compilable**: Yes - constructors work end-to-end

---

### Phase 8: Field Access (Reading)

**Files**: `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`, `compiler/src/compile/type_visitor.rs`, `compiler/src/compile/ir/lowering.rs`

Add field access expression:

```rust
// ast.rs
ExpressionKind::FieldAccess {
    base: Box<Expression<'input>>,
    field: &'input str,
},
```

```lalrpop
PostfixExpr: Expression<'input> = {
    <start:@L> <base:PostfixExpr> "." <field:Ident> <end:@R> =>
        ExpressionKind::FieldAccess { base: Box::new(base), field: field.ident }
            .with_span(file_id, start, end),
    CallExpr,
};
```

**Type checking** validates field exists on struct type:

```rust
ExpressionKind::FieldAccess { base, field } => {
    let base_type = self.visit_expression(base, symtab, registry, diagnostics);

    match base_type {
        Type::Struct(struct_id) => {
            let def = registry.get(struct_id);
            if let Some(field_def) = def.fields.iter().find(|f| f.name == *field) {
                field_def.ty
            } else {
                ErrorKind::UnknownField {
                    ty: base_type,
                    field: field.to_string(),
                }
                .at(expr.span)
                .emit(diagnostics);
                Type::Error
            }
        }
        Type::Error => Type::Error,
        _ => {
            ErrorKind::FieldAccessOnNonStruct { ty: base_type }
                .at(expr.span)
                .emit(diagnostics);
            Type::Error
        }
    }
}
```

**IR lowering** resolves to the expanded symbol and emits a Move:

```rust
ExpressionKind::FieldAccess { base, field } => {
    // Resolve the full field path to a symbol ID
    let field_symbol = self.resolve_field_access(base, field, symtab, registry);

    self.current_block.push(TapIr::Move {
        target: target_symbol,
        source: field_symbol,
    });
}

fn resolve_field_access(&self, base: &Expression, field: &str, ...) -> SymbolId {
    // Build the full path by walking the chain (handles nested access like r.origin.x)
    let path = self.build_field_path(base, field);
    symtab.get_field_symbol(&path).expect("Type checker validated")
}
```

**Tests**:

- `p.x` type-checks to `int`
- Unknown field produces error
- Field access on non-struct produces error
- IR snapshot: `var x = p.x` produces Move from `p.x` symbol
- Chained access: `r.origin.x` works
- Existing tests still pass

**Compilable**: Yes - field reads work end-to-end

---

### Phase 9: Field Assignment

**Files**: `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`, `compiler/src/compile/type_visitor.rs`, `compiler/src/compile/ir/lowering.rs`

Add field assignment statement:

```rust
// ast.rs
StatementKind::FieldAssignment {
    base: Expression<'input>,  // The full a.b.c expression
    value: Expression<'input>,
},
```

Parse `p.x = value;` as FieldAssignment. The grammar needs to recognize when an assignment target is a field access vs a simple variable.

**Type checking** validates assigned value matches field type:

```rust
StatementKind::FieldAssignment { base, value } => {
    let field_type = self.visit_expression(base, symtab, registry, diagnostics);
    let value_type = self.visit_expression(value, symtab, registry, diagnostics);

    if field_type != value_type && field_type != Type::Error && value_type != Type::Error {
        ErrorKind::TypeMismatch {
            expected: field_type,
            got: value_type,
        }
        .at(value.span)
        .emit(diagnostics);
    }
}
```

**IR lowering** resolves target symbol and lowers value to it:

```rust
StatementKind::FieldAssignment { base, value } => {
    // base is an expression like `a.b.c` - resolve to the leaf symbol
    let field_symbol = self.resolve_field_expression(base, symtab, registry);
    self.lower_expression(value, field_symbol, symtab, registry);
}
```

**Tests**:

- `p.x = 10;` works
- Type mismatch produces error
- Nested: `r.origin.x = 5;` works
- IR snapshot tests
- Existing tests still pass

**Compilable**: Yes - field assignment works end-to-end

---

### Phase 10: Struct-to-Struct Assignment

**Files**: `compiler/src/compile/ir/lowering.rs`

When assigning one struct variable to another, copy all leaf fields:

```rust
// In variable assignment lowering, check if the type is a struct
if let Type::Struct(_) = value_type {
    let src_leaves = symtab.get_struct_expansion(src_symbol).leaf_symbols;
    let dst_leaves = symtab.get_struct_expansion(dst_symbol).leaf_symbols;

    for (src, dst) in src_leaves.iter().zip(&dst_leaves) {
        self.current_block.push(TapIr::Move {
            target: *dst,
            source: *src,
        });
    }
} else {
    // Scalar assignment - existing code
    self.current_block.push(TapIr::Move {
        target: dst_symbol,
        source: src_symbol,
    });
}
```

This also handles struct assignment from function returns and constructor results.

**Tests**:

- `var q = p;` copies all fields
- Assignment from function return
- Nested struct assignment
- IR snapshot tests
- Existing tests still pass

**Compilable**: Yes - struct assignment works

---

### Phase 11: Function Parameters & Returns

**Files**: `compiler/src/compile/symtab_visitor.rs`, `compiler/src/compile/ir/lowering.rs`

Functions can take struct parameters and return structs:

```tapir
fn add_points(a: Point, b: Point) -> Point {
    return Point(a.x + b.x, a.y + b.y);
}
```

**Parameter expansion**: When processing function arguments, expand struct parameters into multiple symbols just like local variables:

```rust
// In visit_function for each parameter
if let Type::Struct(struct_id) = param_type {
    let expansion = symtab.expand_struct_symbol(param_name, struct_id, span, registry);
    // Store expansion info for use in IR lowering
} else {
    // Scalar parameter - existing code
}
```

**Return value expansion**: Return statements with struct values expand to multiple return values at IR level.

**Call site handling**: When calling a function that takes struct parameters, the caller must pass the expanded leaf symbols. When receiving a struct return value, the caller must receive into expanded symbols.

**Tests**:

- Function with struct param
- Function returning struct
- Calling such functions
- IR snapshot tests
- Existing tests still pass

**Compilable**: Yes - struct function params work

---

## Implementation Order Summary

| Phase | What's Added              | Can Test                |
| ----- | ------------------------- | ----------------------- |
| 6     | Symbol expansion          | Symbol table tests      |
| 7     | Constructor IR            | Constructor IR snapshots|
| 8     | Field access (read)       | Field read tests + IR   |
| 9     | Field assignment          | Field write tests + IR  |
| 10    | Struct assignment         | Struct copy tests + IR  |
| 11    | Function params           | Function tests          |

## What's Next

After completing 008b, structs are fully usable within tapir-script:
- Define structs
- Construct struct values
- Access and assign fields
- Copy structs
- Pass structs to/from functions

But you cannot yet:
- Use struct properties (communicate with Rust host)
- Use the TapirScript derive macro with struct properties

Continue to [008c-struct-rust-integration.md](008c-struct-rust-integration.md) for properties and macro updates.
