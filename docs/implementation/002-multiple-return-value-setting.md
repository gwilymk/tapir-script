# Multiple Return Value Assignment

## Overview

This document outlines the implementation plan for supporting multiple return value assignment in tapir-script. This feature allows capturing all return values from functions that return multiple values in a single statement:

```tapir
var a, b = function_which_returns_two_values();
```

Currently, functions can declare multiple return types and return multiple values, but there is no syntax to capture those values into variables.

## Current State

### What Works

- **Function Return Types**: Functions can declare multiple return types via `-> (int, fix)` syntax
- **Return Statements**: Functions can return multiple values via `return 5, 3.0;`
- **IR & Bytecode**: `Call` and `CallExternal` instructions already have `target: Box<[SymbolId]>` supporting multiple targets
- **Type System**: `FunctionType.rets` is `Vec<Type>`, fully supporting multiple returns

### What's Missing

- **Parser**: Grammar only accepts single identifier in `var` declarations
- **AST**: `VariableDeclaration` stores single `ident: &'input str`
- **Assignment**: Only single identifier assignments are supported
- **Symbol Resolution**: Metadata only stores single `SymbolId`
- **Type Checking**: Multi-return functions are rejected in expression contexts
- **IR Generation**: Only single target is passed to `Call` instructions

### Current Workaround

```tapir
fn returns_two() -> (int, fix) {
    return 5, 3.0;
}

# Can only call and discard:
returns_two();

# Cannot capture:
# var a, b = returns_two();  # SYNTAX ERROR
```

## Design Decisions

### Syntax Choice

Support comma-separated identifiers on the left-hand side and comma-separated expressions on the right:

```tapir
var a, b = returns_two();      # Unpack multi-return function
var a, b = expr1, expr2;       # Paired assignment
a, b = returns_two();          # Assignment (both must exist)
a, b = expr1, expr2;           # Paired assignment to existing
```

Rejected alternatives:

- Tuple unpacking `var (a, b) = ...` - adds parentheses inconsistently with return syntax
- Separate statements `var a = returns_two()[0];` - requires indexing syntax and multiple calls

### Two Modes of Multi-Assignment

**Paired assignment**: N identifiers, N expressions - each expression assigned to corresponding identifier:

```tapir
var a, b = 1, 2;               # a = 1, b = 2
var x, y = foo(), bar();       # x = foo(), y = bar()
a, b = b, a;                   # Swap values
```

**Multi-return unpacking**: N identifiers, 1 function call returning N values:

```tapir
var a, b = returns_two();      # Unpack 2 return values into a, b
```

The mode is determined by comparing counts:
- If `len(identifiers) == len(expressions)`: paired assignment
- If `len(expressions) == 1` and expression is a call returning `len(identifiers)` values: unpack
- Otherwise: error

### New vs Existing Variables

For declarations, all identifiers are new (may shadow existing):

```tapir
var a, b = returns_two();  # Declares both a and b
var a, existing = 1, 2;    # Declares both (shadows 'existing' if in scope)
```

For assignments, all identifiers must already exist:

```tapir
a, b = returns_two();      # Both must be declared
a, b = 1, 2;               # Both must be declared
```

### Type Validation

For paired assignment, each expression's type must match its target:

```tapir
var a: int, b: fix = 1, 2.0;   # OK: types match
var a: int, b: int = 1, 2.0;   # ERROR: fix not assignable to int
```

For multi-return unpacking, each return type must match its target:

```tapir
fn returns_int_fix() -> (int, fix) { return 1, 2.0; }

var a, b = returns_int_fix();       # OK: a is int, b is fix
var a: fix, b = returns_int_fix();  # ERROR: int not assignable to fix
```

### Count Validation

```tapir
var a, b = 1, 2, 3;            # ERROR: 2 targets, 3 expressions
var a, b, c = returns_two();   # ERROR: 3 targets, function returns 2
var a, b = 1;                  # ERROR: 2 targets, 1 expression (not a multi-return call)
```

### No Flattening

Multi-return calls are **not** expanded when mixed with other expressions:

```tapir
var x, y, z = returns_two(), 3;   # ERROR: 3 targets, 2 expressions
```

Even though `returns_two()` returns 2 values, it counts as 1 expression. This keeps semantics simple and predictable. Write instead:

```tapir
var x, y = returns_two();
var z = 3;
```

### Evaluation Order

For paired assignment, all expressions on the right-hand side are evaluated **before** any assignments occur. This enables the swap idiom:

```tapir
a, b = b, a;   # Swap values: evaluates b and a first, then assigns
```

Implementation: evaluate all RHS expressions into temporaries, then assign temporaries to targets.

## Implementation Plan

### Phase 1: AST Changes

**Files**: `compiler/src/ast.rs`

1. Generalize existing variants to support multiple identifiers and multiple expressions:

   ```rust
   pub enum StatementKind<'input> {
       VariableDeclaration {
           idents: Vec<&'input str>,       // Was: ident: &'input str
           values: Vec<Expression<'input>>, // Was: value: Expression<'input>
       },
       Assignment {
           idents: Vec<&'input str>,       // Was: ident: &'input str
           values: Vec<Expression<'input>>, // Was: value: Expression<'input>
       },
       // ...
   }
   ```

   Single-variable assignment becomes the common case with one-element vecs.

2. Update `Statement` metadata to handle multiple symbols:

   ```rust
   pub type StatementMeta = MaybeResolved<Vec<SymbolId>>;
   ```

   For single-variable statements, this is a one-element vec. All existing code that accesses the metadata will need minor updates (e.g., `meta.get()[0]` instead of `meta.get()`).

### Phase 2: Grammar Changes

**Files**: `compiler/src/grammar.lalrpop`

1. Add comma-separated identifier list:

   ```lalrpop
   IdentifierList: Vec<&'input str> = {
       <first: identifier> <rest: ("," <identifier>)*> => {
           let mut idents = vec![first];
           idents.extend(rest);
           idents
       }
   };
   ```

2. Add comma-separated expression list:

   ```lalrpop
   ExpressionList: Vec<Expression<'input>> = {
       <first: Expression> <rest: ("," <Expression>)*> => {
           let mut exprs = vec![first];
           exprs.extend(rest);
           exprs
       }
   };
   ```

3. Update statement rules to use both lists:

   ```lalrpop
   StatementKind: StatementKind<'input> = {
       "var" <idents: IdentifierList> "=" <values: ExpressionList> ";" =>
           StatementKind::VariableDeclaration { idents, values },

       <idents: IdentifierList> "=" <values: ExpressionList> ";" =>
           StatementKind::Assignment { idents, values },
       // ...
   };
   ```

   Note: LALRPOP may require restructuring to avoid ambiguity between assignment and expression statements. The comma in `ExpressionList` may also conflict with other uses of comma - may need precedence adjustments or a dedicated context.

### Phase 3: Symbol Resolution

**Files**: `compiler/src/compile/symtab_visitor.rs`

Symbol resolution doesn't change much - it still just resolves identifiers. The expression count validation happens in type checking.

1. Update `VariableDeclaration` to handle multiple identifiers:

   ```rust
   ast::StatementKind::VariableDeclaration { idents, values, .. } => {
       // Visit all value expressions first
       for value in values {
           self.visit_expression(value);
       }

       let symbols: Vec<SymbolId> = idents
           .iter()
           .map(|ident| {
               let sym_id = self.symtab.new_symbol();
               self.local_symbols.insert(ident.to_string(), sym_id);
               sym_id
           })
           .collect();
       statement.meta.set(symbols);
   }
   ```

2. Update `Assignment` to handle multiple identifiers:

   ```rust
   ast::StatementKind::Assignment { idents, values, .. } => {
       // Visit all value expressions first
       for value in values {
           self.visit_expression(value);
       }

       let symbols: Result<Vec<SymbolId>, _> = idents
           .iter()
           .map(|ident| {
               self.resolve_variable(ident)
                   .ok_or(CompilerErrorKind::UnknownVariable { name: ident })
           })
           .collect();

       match symbols {
           Ok(syms) => statement.meta.set(syms),
           Err(e) => self.diagnostics.add_message(e, statement.span),
       }
   }
   ```

### Phase 4: Type Checking

**Files**: `compiler/src/compile/type_visitor.rs`

Type checking determines the assignment mode and validates types accordingly.

1. Add helper to get return types from a call expression:

   ```rust
   fn return_types_for_call(&self, expr: &Expression) -> Option<Vec<Type>> {
       match &expr.kind {
           ExpressionKind::Call { .. } => {
               let func_id = expr.meta.get().expect("resolved");
               Some(self.get_function_return_types(func_id))
           }
           _ => None,
       }
   }
   ```

2. Update `VariableDeclaration` and `Assignment` handling:

   ```rust
   ast::StatementKind::VariableDeclaration { idents, values, .. }
   | ast::StatementKind::Assignment { idents, values, .. } => {
       let symbol_ids = statement.meta.get().expect("resolved");
       let num_idents = idents.len();
       let num_values = values.len();

       if num_idents == num_values {
           // Paired assignment: each expression to each identifier
           for (sym_id, value) in symbol_ids.iter().zip(values.iter()) {
               let value_type = self.type_for_expression(value);
               self.check_or_set_symbol_type(*sym_id, value_type);
           }
       } else if num_values == 1 {
           // Potentially multi-return unpacking
           if let Some(return_types) = self.return_types_for_call(&values[0]) {
               if return_types.len() == num_idents {
                   // Multi-return unpacking
                   for (sym_id, ret_type) in symbol_ids.iter().zip(return_types.iter()) {
                       self.check_or_set_symbol_type(*sym_id, ret_type.clone());
                   }
               } else {
                   self.diagnostics.add_message(
                       CompilerErrorKind::CountMismatch {
                           expected: num_idents,
                           actual: return_types.len(),
                       },
                       statement.span,
                   );
               }
           } else {
               // Single non-call expression can't satisfy multiple targets
               self.diagnostics.add_message(
                   CompilerErrorKind::CountMismatch {
                       expected: num_idents,
                       actual: 1,
                   },
                   statement.span,
               );
           }
       } else {
           // Mismatched counts
           self.diagnostics.add_message(
               CompilerErrorKind::CountMismatch {
                   expected: num_idents,
                   actual: num_values,
               },
               statement.span,
           );
       }
   }
   ```

3. Add new error variant:

   ```rust
   pub enum CompilerErrorKind {
       // ...
       CountMismatch { expected: usize, actual: usize },
   }
   ```

### Phase 5: IR Generation

**Files**: `compiler/src/compile/ir.rs`

IR generation needs to handle both paired assignment and multi-return unpacking.

1. Update `VariableDeclaration` and `Assignment` handling:

   ```rust
   ast::StatementKind::VariableDeclaration { values, .. }
   | ast::StatementKind::Assignment { values, .. } => {
       let target_symbols = statement.meta.get().expect("resolved");
       let num_targets = target_symbols.len();
       let num_values = values.len();

       if num_targets == num_values {
           // Paired assignment: evaluate all RHS into temporaries first,
           // then move to targets (enables swap idiom: a, b = b, a)
           let temps: Vec<SymbolId> = values
               .iter()
               .map(|value| {
                   let temp = symtab.new_temporary();
                   self.blocks_for_expression(value, temp, symtab);
                   temp
               })
               .collect();

           // Move temporaries to final targets
           for (target, temp) in target_symbols.iter().zip(temps.iter()) {
               self.current_block.push(TapIr {
                   instr: TapIrInstr::Move { to: *target, from: *temp },
               });
           }
       } else if num_values == 1 {
           // Multi-return unpacking - must be a call expression
           match &values[0].kind {
               ast::ExpressionKind::Call { arguments, .. } => {
                   let func_id = values[0].meta.get().expect("resolved");
                   let args = self.evaluate_arguments(arguments, symtab);

                   match func_id {
                       InternalOrExternalFunctionId::Internal(f) => {
                           self.current_block.push(TapIr {
                               instr: TapIrInstr::Call {
                                   target: target_symbols.clone().into_boxed_slice(),
                                   f: *f,
                                   args,
                               },
                           });
                       }
                       InternalOrExternalFunctionId::External(f) => {
                           self.current_block.push(TapIr {
                               instr: TapIrInstr::CallExternal {
                                   target: target_symbols.clone().into_boxed_slice(),
                                   f: *f,
                                   args,
                               },
                           });
                       }
                   }
               }
               _ => panic!("Type checker should have caught this"),
           }
       } else {
           panic!("Type checker should have caught count mismatch");
       }
   }
   ```

2. No new IR instructions needed:
   - `Move` already exists for copying temporaries to targets
   - `Call` and `CallExternal` already support multiple targets via `Box<[SymbolId]>`

### Phase 6: Bytecode (If Needed)

Review the bytecode layer to ensure multi-target calls are properly handled. The current implementation may already support this if targets are stored in consecutive registers.

**Files**: `bytecode/src/lib.rs`, bytecode emission code

1. Verify `Call` opcode handles multiple return registers
2. If not, may need a new opcode or encoding scheme

## Testing Strategy

### Unit Tests

1. **Parsing**: Snapshot tests for multi-variable syntax
   - `var a, b = foo();`
   - `var a, b = 1, 2;`
   - `a, b, c = bar();`
   - `a, b = b, a;`
   - Error: `var a, = foo();` (trailing comma)
   - Error: `var a, b = 1, 2,;` (trailing comma)

2. **Symbol Resolution**:
   - Multi-declaration creates multiple symbols
   - Multi-assignment resolves existing symbols
   - Error: Multi-assignment with undeclared variable

3. **Type Checking**:
   - Paired: `var a, b = 1, 2;` OK
   - Paired: `var a, b = foo(), bar();` OK (each returns 1)
   - Unpack: `var a, b = returns_two();` OK
   - Error: `var a, b = 1, 2, 3;` (count mismatch)
   - Error: `var a, b, c = returns_two();` (function returns 2)
   - Error: `var a, b = 1;` (1 expression, not multi-return)
   - Error: `var a, b, c = returns_two(), 3;` (no flattening)
   - Type mismatch: `var a: int, b: int = 1, 2.0;`

4. **IR Generation**: Snapshot tests showing:
   - Paired assignment generating sequential expressions
   - Multi-return unpacking generating multi-target `Call`

### Integration Tests

1. Script calling function, capturing both returns, using both values
2. Paired assignment: `var a, b = 1, 2; var c = a + b;`
3. Swap idiom: `a, b = b, a;`
4. Chained: `var a, b = foo(); var c, d = bar(a, b);`
5. Assignment after declaration: `var a, b = foo(); a, b = baz();`

## Error Messages

```
error: count mismatch
  --> script.tapir:5:1
   |
 5 | var a, b, c = returns_two();
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: expected 3 values but got 2

error: count mismatch
  --> script.tapir:7:1
   |
 7 | var a, b = 5;
   | ^^^^^^^^^^^^
   |
   = note: expected 2 values but got 1

error: count mismatch
  --> script.tapir:9:1
   |
 9 | var a, b, c = returns_two(), 3;
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: expected 3 values but got 2
   = help: multi-return functions are not expanded in expression lists

error: unknown variable 'b'
  --> script.tapir:11:1
   |
11 | a, b = 1, 2;
   |    ^
   |
   = help: use 'var a, b = ...' to declare new variables
```

## Dependencies

- No external crate dependencies
- Internal: builds on existing multi-return function infrastructure
- Prerequisite: None (extern fn support is independent)

## Alternatives Considered

### Tuple Type

Instead of multi-value returns, introduce a tuple type:

```tapir
fn returns_tuple() -> (int, fix) { ... }
var t = returns_tuple();  # t has type (int, fix)
var a = t.0;              # Indexing syntax
```

Rejected because:

- Requires new type system complexity
- Indexing syntax feels foreign to the language style
- Existing multi-return infrastructure would need rework

### Ignore Syntax

Allow ignoring some return values:

```tapir
var a, _ = returns_two();  # Ignore second return
```

This is a potential future enhancement but not required for initial implementation. Can be added later if needed.

## Estimated Complexity

- Phase 1 (AST): Low - straightforward additions
- Phase 2 (Grammar): Medium - LALRPOP ambiguity handling may require iteration
- Phase 3 (Symbol Resolution): Low - follows existing patterns
- Phase 4 (Type Checking): Medium - new validation logic
- Phase 5 (IR Generation): Low - infrastructure already exists
- Phase 6 (Bytecode): Low to Medium - depends on current multi-target support
