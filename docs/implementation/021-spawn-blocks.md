# Implementation Plan: Spawn Blocks

## Overview

Add support for inline spawn blocks - anonymous functions that are spawned immediately. This is syntactic sugar for a common pattern where a spawned function is only called once.

```tapir
# Current approach - requires named function
fn animation_loop() {
    loop {
        animation_frame = animation_frame + 1;
        wait;
    }
}
spawn animation_loop();

# New approach - inline spawn block
spawn {
    loop {
        animation_frame = animation_frame + 1;
        wait;
    }
}
```

Both produce identical bytecode - the compiler synthesizes a hidden function for spawn blocks.

## Design Decisions

### Syntax

Spawn blocks use the existing `spawn` keyword followed by a block:

```tapir
spawn { <statements> }
```

The block contains zero or more statements, just like a function body. The spawn block is an expression that returns a `task` handle, consistent with `spawn fn()`.

### Allowed Anywhere

Spawn blocks can appear anywhere an expression is valid - inside functions, event handlers, conditionals, loops, etc:

```tapir
fn on_hit() {
    if should_explode {
        spawn {
            wait 30;
            trigger remove_me();
        };
    }
}
```

The synthetic function is always top-level, but the spawn _call_ stays in place. This works because spawn blocks have no closure capture - the body only sees globals and properties regardless of where the block appears in source.

### No Closures

Spawn blocks are **not closures**. They cannot capture local variables:

```tapir
fn example() {
    var x = 42;
    spawn {
        x = x + 1;  # ERROR: `x` is not defined
    };
}
```

Spawn blocks can only access:

- Global variables
- Properties
- Other functions (including calling them)

This matches the existing behavior of spawned functions and keeps the implementation simple.

### Compilation Strategy: AST Desugaring

Spawn blocks are desugared into synthetic functions early in compilation (after parsing, before symbol resolution). This approach:

- Reuses existing function compilation infrastructure
- Produces identical bytecode to hand-written functions
- Keeps later compilation stages unchanged
- Makes the transformation visible in AST snapshots for testing

The desugaring transforms:

```tapir
spawn {
    loop { wait; }
}
```

Into:

```tapir
fn __spawn_block_0() {
    loop { wait; }
}
spawn __spawn_block_0();
```

### Synthetic Function Naming

Synthetic functions use the naming pattern `@spawn_block_<N>` where N is a unique counter. The @ prefix:

- Indicates compiler-generated code
- Is reserved
- Won't conflict with user-defined names

### Return Type

Spawn blocks always have no return value (unit type), matching the constraint that spawned functions ignore return values. A spawn block with an explicit return statement is a compile error:

```tapir
spawn {
    return 42;  # ERROR: spawn blocks cannot return values
};
```

### Task Handle

Like regular spawn expressions, spawn blocks return a `task` handle:

```tapir
var t: task = spawn {
    wait 100;
};
t.cancel();  # Can cancel the spawned block
```

## Grammar Changes

**File:** `compiler/src/grammar.lalrpop`

Add spawn block as an expression alongside the existing spawn call:

```lalrpop
// Existing spawn call
<start: @L> "spawn" <ident: identifier> "(" <arguments: CommaSeparated<Expression>> ")" <end: @R> =>
    ExpressionKind::Spawn {
        call: Box::new(ExpressionKind::Call {
            name: ident,
            arguments,
        }.with_span(file_id, start, end))
    }.with_span(file_id, start, end),

// NEW: spawn block
<start: @L> "spawn" <block: Block> <end: @R> =>
    ExpressionKind::SpawnBlock {
        body: block,
    }.with_span(file_id, start, end),
```

**Tested:** The grammar compiles without LALRPOP conflicts. The parser correctly distinguishes `spawn identifier(...)` from `spawn { ... }` because `identifier` and `{` are distinct tokens - no ambiguity.

## AST Changes

**File:** `compiler/src/ast.rs`

### String-Owned Names

Change name fields from `&'input str` to `String` so that synthetic (compiler-generated) names don't require leaking. This affects:

- `Function.name`: `&'input str` → `String`
- `ExpressionKind::Call { name }`: `&'input str` → `String`

The grammar produces these via `.to_string()` at parse time. This is a small cost — names are short and allocated once.

Other `&'input str` fields (e.g. `Variable`, `TypeWithLocation.name`, `BuiltinFunction.name`, `ExternFunctionDefinition.name`) stay borrowed since they always come from source text.

### SpawnBlock Variant

Add a new expression variant for spawn blocks:

```rust
pub enum ExpressionKind<'input> {
    // ...existing variants...

    /// Spawn an inline block as a concurrent task
    SpawnBlock {
        body: Vec<Statement<'input>>,
    },

    // ...remaining variants...
}
```

## Desugaring Infrastructure

**File:** `compiler/src/compile/desugar.rs` (new file)

Rather than a single monolithic desugarer, we provide a `DesugarPass` trait and shared mutable AST walking functions. Each desugaring (spawn blocks now, `loop <expr> { }` and `else if` later) is a separate trait implementation. This addresses the lack of a mutable equivalent to `all_inner` — instead of adding `all_inner_mut()`, individual passes call shared `walk_*` functions in their default arms.

### The `DesugarPass` Trait

```rust
/// A desugaring pass that transforms the AST before symbol resolution.
///
/// Implement this trait for each desugaring transformation. The default
/// methods recurse into children via `walk_*` free functions — override
/// only the methods relevant to your transformation.
pub trait DesugarPass<'input> {
    fn visit_expression(&mut self, expr: &mut Expression<'input>) {
        walk_expression(self, expr);
    }

    fn visit_statement(&mut self, stmt: &mut Statement<'input>) {
        walk_statement(self, stmt);
    }

    /// Called after traversal to inject any new top-level items (e.g. synthetic functions).
    fn finish(self, module: &mut Script<'input>);
}
```

### Shared Walking Functions

Free functions that handle the recursive traversal via mutual recursion with the trait methods. `walk_expression` calls `pass.visit_expression()` (not `walk_expression`) on each child — this ensures the pass's overridden method fires at every level. The default `visit_expression` calls `walk_expression`, completing the cycle: `visit → walk → visit → walk → ...` down the tree.

```rust
pub fn walk_expression<'input>(pass: &mut impl DesugarPass<'input>, expr: &mut Expression<'input>) {
    match &mut expr.kind {
        ExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            pass.visit_expression(lhs);
            pass.visit_expression(rhs);
        }
        ExpressionKind::UnaryOperation { operand, .. } => {
            pass.visit_expression(operand);
        }
        ExpressionKind::Call { arguments, .. } => {
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        ExpressionKind::Spawn { call } => {
            pass.visit_expression(call);
        }
        ExpressionKind::SpawnBlock { body } => {
            for stmt in body {
                pass.visit_statement(stmt);
            }
        }
        ExpressionKind::FieldAccess { base, .. } => {
            pass.visit_expression(base);
        }
        ExpressionKind::MethodCall { receiver, arguments, .. } => {
            pass.visit_expression(receiver);
            for arg in arguments {
                pass.visit_expression(arg);
            }
        }
        // Leaves — nothing to recurse into
        ExpressionKind::Integer(_)
        | ExpressionKind::Fix(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Variable(_)
        | ExpressionKind::Nop
        | ExpressionKind::Error => {}
    }
}

pub fn walk_statement<'input>(pass: &mut impl DesugarPass<'input>, stmt: &mut Statement<'input>) {
    match &mut stmt.kind {
        StatementKind::Expression { expression } => pass.visit_expression(expression),
        StatementKind::VariableDeclaration { values, .. } => {
            for val in values { pass.visit_expression(val); }
        }
        StatementKind::Assignment { values, .. } => {
            for val in values { pass.visit_expression(val); }
        }
        StatementKind::Wait { frames } => {
            if let Some(expr) = frames { pass.visit_expression(expr); }
        }
        StatementKind::If { condition, true_block, false_block } => {
            pass.visit_expression(condition);
            for s in true_block { pass.visit_statement(s); }
            for s in false_block { pass.visit_statement(s); }
        }
        StatementKind::Loop { block } | StatementKind::Block { block } => {
            for s in block { pass.visit_statement(s); }
        }
        StatementKind::Return { values } => {
            for val in values { pass.visit_expression(val); }
        }
        StatementKind::Trigger { arguments, .. } => {
            for arg in arguments { pass.visit_expression(arg); }
        }
        StatementKind::Continue | StatementKind::Break
        | StatementKind::Nop | StatementKind::Error => {}
    }
}

/// Run a desugaring pass over a module. Walks all top-level statements
/// and function bodies, then calls `finish` to inject any synthetic items.
pub fn run_desugar_pass<'input>(mut pass: impl DesugarPass<'input>, module: &mut Script<'input>) {
    for top_level in &mut module.statements {
        match top_level {
            TopLevelStatement::Statement(stmt) => pass.visit_statement(stmt),
            TopLevelStatement::FunctionDefinition(f) => {
                for stmt in &mut f.statements {
                    pass.visit_statement(stmt);
                }
            }
            _ => {}
        }
    }
    pass.finish(module);
}
```

### Spawn Block Desugarer

The spawn block transformation as a `DesugarPass` implementation:

```rust
pub struct SpawnBlockDesugarer {
    counter: u32,
    synthetic_functions: Vec<TopLevelStatement<'input>>,
}

impl<'input> DesugarPass<'input> for SpawnBlockDesugarer {
    fn visit_expression(&mut self, expr: &mut Expression<'input>) {
        // Recurse first (bottom-up) so nested spawn blocks are handled
        walk_expression(self, expr);

        if let ExpressionKind::SpawnBlock { body } = &mut expr.kind {
            let name = format!("@spawn_block_{}", self.counter);
            self.counter += 1;

            let fn_decl = Function {
                name: name.clone(),
                statements: std::mem::take(body),
                arguments: vec![],
                return_types: FunctionReturn::default(),
                span: expr.span,
                kind: FunctionKind::Function,
                modifiers: FunctionModifiers::default(),
                meta: Metadata::default(),
            };

            self.synthetic_functions.push(TopLevelStatement::FunctionDefinition(fn_decl));

            expr.kind = ExpressionKind::Spawn {
                call: Box::new(ExpressionKind::Call {
                    name,
                    arguments: vec![],
                }.with_span(expr.span.file_id, expr.span.start, expr.span.end))
            };
        }
    }

    fn finish(self, module: &mut Script<'input>) {
        module.statements.extend(self.synthetic_functions);
    }
}
```

Note: the pass recurses into children _before_ checking for `SpawnBlock`, so nested spawn blocks (e.g. a spawn block inside a spawn block's body) are handled bottom-up.

### Span Handling

Synthetic functions need spans for error reporting. Use the span of the original `spawn` block:

- Function declaration span: entire `spawn { ... }` expression
- Function name span: the `spawn` keyword position

This ensures errors inside spawn blocks point to the correct source location.

## Integration Point

**File:** `compiler/src/compile/compile.rs`

Run desugaring after parsing, before symbol resolution. Each desugar pass is run independently via `run_desugar_pass`:

```rust
pub fn compile(source: &str, ...) -> Result<...> {
    // 1. Parse
    let mut module = parse(source)?;

    // 2. NEW: Desugar
    run_desugar_pass(SpawnBlockDesugarer::new(), &mut module);
    // Future: run_desugar_pass(LoopCountDesugarer::new(), &mut module);
    // Future: run_desugar_pass(ElseIfDesugarer::new(), &mut module);

    // 3. Build symbol table (existing)
    let symtab = build_symbol_table(&module, ...)?;

    // 4. Type check (existing)
    type_check(&mut module, &symtab, ...)?;

    // ... rest of compilation
}
```

## Error Messages

### Undefined Variable in Spawn Block

When a spawn block references a local variable from the enclosing scope:

```
error: undefined variable `x`
  --> script.tapir:4:9
   |
 4 |         x = x + 1;
   |         ^
   |
   = note: spawn blocks cannot capture local variables
   = help: use a global variable instead, or pass the value to a function
```

The "note" is added when the undefined variable matches a local in an enclosing function scope.

### Return in Spawn Block

```
error: spawn blocks cannot return values
  --> script.tapir:3:9
   |
 3 |         return 42;
   |         ^^^^^^^^^
   |
   = help: spawned tasks ignore return values; use a global variable to communicate results
```

This error is detected during type checking when a `return` with a value appears in a synthetic `__spawn_block_*` function.

## Testing Strategy

### Parser Tests

Verify spawn blocks parse correctly:

```tapir
spawn { };
spawn { wait; };
spawn { loop { wait; } };
var t = spawn { wait 10; };
```

### AST Snapshot Tests

Show that spawn blocks create the expected AST structure before desugaring.

### Desugaring Tests

Snapshot tests showing the transformation:

**Input:**

```tapir
spawn {
    wait;
}
```

**After desugaring:**

```tapir
fn @spawn_block_0() {
    wait;
}
spawn @spawn_block_0();
```

### Type Checking Tests

```tapir
# Valid - accessing globals and properties
property x: int;
global g: int = 0;

spawn {
    x = g + 1;
};

# Invalid - accessing locals
fn foo() {
    var local = 42;
    spawn {
        local = 0;  # ERROR: undefined variable `local`
    };
}
```

### Integration Tests

```tapir
# Spawn block runs concurrently
property counter: int;

spawn {
    loop {
        counter = counter + 1;
        wait;
    }
};

wait 10;
# counter should be 10
```

```tapir
# Task handle works
property cancelled: bool;
global t: task;

t = spawn {
    loop {
        wait;
    }
};

wait 5;
t.cancel();
cancelled = true;
```

### Multiple Spawn Blocks

```tapir
# Each gets a unique synthetic function
spawn { wait; };
spawn { wait; };
spawn { wait; };

# Generates:
# fn __spawn_block_0() { wait; }
# fn __spawn_block_1() { wait; }
# fn __spawn_block_2() { wait; }
```

## Implementation Order

### Phase 0: String-Owned Names

1. **ast.rs**: Change `Function.name` and `Call.name` from `&'input str` to `String`
2. **grammar.lalrpop**: Add `.to_string()` at parse sites for these fields
3. **Fix compilation**: Update all uses across symtab_visitor, type_visitor, IR lowering, etc.

**Compilable**: Yes - no behavior change, just ownership

### Phase 1: Grammar and AST

1. **ast.rs**: Add `SpawnBlock { body: Vec<Statement> }` to `ExpressionKind`
2. **grammar.lalrpop**: Add spawn block production
3. **Tests**: Parser tests for spawn block syntax

**Compilable**: No - will fail at later stages without desugaring

### Phase 2: Desugaring Infrastructure + Spawn Block Pass

1. **desugar.rs**: Create new desugaring module with `DesugarPass` trait and `walk_*` functions
2. **desugar.rs**: Implement `run_desugar_pass` driver
3. **desugar.rs**: Implement `SpawnBlockDesugarer` as a `DesugarPass`
4. **compile.rs**: Integrate `run_desugar_pass(SpawnBlockDesugarer::new(), &mut module)` before symbol resolution
5. **ast.rs**: Add `unreachable!()` for `SpawnBlock` in `all_inner` and other expression matchers
6. **Tests**: Desugaring snapshot tests

**Compilable**: Yes - spawn blocks work end-to-end

### Phase 3: Error Messages

1. **type_visitor.rs**: Detect and report "spawn blocks cannot return values"
2. **symtab_visitor.rs**: Add note about closures when variable undefined in spawn block
3. **Tests**: Error message snapshot tests

**Compilable**: Yes - with improved error messages

### Phase 4: Documentation

1. **tapir-reference.md**: Document spawn block syntax
2. **tapir-reference.md**: Add examples showing spawn blocks vs named functions
3. **tapir-reference.md**: Note about no closure capture

## Example Usage

### Animation Loop

```tapir
property animation_frame: int;

spawn {
    loop {
        animation_frame = animation_frame + 1;
        wait;
    }
};
```

### Timed Event

```tapir
property visible: bool;

# Flash for 60 frames then hide
spawn {
    var i = 0;
    loop {
        if i >= 60 { break; }
        visible = (i / 8) % 2 == 0;
        i = i + 1;
        wait;
    }
    visible = false;
};
```

### Multiple Concurrent Tasks

```tapir
property x: fix;
property y: fix;
property rotation: fix;

# Start three independent animation loops
spawn {
    loop {
        x = x + 0.5;
        wait;
    }
};

spawn {
    loop {
        y = y + sin(frame() * 0.1);
        wait;
    }
};

spawn {
    loop {
        rotation = rotation + 1.0;
        wait;
    }
};
```

### Cancellable Background Task

```tapir
global despawn_timer: task;

despawn_timer = spawn {
    wait 300;
    trigger remove_me();
};

event fn on_collected() {
    despawn_timer.cancel();
    # ... pickup animation ...
}
```

## Future Extensions

### Arguments (Explicit Passing)

Allow passing values into spawn blocks explicitly (not closure capture):

```tapir
var x = 42;
spawn(x) {
    # `x` is a parameter with value 42
    some_global = x;
};
```

This would desugar to:

```tapir
fn __spawn_block_0(x: int) {
    some_global = x;
}
spawn __spawn_block_0(42);
```

Requires grammar extension and type inference for the parameter.

### Spawn Block Return Value (via channel)

A more advanced extension could allow spawn blocks to produce a result:

```tapir
var result = spawn {
    # ... computation ...
    yield 42;
};

wait_for result;
use result.value;  # 42
```

This would require significant VM changes for task result communication.

## Implementation Notes

**Grammar verified (pre-implementation):** Added the spawn block grammar rule and `SpawnBlock` AST variant as a test. The LALRPOP grammar compiled without conflicts, and `spawn { ... }` parsed correctly while `spawn function()` continued to work. The changes were reverted after verification.

**SpawnBlock in downstream passes:** Yes — `SpawnBlock` is fully eliminated by the desugaring pass, so later stages never see it. The type checker, IR lowering, `all_inner`, and any other match on `ExpressionKind` should add an `unreachable!("SpawnBlock should have been desugared")` arm. This:

- Documents the intent clearly
- Catches bugs immediately if desugaring is ever accidentally skipped
- Satisfies exhaustiveness checking without silently ignoring the variant

```rust
// In type_visitor.rs, ir/lowering.rs, all_inner(), etc:
ExpressionKind::SpawnBlock { .. } => {
    unreachable!("SpawnBlock should have been desugared before this point")
}
```
