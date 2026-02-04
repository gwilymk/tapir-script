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

The synthetic function is always top-level, but the spawn *call* stays in place. This works because spawn blocks have no closure capture - the body only sees globals and properties regardless of where the block appears in source.

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

Synthetic functions use the naming pattern `__spawn_block_<N>` where N is a unique counter. The double underscore prefix:
- Indicates compiler-generated code
- Is reserved (user functions starting with `__` could be disallowed)
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

## Desugaring Pass

**File:** `compiler/src/compile/desugar.rs` (new file)

Create a desugaring pass that runs after parsing but before symbol resolution. The pass:

1. Walks the AST looking for `SpawnBlock` expressions
2. For each spawn block:
   a. Generate a unique function name
   b. Create a synthetic `FnDeclaration` with the block's body
   c. Replace the `SpawnBlock` with a `Spawn { call }` expression
3. Collect all synthetic functions
4. Add them to the module's function declarations

```rust
pub struct Desugarer {
    spawn_block_counter: u32,
    synthetic_functions: Vec<TopLevelStatement<'input>>,
}

impl Desugarer {
    pub fn desugar_module(&mut self, module: &mut Module<'input>) {
        // Walk all top-level statements and functions
        for stmt in &mut module.statements {
            self.desugar_statement(stmt);
        }

        // Append synthetic functions to the module
        module.statements.extend(self.synthetic_functions.drain(..));
    }

    fn desugar_expression(&mut self, expr: &mut Expression<'input>) {
        match &mut expr.kind {
            ExpressionKind::SpawnBlock { body } => {
                // Generate unique function name
                let name = format!("__spawn_block_{}", self.spawn_block_counter);
                self.spawn_block_counter += 1;

                // Create synthetic function
                let fn_decl = FnDeclaration {
                    name: /* synthetic ident with name */,
                    parameters: vec![],
                    return_type: None,
                    body: std::mem::take(body),
                    // ...
                };

                self.synthetic_functions.push(TopLevelStatement::Fn(fn_decl));

                // Replace SpawnBlock with Spawn { call }
                expr.kind = ExpressionKind::Spawn {
                    call: Box::new(ExpressionKind::Call {
                        name: /* synthetic ident with name */,
                        arguments: vec![],
                    }.with_span(expr.span.file_id, ...))
                };
            }
            // Recurse into other expressions...
            _ => { /* visit children */ }
        }
    }
}
```

### Span Handling

Synthetic functions need spans for error reporting. Use the span of the original `spawn` block:

- Function declaration span: entire `spawn { ... }` expression
- Function name span: the `spawn` keyword position

This ensures errors inside spawn blocks point to the correct source location.

## Integration Point

**File:** `compiler/src/compile/compile.rs`

Run desugaring after parsing, before symbol resolution:

```rust
pub fn compile(source: &str, ...) -> Result<...> {
    // 1. Parse
    let mut module = parse(source)?;

    // 2. NEW: Desugar spawn blocks into synthetic functions
    let mut desugarer = Desugarer::new();
    desugarer.desugar_module(&mut module);

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
fn __spawn_block_0() {
    wait;
}
spawn __spawn_block_0();
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

### Phase 1: Grammar and AST

1. **ast.rs**: Add `SpawnBlock { body: Vec<Statement> }` to `ExpressionKind`
2. **grammar.lalrpop**: Add spawn block production
3. **Tests**: Parser tests for spawn block syntax

**Compilable**: No - will fail at later stages without desugaring

### Phase 2: Desugaring Pass

1. **desugar.rs**: Create new desugaring module
2. **desugar.rs**: Implement AST walker for expressions and statements
3. **desugar.rs**: Transform `SpawnBlock` into synthetic function + spawn call
4. **compile.rs**: Integrate desugaring pass before symbol resolution
5. **Tests**: Desugaring snapshot tests

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
