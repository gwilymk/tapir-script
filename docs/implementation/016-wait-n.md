# Implementation Plan: Wait N Frames

## Overview

Extend the `wait` statement to optionally take an expression that evaluates to an `int`, causing the script to wait that many frames before continuing.

```tapir
wait;       # wait 1 frame (existing behavior)
wait 30;    # wait 30 frames
wait n;     # wait n frames (where n is an int variable or expression)
wait 0;     # no-op, continue immediately
```

This provides a cleaner syntax for common animation patterns without requiring manual loop construction.

## Design Decisions

### Syntax

The optional expression follows the `wait` keyword:

```tapir
wait;           # wait 1 frame
wait <expr>;    # wait <expr> frames
```

The expression must evaluate to `int`. If the value is:
- Positive: wait that many frames
- Zero: no-op, continue execution immediately
- Negative: no-op, continue execution immediately (defensive, same as zero)

### Semantics

`wait n;` is semantically equivalent to:

```tapir
var _i = 0;
loop {
    if _i >= n { break; }
    _i = _i + 1;
    wait;
}
```

But implemented more efficiently at the VM level.

### Implementation Approach: VM-Level Counter

Rather than desugaring `wait n;` into a loop (which bloats bytecode), we implement it with:

1. The existing `Wait` opcode, extended to use its previously-unused fields
2. A `wait_remaining` counter in the VM's `State` struct
3. Early-exit logic in `run_until_wait` that decrements and returns Waiting

This approach:
- Produces compact bytecode (2 instructions vs ~10+ for a loop)
- Keeps the common `wait;` case unchanged (single instruction, all zeros)
- No new opcode needed - we reuse `Wait` with a flag in the `a` field
- Runtime overhead is minimal (one counter check per frame)

### Bytecode Encoding

The `Wait` instruction uses Type1 format with fields `(target, a, b)`:
- `target`: register containing frame count (only read if `a != 0`)
- `a`: has_frames flag (0 = wait 1 frame, nonzero = read from target)
- `b`: unused

We need the flag because r0 is a valid register - we can't use `target == 0` as a sentinel for "no frames argument".

## Grammar Changes

**File:** `compiler/src/grammar.lalrpop`

Update the wait statement rule to accept an optional expression:

```lalrpop
StatementKind: StatementKind<'input> = {
    // ...existing rules...

    // Wait with optional frame count
    wait ";" => StatementKind::Wait { frames: None },
    wait <frames: Expression> ";" => StatementKind::Wait { frames: Some(frames) },

    // ...remaining rules...
}
```

## AST Changes

**File:** `compiler/src/ast.rs`

Update `StatementKind::Wait` to hold an optional expression:

```rust
pub enum StatementKind<'input> {
    // ...existing variants...

    Wait {
        /// Number of frames to wait. None means 1 frame (original behavior).
        frames: Option<Expression<'input>>,
    },

    // ...remaining variants...
}
```

## Type System Changes

**File:** `compiler/src/compile/type_visitor.rs`

Type-check the frames expression when present:

```rust
StatementKind::Wait { frames } => {
    if let Some(frames_expr) = frames {
        let frames_ty = self.type_for_expression(frames_expr, symtab, diagnostics);
        if frames_ty != Type::Int && frames_ty != Type::Error {
            ErrorKind::TypeMismatch {
                expected: Type::Int,
                got: frames_ty,
            }
            .at(frames_expr.span)
            .emit(diagnostics);
        }
    }
}
```

## IR Changes

**File:** `compiler/src/compile/ir.rs`

Update the `Wait` variant to take an optional frame count:

```rust
pub enum TapIr {
    // ...existing variants...

    /// Wait for frames. None means 1 frame.
    Wait { frames: Option<SymbolId> },

    // ...remaining variants...
}
```

Update the `targets()`, `sources()`, and `could_have_side_effects()` implementations:

```rust
impl TapIr {
    pub fn targets(&self) -> impl Iterator<Item = SymbolId> {
        // Wait has no targets
    }

    pub fn sources(&self) -> impl Iterator<Item = SymbolId> {
        match self {
            TapIr::Wait { frames: Some(f) } => Some(*f),
            TapIr::Wait { frames: None } => None,
            // ...
        }
    }

    pub fn could_have_side_effects(&self) -> bool {
        match self {
            TapIr::Wait { .. } => true,
            // ...
        }
    }
}
```

**File:** `compiler/src/compile/ir/lowering.rs`

Update the lowering for Wait statements:

```rust
ast::StatementKind::Wait { frames } => {
    let frames_symbol = frames.as_ref().map(|expr| self.visit_expression(expr, symtab));
    self.current_block.push(TapIr::Wait { frames: frames_symbol });
}
```

## Bytecode Changes

**File:** `bytecode/src/lib.rs`

No new opcode needed. We extend the existing `Wait` instruction to use its unused fields:

```
Wait instruction (Type1):
- opcode: Wait
- target: frames register (only meaningful if a != 0)
- a: has_frames flag (0 = wait 1 frame, nonzero = read count from target)
- b: unused
```

This approach:
- Keeps backwards compatibility (existing `Wait` with all zeros still works)
- Avoids wasting opcode space
- r0 is a valid register, so we need the flag to distinguish "wait 1" from "wait value-in-r0"

Update the constructor:

```rust
impl Type1 {
    // Existing: wait 1 frame
    pub const fn wait() -> Self {
        Self::new0(Opcode::Wait)
    }

    // NEW: wait N frames (read count from register)
    pub const fn wait_n(frames_reg: u8) -> Self {
        Self::new2(Opcode::Wait, frames_reg, 1)  // a=1 means "has frames"
    }
}
```

## Compiler Bytecode Emission

**File:** `compiler/src/compile.rs`

Update bytecode emission for `Wait`:

```rust
TapIr::Wait { frames } => {
    match frames {
        None => self.bytecode.wait(),
        Some(f) => self.bytecode.wait_n(v(f)),
    }
}
```

Also add the helper method to `ByteCode`:

```rust
impl ByteCode {
    fn wait_n(&mut self, frames_reg: u8) {
        self.data.push(Type1::wait_n(frames_reg).encode());
    }
}
```

## VM Changes

### State Structure

**File:** `vm/src/state.rs`

Add a counter to track remaining wait frames:

```rust
#[derive(Debug)]
pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
    stack_offset: usize,
    pub task_id: u32,
    pub cancelled: bool,
    /// Remaining frames to wait (0 = not waiting)
    wait_remaining: i32,
}
```

Update constructors to initialize `wait_remaining: 0`.

### Instruction Dispatch

**File:** `vm/src/state.rs`

Add early-exit check at the start of `run_until_wait`:

```rust
pub(crate) fn run_until_wait(
    &mut self,
    bytecode: &[u32],
    properties: &mut dyn ObjectSafeProperties,
    frame: i32,
    globals: &mut [i32],
    next_task_id: &mut u32,
) -> RunResult {
    // Check if still waiting from a previous WaitN
    if self.wait_remaining > 0 {
        self.wait_remaining -= 1;
        return RunResult::Waiting;
    }

    loop {
        // ...existing instruction dispatch...
    }
}
```

Update the `Wait` opcode handler to check for frames argument:

```rust
O::Wait => {
    type1!(frames_reg, has_frames);

    if has_frames == 0 {
        // Simple wait 1 frame (original behavior)
        return RunResult::Waiting;
    }

    // Wait N frames
    let frames = self.get_reg(frames_reg);

    if frames <= 0 {
        // wait 0 or negative = no-op, continue execution
    } else if frames == 1 {
        // wait 1 = same as regular wait
        return RunResult::Waiting;
    } else {
        // wait N where N > 1: wait this frame, then N-1 more frames
        self.wait_remaining = frames - 1;
        return RunResult::Waiting;
    }
}
```

## Testing Strategy

### Parser Tests

```tapir
# Basic syntax
wait;
wait 10;
wait n;
wait n + 5;
wait some_function();
```

### Type Checking Tests

```tapir
# Valid
var n: int = 10;
wait n;
wait 30;
wait n * 2;

# Invalid - type mismatch
var f: fix = 1.5;
wait f;  # ERROR: expected int, got fix

var b: bool = true;
wait b;  # ERROR: expected int, got bool
```

### IR Generation Tests

Snapshot tests showing:
- `wait;` generates `TapIr::Wait { frames: None }`
- `wait 10;` generates load constant + `TapIr::Wait { frames: Some(symbol) }`
- `wait n;` generates `TapIr::Wait { frames: Some(n_symbol) }`

### Runtime Tests

```tapir
# Test wait 0 is no-op
property frame_at_end: int;
frame_at_end = frame();
wait 0;
frame_at_end = frame();  # Should still be same frame
```

```tapir
# Test wait 1 same as wait
property f1: int;
property f2: int;

f1 = frame();
wait;
f2 = frame();
# f2 - f1 should equal 1
```

```tapir
# Test wait N
property start: int;
property end: int;

start = frame();
wait 5;
end = frame();
# end - start should equal 5
```

```tapir
# Test wait with expression
property result: int;
var n = 3;
var start = frame();
wait n + 2;  # wait 5 frames
result = frame() - start;  # should be 5
```

```tapir
# Test negative wait is no-op
property before: int;
property after: int;
before = frame();
wait -10;
after = frame();
# before == after
```

### Edge Cases

- `wait 0;` - no-op
- `wait -1;` - no-op (defensive)
- `wait 1;` - same as `wait;`
- Very large wait values (ensure no overflow issues)
- Wait in spawned tasks
- Multiple concurrent tasks with different wait durations

## Implementation Order

### Phase 1: AST and Grammar

1. **ast.rs**: Change `Wait` from unit variant to struct with `frames: Option<Expression>`
2. **grammar.lalrpop**: Add `wait <expr> ";"` production
3. **Tests**: Verify parsing works

**Compilable**: Yes (existing code paths handle None case)

### Phase 2: Type Checking

1. **type_visitor.rs**: Type-check the frames expression
2. **Tests**: Verify type errors for non-int expressions

**Compilable**: Yes

### Phase 3: IR

1. **ir.rs**: Change `Wait` to `Wait { frames: Option<SymbolId> }`
2. **ir.rs**: Update `targets()`, `sources()`, `could_have_side_effects()`
3. **lowering.rs**: Generate `Wait { frames }` with optional symbol
4. **Tests**: Snapshot tests for IR generation

**Compilable**: Yes (but frames won't emit to bytecode yet)

### Phase 4: Bytecode

1. **bytecode/src/lib.rs**: Add `Type1::wait_n()` constructor (reuses `Wait` opcode)
2. **compile.rs**: Emit `wait_n` when frames is Some
3. **Tests**: Verify bytecode encoding

**Compilable**: Yes (but VM won't handle the flag yet)

### Phase 5: VM

1. **state.rs**: Add `wait_remaining: i32` to `State`
2. **state.rs**: Add early-exit check at start of `run_until_wait`
3. **state.rs**: Handle `O::WaitN` opcode
4. **Tests**: Full integration tests

**Compilable**: Yes - feature complete

### Phase 6: Documentation

1. **tapir-reference.md**: Document `wait <expr>;` syntax
2. **bytecode-reference.md**: Document `WaitN` opcode

## Example Usage

```tapir
property x: fix;
property y: fix;
property visible: bool;

# Fade in over 30 frames
fn fade_in() {
    var i = 0;
    loop {
        if i >= 30 { break; }
        # Set alpha based on i/30...
        i = i + 1;
        wait;
    }
}

# Same thing, more concise with wait N
fn fade_in_v2() {
    # Setup fade...
    wait 30;
    # Fade complete
}

# Wait for a variable duration
fn wait_for_animation(duration: int) {
    wait duration;
}

# Practical example: despawn timer with flashing
fn despawn_timer() {
    wait 300;  # Wait 5 seconds at 60fps

    # Flash for 2 seconds
    var i = 0;
    loop {
        if i >= 120 { break; }
        visible = (i / 8) % 2 == 0;
        i = i + 1;
        wait;
    }

    trigger remove_me();
}
```

## Future Extensions

### wait_until

A variant that waits until a condition is true:

```tapir
wait_until x > 100;  # Desugars to: loop { if x > 100 { break; } wait; }
```

### Timeout with condition

```tapir
wait 60 until x > 100;  # Wait up to 60 frames or until condition
```

These would require additional grammar and possibly new IR instructions.

## Implementation Notes

This section will document differences between the plan and actual implementation.
