# Implementation Plan: Spawn Returns Task Handle

## Overview

Modify `spawn` to return a `task` handle that can be used to cancel spawned functions. This enables patterns like cancelling background animations when game state changes.

```tapir
global timer: task;

# Top-level code runs first
timer = spawn despawn_timer();

event fn on_picked_up(hud_x: fix, hud_y: fix) {
    timer.cancel();  # stop the despawn timer
    spawn fly_to_hud(hud_x, hud_y);
}

fn despawn_timer() {
    # wait 300 frames...
    # start flashing...
    trigger remove_me();
}
```

## Design Decisions

### Task Type

`task` is a new builtin type representing a handle to a spawned function:

- Internally stored as a 32-bit unsigned integer (task ID)
- Task ID 0 is the "empty" sentinel (no task)
- Task IDs are monotonically increasing and never reused
- Copyable - multiple variables can hold the same task ID
- Can be stored in globals, locals, and passed to functions

### Spawn Returns Task

`spawn` becomes an expression statement that returns a `task`:

```tapir
var t = spawn some_function();
```

The return value can be ignored:

```tapir
spawn fire_and_forget();  # task value discarded
```

Note: Global initializers must be constant expressions, so you cannot do `global t: task = spawn foo();`. Instead, assign in top-level code:

```tapir
global t: task;
t = spawn foo();
```

### Task Methods

Defined in the prelude:

```tapir
builtin(-2) fn task.cancel(self);
```

The `cancel` method:

- Stops the spawned function if it's still running
- No-op if the task has already finished
- No-op if called on the empty task (ID 0)
- No-op if called with a stale task ID (already finished and removed)

### Uninitialized Globals

To support storing tasks in globals, allow globals without initializers:

```tapir
global timer: task;      # zero-initialized (empty task)
global counter: int;     # zero-initialized (0)
global flag: bool;       # zero-initialized (false)
global pos: fix;         # zero-initialized (0.0)
global point: Point;     # all fields zero-initialized
```

This applies to all types, not just `task`.

### Task ID Management

Task IDs are managed separately from internal thread indices to prevent stale handle issues:

- Task IDs start at 1 and increment monotonically
- Task ID 0 is reserved as the "empty" sentinel
- Each thread stores its own task ID
- When cancelling, linear scan threads to find matching task ID
- If not found, the task already finished - no-op

This ensures that storing a task handle in a global and using it later is always safe - at worst, you cancel something that already finished.

### Cancelling Self

A spawned function can technically cancel itself if it has access to its own task ID via a global:

```tapir
global my_task: task;

my_task = spawn self_cancelling();

fn self_cancelling() {
    # ... do stuff ...
    my_task.cancel();  # cancels self!
}
```

The VM handles this gracefully via a `cancelled` flag:

1. Cancel builtin returns `RunResult::Cancel(task_id)`
2. Outer loop finds the state (including self) and sets `cancelled = true`
3. Current state finishes its instruction normally
4. On next iteration, the cancelled flag is checked and the state is removed

This avoids index management issues during iteration.

## Grammar Changes

### Uninitialized Globals

Extend `GlobalDeclaration` to allow type-only declarations:

```lalrpop
GlobalDeclaration: GlobalVariable<'input> = {
    # Existing: global name = expr;
    "global" <name:Identifier> "=" <value:Expression> ";" => GlobalVariable {
        name,
        value: Some(value),
        ty: None,
    },

    # Existing: global name: type = expr;
    "global" <name:Identifier> ":" <ty:Type> "=" <value:Expression> ";" => GlobalVariable {
        name,
        value: Some(value),
        ty: Some(ty),
    },

    # NEW: global name: type;
    "global" <name:Identifier> ":" <ty:Type> ";" => GlobalVariable {
        name,
        value: None,
        ty: Some(ty),
    },
}
```

### Task Type Keyword

Add `task` as a type keyword:

```lalrpop
TypeIdentifier: Ident<'input> = {
    <start:@L> <ident:identifier> <end:@R> => Ident { ident, span: Span::new(file_id, start, end) },
    <start:@L> "int" <end:@R> => Ident { ident: "int", span: Span::new(file_id, start, end) },
    <start:@L> "fix" <end:@R> => Ident { ident: "fix", span: Span::new(file_id, start, end) },
    <start:@L> "bool" <end:@R> => Ident { ident: "bool", span: Span::new(file_id, start, end) },
    <start:@L> "task" <end:@R> => Ident { ident: "task", span: Span::new(file_id, start, end) },  # NEW
}
```

### Spawn as Expression Statement

Currently `spawn` is a statement. Convert to an expression statement so it can appear in expression position while still being usable as a standalone statement:

```lalrpop
# Spawn becomes an expression
SpawnExpr: Expression<'input> = {
    <start:@L> "spawn" <call:FunctionCall> <end:@R> =>
        ExpressionKind::Spawn { call: Box::new(call) }.with_span(file_id, start, end),
}

# Add to expression rules at appropriate precedence
Term: Expression<'input> = {
    # ... existing terms ...
    SpawnExpr,
}

# Expression statements allow spawn to be used standalone
ExpressionStatement: Statement<'input> = {
    <expr:Expression> ";" => Statement::Expression(expr),
}
```

## AST Changes

### GlobalVariable

Update to support optional initializer:

```rust
#[derive(Clone, Debug, Serialize)]
pub struct GlobalVariable<'input> {
    pub name: Ident<'input>,
    /// The initializer expression. None for uninitialized globals.
    pub value: Option<Expression<'input>>,
    /// The declared type. Required if value is None.
    pub ty: Option<TypeAnnotation<'input>>,
}
```

### Type Enum

Add `Task` variant:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum Type {
    Int,
    Fix,
    Bool,
    Struct(StructId),
    Task,  # NEW
    Error,
}
```

### Spawn Expression

Convert spawn from statement to expression. The AST should have:

```rust
pub enum ExpressionKind<'input> {
    # ... existing variants ...

    Spawn {
        call: Box<Expression<'input>>,  # The function call being spawned
    },
}
```

Remove `Spawn` from `StatementKind`

## Type System Changes

### Type Resolution

In `struct_visitor.rs`, resolve `"task"` to `Type::Task`:

```rust
fn resolve_type_name(name: &str, ...) -> Type {
    match name {
        "int" => Type::Int,
        "fix" => Type::Fix,
        "bool" => Type::Bool,
        "task" => Type::Task,  # NEW
        _ => # look up struct...
    }
}
```

### Spawn Type Checking

In `type_visitor.rs`, `Spawn` expressions return `Type::Task`:

```rust
ExpressionKind::Spawn { call } => {
    # Type check the call (existing logic)
    self.type_for_expression(call, symtab, diagnostics);

    # Spawn always returns a task handle
    Type::Task
}
```

### Global Type Checking

Handle uninitialized globals:

```rust
fn visit_global(&mut self, global: &mut GlobalVariable<'input>, ...) {
    match (&global.value, &global.ty) {
        (Some(value), Some(ty)) => {
            # Existing: check value matches declared type
        }
        (Some(value), None) => {
            # Existing: infer type from value
        }
        (None, Some(ty)) => {
            # NEW: uninitialized global with declared type
            let resolved_ty = resolve_type(&ty, ...);
            global.meta.set(resolved_ty);
        }
        (None, None) => {
            # Error: must have type or initializer
            ErrorKind::GlobalRequiresTypeOrInitializer
                .at(global.name.span)
                .emit(diagnostics);
        }
    }
}
```

## Prelude Changes

Add the task cancel builtin method:

```tapir
builtin(-2) fn task.cancel(self);
```

The builtin ID -2 is special-cased in the VM to return `RunResult::Cancel(task_id)` rather than executing as a normal builtin. This allows the outer execution loop to handle the actual cancellation.

## IR Changes

### Spawn Instruction

Modify `Spawn` IR to include a target for the task ID:

```rust
pub enum TapIr {
    # ... existing variants ...

    /// Spawn a function, storing the task ID in target
    Spawn {
        target: SymbolId,  # Where to store the task ID
        function: FunctionId,
        args: Box<[SymbolId]>,
    },
}
```

### Cancel Builtin

Compiles to `CallBuiltin` with the special builtin ID -2:

```rust
TapIr::CallBuiltin {
    target: dummy_symbol,  # cancel returns nothing
    f: BuiltinFunction::TaskCancel,  # ID -2
    args: Box::new([task_symbol]),
}
```

## Bytecode Changes

### Spawn Opcode

Programs can exceed 2^16 instructions, so the function PC needs more than 16 bits. Use Type1 for registers, followed by a data word for the PC:

```rust
pub enum Opcode {
    # ... existing opcodes ...

    /// Spawn a function, store task ID in target
    /// Type1: target(8), first_arg(8), num_args(8)
    /// Followed by: function PC as 32-bit word (not decoded as opcode)
    Spawn,
}
```

Encoding:

```
[Spawn instruction: Type1 with target, first_arg, num_args]
[Function PC: raw 32-bit word]
```

This follows the `LoadConstant` pattern of reading the next word as data.

### No CancelTask Opcode

`cancel` is implemented as a special-case builtin (-2), not a dedicated opcode. It uses the existing `CallBuiltin` instruction path but is intercepted in the VM to set a cancelled flag rather than executing as a normal builtin.

### Global Zero-Initialization

Globals without initializers should be zero-initialized. This may already happen naturally if global storage is zeroed, or may need explicit handling in bytecode generation to emit zero constants.

## VM Changes

### Task ID Tracking

Add task ID to thread struct and a counter to ScriptState:

```rust
pub struct Thread {
    # ... existing fields ...

    /// Task ID for this thread (0 if not spawned via spawn expression)
    task_id: u32,
}

pub struct ScriptState {
    # ... existing fields ...

    /// Next task ID to assign (starts at 1, 0 is empty sentinel)
    next_task_id: u32,
}
```

With a maximum of ~5 concurrent tasks, there's no need for a HashMap. Just store the task ID in each thread and linear scan when cancelling.

### State Changes

Add `task_id` and `cancelled` fields to `State`:

```rust
pub(crate) struct State {
    pc: usize,
    stack: Vec<i32>,
    stack_offset: usize,
    pub task_id: u32,   # 0 for main thread / threads without handles
    pub cancelled: bool,
}
```

Also need to expose `set_reg` as `pub(crate)` so the outer loop can set the target register when handling `Spawn`.

### Cancellation Strategy

Rather than removing states during iteration (which complicates index management and self-cancellation), cancellation marks the state:

1. When `cancel(task_id)` is called, find the state with matching `task_id` and set `cancelled = true`
2. Before running a state, check if `cancelled` - if so, treat as `Finished` and remove it
3. This handles self-cancellation cleanly: the current state finishes its instruction, returns, and is removed on the next iteration

No new `RunResult` variant needed - just a flag check in the outer loop.

### Spawn Instruction Handler

Pass `&mut next_task_id` to `run_until_wait` so the spawn handler can assign IDs directly:

```rust
pub(crate) fn run_until_wait(
    &mut self,
    bytecode: &[u32],
    properties: &mut dyn ObjectSafeProperties,
    frame: i32,
    globals: &mut [i32],
    next_task_id: &mut u32,  # NEW
) -> RunResult {
    # ...

    O::Spawn => {
        type1!(target, first_arg, num_args);

        # Read function PC from next word (like LoadConstant)
        let function_pc = bytecode[self.pc] as usize;
        self.pc += 1;

        # ... existing stack copying logic ...

        # Assign task ID
        let task_id = *next_task_id;
        *next_task_id += 1;

        let mut new_state = State::new(function_pc, new_stack);
        new_state.task_id = task_id;

        # Store task ID in caller's target register
        self.set_reg(target, task_id as i32);

        return RunResult::Spawn(Box::new(new_state));
    }
}
```

This keeps the spawn logic self-contained in `State` while `next_task_id` lives in `Vm`.

### Cancel Builtin Handler

Special-case builtin ID -2 to mark the target state as cancelled:

```rust
O::CallBuiltin => {
    type1!(target, builtin_id, first_arg);
    let builtin_id = builtin_id as i8 as i16;

    # Special case for task.cancel()
    if builtin_id == -2 {
        let task_id = self.get_reg(first_arg) as u32;
        # Mark for cancellation - actual removal happens in outer loop
        # Need access to states list here, so this is handled via
        # a callback or by returning a Cancel result
        return RunResult::Cancel(task_id);
    }

    # ... existing builtin dispatch ...
}
```

Add `Cancel` variant to `RunResult`:

```rust
pub(crate) enum RunResult {
    Waiting,
    Finished,
    Spawn(Box<State>),  # unchanged - state already has task_id set
    Cancel(u32),        # task_id to mark as cancelled
}
```

### Outer Loop Handling

In `Vm::run_until_wait`:

```rust
fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
    let mut state_index = 0;
    while state_index < self.states.len() {
        # Check if this state was cancelled
        if self.states[state_index].cancelled {
            self.states.swap_remove(state_index);
            continue;  # Don't increment, check the swapped-in state
        }

        match self.states[state_index].run_until_wait(
            self.bytecode,
            properties,
            self.frame,
            &mut self.globals,
            &mut self.next_task_id,  # pass task ID counter
        ) {
            RunResult::Waiting => {
                state_index += 1;
            }
            RunResult::Finished => {
                self.states.swap_remove(state_index);
            }
            RunResult::Spawn(state) => {
                # State already has task_id set, caller's register already set
                self.states.push(*state);
                # continue running same state (don't increment)
            }
            RunResult::Cancel(task_id) => {
                if task_id != 0 {
                    # Find state with matching task_id and mark as cancelled
                    for state in &mut self.states {
                        if state.task_id == task_id {
                            state.cancelled = true;
                            break;
                        }
                    }
                }
                # Continue running same state (don't increment)
                # If we cancelled self, we'll catch it on next iteration
            }
        }
    }

    self.frame += 1;
}
```

This approach:

- Passes `next_task_id` into `run_until_wait` so spawn can assign IDs directly
- Marks states as cancelled rather than removing mid-iteration
- Checks for cancelled states before running them
- Handles self-cancellation: state marks itself, finishes current instruction, loop continues, next iteration sees cancelled flag and removes it

## Diagnostic Changes

### Error Types

```rust
pub enum ErrorKind {
    # ... existing variants ...

    /// Global declaration requires either a type annotation or an initializer
    GlobalRequiresTypeOrInitializer,

    /// Cannot spawn a non-function
    SpawnRequiresFunction,
}
```

### Example Error Messages

```
error: global declaration requires type annotation or initializer
  --> script.tapir:3:1
   |
 3 | global foo;
   | ^^^^^^^^^^
   |
   = help: add a type: `global foo: int;` or an initializer: `global foo = 0;`
```

## Implementation Order

### Phase 1: Task Type

1. **ast.rs**: Add `Type::Task` variant
2. **struct_visitor.rs**: Resolve `"task"` to `Type::Task`
3. **grammar.lalrpop**: Add `"task"` to `TypeIdentifier`
4. **type_visitor.rs**: Handle `Type::Task` in type checking
5. **Tests**: Task type parses and type-checks

**Compilable**: Yes - task type exists but spawn doesn't return it yet

### Phase 2: Uninitialized Globals

1. **ast.rs**: Make `GlobalVariable::value` optional
2. **grammar.lalrpop**: Add `global name: type;` production
3. **symtab_visitor.rs**: Handle uninitialized globals
4. **type_visitor.rs**: Type check uninitialized globals
5. **lowering.rs**: Generate zero-initialization for uninitialized globals
6. **Tests**: Uninitialized globals work for all types

**Compilable**: Yes - uninitialized globals work

### Phase 3: Spawn as Expression

1. **ast.rs**: Move `Spawn` from `StatementKind` to `ExpressionKind`
2. **grammar.lalrpop**: Parse spawn as expression, allow as expression statement
3. **type_visitor.rs**: `Spawn` returns `Type::Task`
4. **ir.rs**: Add target to `Spawn` IR
5. **lowering.rs**: Generate `Spawn` with target
6. **Tests**: `var t = spawn foo();` works

**Compilable**: Yes - spawn returns task but cancel doesn't exist

### Phase 4: Task ID Tracking in VM

1. **vm/lib.rs**: Add `next_task_id` counter to `Vm`
2. **vm/state.rs**: Add `task_id` field to `State`
3. **bytecode**: Update Spawn opcode encoding (Type1 + data word for function PC)
4. **vm/state.rs**: Update spawn handler to read function PC from data word
5. **vm/lib.rs**: Assign task IDs in outer loop when handling `RunResult::Spawn`
6. **vm/state.rs**: Store task ID in target register
7. **Tests**: Task IDs are assigned correctly

**Compilable**: Yes - VM tracks task IDs

### Phase 5: Cancel Method

1. **prelude**: Add `builtin(-2) fn task.cancel(self);`
2. **vm/state.rs**: Add `cancelled: bool` field to `State`
3. **vm/state.rs**: Add `RunResult::Cancel(u32)` variant
4. **vm/state.rs**: Special-case builtin -2 to return `RunResult::Cancel`
5. **vm/lib.rs**: Handle `RunResult::Cancel` by marking state as cancelled
6. **vm/lib.rs**: Check cancelled flag before running each state
7. **Tests**: Cancel works on running, finished, empty, and self tasks

**Compilable**: Yes - full feature complete

### Phase 6: Documentation

1. **tapir-reference.md**: Document `task` type
2. **tapir-reference.md**: Document `spawn` returning task
3. **tapir-reference.md**: Document `task.cancel()`
4. **tapir-reference.md**: Document uninitialized globals
5. **bytecode-reference.md**: Update Spawn opcode encoding (Type1 + data word)

## Testing Strategy

### Parser Tests

- `global foo: task;` parses correctly
- `global foo: int;` parses correctly
- `global foo: Point;` parses correctly
- `var t = spawn foo();` parses as expression
- `spawn foo();` still works as statement (discarded result)

### Type Checking Tests

- `spawn foo()` has type `task`
- `var t: task = spawn foo();` type checks
- `global timer: task;` type checks
- `timer.cancel();` type checks (task has cancel method)
- `var x: int = spawn foo();` fails (task != int)

### IR Tests

- `var t = spawn foo();` generates Spawn with target
- `spawn foo();` generates Spawn with temporary target (discarded)
- `t.cancel()` generates CallBuiltin for TaskCancel (-2)

### Runtime Tests

- Spawned function runs and task ID is non-zero
- `cancel` on running task stops it
- `cancel` on finished task is no-op
- `cancel` on empty task (0) is no-op
- Task ID uniqueness: spawn, cancel, spawn again gives different ID
- Stale handle: spawn, wait for finish, cancel is no-op
- Global task: store in global, cancel from event handler

### Edge Case Tests

- Spawning many tasks doesn't overflow (or handles overflow gracefully)
- Cancelling from within a spawned function (cancels sibling)
- Self-cancel: task cancels itself via global handle
- Multiple handles to same task: copy task, cancel via either handle works

## Example Code

```tapir
property x: fix;
property y: fix;
property visible: bool;

global despawn_task: task;
global bob_task: task;

# Top-level code runs first
# Fall and bounce logic here...

# Start background tasks
despawn_task = spawn despawn_timer();
bob_task = spawn bob_animation();

fn despawn_timer() {
    var i = 0;
    loop {
        if i >= 300 { break; }
        i = i + 1;
        wait;
    }

    # Start flashing
    var j = 0;
    loop {
        if j >= 120 { break; }
        visible = (j / 8) % 2 == 0;
        j = j + 1;
        wait;
    }

    trigger RemoveMe();
}

fn bob_animation() {
    loop {
        y = y + sin(frame() * 0.1) * 0.5;
        wait;
    }
}

event fn on_picked_up(hud_x: fix, hud_y: fix) {
    # Cancel background tasks
    despawn_task.cancel();
    bob_task.cancel();

    # Start pickup animation
    spawn fly_to_hud(hud_x, hud_y);
}

fn fly_to_hud(target_x: fix, target_y: fix) {
    loop {
        x = x + (target_x - x) * 0.1;
        y = y + (target_y - y) * 0.1;

        # Close enough?
        var dx = target_x - x;
        var dy = target_y - y;
        if dx * dx + dy * dy < 1.0 {
            break;
        }
        wait;
    }

    trigger ApplyPickup();
}
```

## Future Extensions

### task.is_running()

Check if a task is still active:

```tapir
builtin(-3) fn task.is_running(self) -> bool;

if timer.is_running() {
    # still waiting...
}
```

Would require passing state information into `run_until_wait` or adding another `RunResult` variant that returns a value to the calling state.

### wait N

Syntactic sugar for waiting multiple frames:

```tapir
wait 30;  # Desugars to: var _i = 0; loop { if _i >= 30 { break; } _i = _i + 1; wait; }
```

Could be implemented as an AST desugaring pass.

### Compound Assignment

```tapir
i += 1;  # Desugars to: i = i + 1;
```

### lerp Builtin

```tapir
builtin fn lerp(a: fix, b: fix, t: fix) -> fix;

x = lerp(x, target_x, 0.1);
```

## Implementation Notes

This section will document differences between the plan and actual implementation.
