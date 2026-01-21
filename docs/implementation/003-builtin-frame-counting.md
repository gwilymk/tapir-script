# Built-in Frame Counting

## Overview

This document outlines the implementation plan for a built-in `frame` variable in tapir-script. The `frame` variable provides scripts with access to the current frame index, which increments each time `Script::run()` is called. This enables frame-based timing for animations, cooldowns, and sequencing without requiring the host to expose a property.

## Motivation

Many game scripting scenarios require knowing the current frame:

```tapir
# Wait until a specific frame
while frame < 60 {
    wait;
}

# Frame-based animation timing
var start_frame = frame;
while frame - start_frame < 30 {
    # animate over 30 frames
    wait;
}

# Periodic actions
if frame %% 10 == 0 {
    # every 10 frames
}
```

Currently, achieving this requires the host to define a property and manually increment it each frame. A built-in `frame` variable provides this functionality out of the box.

## Design Decisions

### Scope: Global Frame Counter

The `frame` variable represents a **global** frame count maintained by the `Vm` struct, not per-`State`. This ensures:

- All concurrent threads see the same frame value
- Frame count matches the outer game loop's conception of "frame"
- Spawned threads immediately see the correct current frame

### Increment Timing

Frame count increments at the **end** of each `Script::run()` call, after all bytecode has executed:

```
run() called → execute until wait → frame += 1 → return
run() called → execute until wait → frame += 1 → return
```

This means on the first `run()` call, `frame` equals 0. This is useful when `frame` is used as an index into animation data - the first frame of an animation is frame 0.

### Read-Only Access

The `frame` variable is **read-only**. Scripts cannot assign to it:

```tapir
frame = 10;  # ERROR: cannot assign to built-in variable 'frame'
```

### Type

`frame` has type `int` (32-bit signed integer). This provides approximately 1.2 years of runtime at 60 FPS before overflow, which is sufficient for game scripting (especially on GBA where this will run).

### Implementation Approach: New Bytecode Instruction

Rather than treating `frame` as a special property (which would require reserving a property index), we add a dedicated `GetFrame` bytecode instruction. This:

- Avoids collision with user-defined properties
- Makes the intent clear in bytecode
- Allows future built-in variables (e.g., `local_frame`) to follow the same pattern

## Current State

### What Exists

- **VM**: `Vm` struct manages execution, `State` handles individual threads
- **Properties**: Properties use `GetProp`/`SetProp` bytecode with index lookup
- **Symbol Resolution**: `NameTable` maps identifiers to `SymbolId`
- **Type System**: Three types: `int`, `fix`, `bool`

### What's Missing

- Reserved symbol ID space for built-in variables
- Compiler support for read-only built-in variables
- `GetFrame` bytecode instruction
- Frame counter in VM
- Error for attempting to assign to built-ins

## Implementation Plan

### Phase 1: VM Frame Counter

**Files**: `vm/src/lib.rs`, `vm/src/state.rs`

1. Add frame counter to `Vm` struct:

   ```rust
   struct Vm<'a> {
       bytecode: &'a [u32],
       states: Vec<State>,
       frame: u32,  // NEW
   }

   impl<'a> Vm<'a> {
       pub fn new(bytecode: &'a [u32]) -> Self {
           Self {
               bytecode,
               states: vec![State::new(0, vec![-1])],
               frame: 0,  // Starts at 0
           }
       }
   }
   ```

2. Increment frame at the end of `run_until_wait`:

   ```rust
   fn run_until_wait(&mut self, properties: &mut dyn ObjectSafeProperties) {
       let mut state_index = 0;
       while state_index < self.states.len() {
           // ... existing loop ...
       }

       self.frame += 1;  // NEW: increment after execution
   }
   ```

3. Pass frame to `State::run_until_wait`:

   ```rust
   // In Vm::run_until_wait
   match self.states[state_index].run_until_wait(self.bytecode, properties, self.frame) {
   ```

   ```rust
   // In State
   pub(crate) fn run_until_wait(
       &mut self,
       bytecode: &[u32],
       properties: &mut dyn ObjectSafeProperties,
       frame: u32,  // NEW
   ) -> RunResult {
   ```

### Phase 2: Bytecode Instruction

**Files**: `bytecode/src/lib.rs`

1. Add `GetFrame` opcode:

   ```rust
   pub enum Opcode {
       // ... existing opcodes ...
       GetFrame,
   }
   ```

2. Add `Type1::get_frame` constructor:

   ```rust
   impl Type1 {
       pub const fn get_frame(target: u8) -> Self {
           Self::new1(Opcode::GetFrame, target)
       }
   }
   ```

3. Update disassembler (if present) to handle `GetFrame`.

### Phase 3: VM Instruction Dispatch

**Files**: `vm/src/state.rs`

1. Handle `GetFrame` opcode in the match:

   ```rust
   O::GetFrame => {
       type1!(target);
       self.set_reg(target, frame as i32);
   }
   ```

### Phase 4: Compiler - Built-in Variable Infrastructure

**Files**: `compiler/src/compile/symtab_visitor.rs`, new file `compiler/src/builtins.rs`

Rather than hard-coding checks for `"frame"` throughout the compiler, we create a generic infrastructure for built-in variables that can be extended for future additions like `local_frame`.

1. Create a `BuiltinVariable` enum and reserved symbol ID space:

   ```rust
   // compiler/src/builtins.rs

   use crate::ast::SymbolId;
   use crate::Type;

   /// Built-in variables provided by the runtime.
   /// These use reserved SymbolIds with the MSB set.
   #[derive(Clone, Copy, Debug, PartialEq, Eq)]
   pub enum BuiltinVariable {
       Frame,
       // Future: LocalFrame,
   }

   impl BuiltinVariable {
       /// Reserved symbol IDs have the MSB set
       const RESERVED_BIT: usize = 1 << (usize::BITS - 1);

       pub const fn symbol_id(self) -> SymbolId {
           SymbolId(Self::RESERVED_BIT | (self as usize))
       }

       pub fn from_symbol_id(id: SymbolId) -> Option<Self> {
           if id.0 & Self::RESERVED_BIT == 0 {
               return None;
           }
           match id.0 & !Self::RESERVED_BIT {
               0 => Some(BuiltinVariable::Frame),
               _ => None,
           }
       }

       pub fn from_name(name: &str) -> Option<Self> {
           match name {
               "frame" => Some(BuiltinVariable::Frame),
               _ => None,
           }
       }

       pub const fn name(self) -> &'static str {
           match self {
               BuiltinVariable::Frame => "frame",
           }
       }

       pub const fn ty(self) -> Type {
           match self {
               BuiltinVariable::Frame => Type::Int,
           }
       }

       pub const fn is_writable(self) -> bool {
           match self {
               BuiltinVariable::Frame => false,
           }
       }
   }

   pub fn is_reserved_symbol(id: SymbolId) -> bool {
       id.0 & BuiltinVariable::RESERVED_BIT != 0
   }
   ```

2. Update `NameTable::get` to recognize built-ins:

   ```rust
   pub fn get(&self, name: &str) -> Option<SymbolId> {
       // Check built-ins first
       if let Some(builtin) = BuiltinVariable::from_name(name) {
           return Some(builtin.symbol_id());
       }
       // ... existing lookup logic ...
   }
   ```

3. In `SymTabVisitor`, when processing variable declarations, check for reserved names:

   ```rust
   ast::StatementKind::VariableDeclaration { ident, .. } => {
       if BuiltinVariable::from_name(ident).is_some() {
           self.diagnostics.add_message(
               CompilerErrorKind::CannotShadowBuiltin { name: ident },
               statement.span,
           );
           return;
       }
       // ... existing logic ...
   }
   ```

### Phase 5: Compiler - Type Checking

**Files**: `compiler/src/compile/type_visitor.rs`

1. Handle built-in symbols when resolving variable types:

   ```rust
   fn type_for_symbol(&self, symbol: SymbolId) -> Option<Type> {
       if let Some(builtin) = BuiltinVariable::from_symbol_id(symbol) {
           return Some(builtin.ty());
       }
       // ... existing lookup ...
   }
   ```

2. Reject assignment to read-only built-ins:

   ```rust
   ast::StatementKind::Assignment { ident, .. } => {
       if let Some(builtin) = BuiltinVariable::from_name(ident) {
           if !builtin.is_writable() {
               self.diagnostics.add_message(
                   CompilerErrorKind::CannotAssignToBuiltin { name: builtin.name() },
                   statement.span,
               );
               return;
           }
       }
       // ... existing logic ...
   }
   ```

3. Add new error variants:

   ```rust
   pub enum CompilerErrorKind {
       // ... existing variants ...
       CannotShadowBuiltin { name: &'static str },
       CannotAssignToBuiltin { name: &'static str },
   }
   ```

### Phase 6: Compiler - IR Generation

**Files**: `compiler/src/compile/ir.rs`

1. Add new IR instruction:

   ```rust
   pub enum TapIrInstr {
       // ... existing instructions ...
       GetBuiltin { target: SymbolId, builtin: BuiltinVariable },
   }
   ```

2. Handle built-in variables in expression IR generation:

   ```rust
   ast::ExpressionKind::Variable(_) => {
       let source = *expr.meta.get().expect("Should've resolved variable");

       if let Some(builtin) = BuiltinVariable::from_symbol_id(source) {
           self.current_block.push(TapIr {
               instr: TapIrInstr::GetBuiltin { target: target_symbol, builtin },
           });
       } else if let Some(property) = symtab.get_property(source) {
           self.current_block.push(TapIr {
               instr: TapIrInstr::GetProp {
                   target: target_symbol,
                   prop_index: property.index,
               },
           });
       } else {
           self.current_block.push(TapIr {
               instr: TapIrInstr::Move {
                   target: target_symbol,
                   source,
               },
           });
       }
   }
   ```

3. Update IR instruction implementations:

   ```rust
   impl TapIrInstr {
       fn targets(&self) -> Box<[SymbolId]> {
           match self {
               // ... existing arms ...
               TapIrInstr::GetBuiltin { target, .. } => Box::new([*target]),
           }
       }

       fn sources(&self) -> Box<[SymbolId]> {
           match self {
               // ... existing arms ...
               TapIrInstr::GetBuiltin { .. } => Box::new([]),
           }
       }
   }
   ```

**Note on optimizations**: `GetBuiltin` for `frame` behaves similarly to `GetProp` - the value cannot change between wait points within a single `run()` call. The existing optimization passes that handle property loads should apply the same logic here, loading `frame` once per wait-bounded section and reusing the value.

### Phase 7: Compiler - Bytecode Emission

**Files**: `compiler/src/compile.rs`

1. Emit bytecode for built-ins:

   ```rust
   TapIrInstr::GetBuiltin { target, builtin } => {
       match builtin {
           BuiltinVariable::Frame => {
               self.bytecode.push(bytecode::Type1::get_frame(v(target)).encode());
           }
       }
   }
   ```

### Phase 8: Proc Macro Integration

**Files**: `tapir-script-macros-core/src/lib.rs` (or equivalent)

The proc macro generates the `TapirScript` implementation. No changes should be needed since `frame` doesn't affect the property trait - it's handled entirely in the VM.

Verify that the generated code doesn't accidentally conflict with `frame` as a property name.

## Testing Strategy

### Unit Tests

1. **Parsing**: Verify `frame` parses as a valid variable reference:
   - `var x = frame;` - OK
   - `if frame > 10 { ... }` - OK
   - `frame = 5;` - ERROR
   - `var frame = 5;` - ERROR

2. **Type Checking**:
   - `var x: int = frame;` - OK
   - `var x: fix = frame;` - ERROR (type mismatch)
   - `var x: bool = frame > 10;` - OK (comparison returns bool)

3. **IR Generation**: Snapshot test showing `GetBuiltin` instruction:

   ```
   GetBuiltin { target: Symbol(1), builtin: Frame }
   ```

4. **Bytecode**: Test `GetFrame` opcode encoding/decoding round-trip.

### Integration Tests

1. **Basic read**:

   ```tapir
   int_prop = frame;
   ```

   After one `run()`: `int_prop` equals 0.

2. **Frame increment**:

   ```tapir
   int_prop = frame;
   wait;
   int_prop = frame;
   ```

   After first `run()`: `int_prop` equals 0.
   After second `run()`: `int_prop` equals 1.

3. **Spawned threads see same frame**:

   ```tapir
   spawn other();
   int_prop = frame;
   wait;

   fn other() {
       int_prop = frame;
   }
   ```

   Both assignments should write the same frame value (0 on first run).

4. **Frame in expressions**:

   ```tapir
   var start = frame;
   wait;
   wait;
   int_prop = frame - start;
   ```

   After three `run()` calls: `int_prop` equals 2.

5. **Frame in loops**:

   ```tapir
   while frame < 5 {
       wait;
   }
   int_prop = frame;
   ```

   After enough `run()` calls: `int_prop` equals 5.

### Error Message Tests

```
error: cannot shadow built-in variable
  --> script.tapir:1:5
   |
 1 | var frame = 10;
   |     ^^^^^
   |
   = note: 'frame' is a built-in variable

error: cannot assign to built-in variable
  --> script.tapir:2:1
   |
 2 | frame = 20;
   | ^^^^^
   |
   = note: 'frame' is read-only
```

## Alternatives Considered

### As a Property at Index 0

Reserve property index 0 for `frame`:

```rust
fn get_prop(&self, index: u8) -> i32 {
    if index == 0 {
        return self.frame;
    }
    // ... user properties start at index 1 ...
}
```

**Rejected because**:

- Complicates property indexing for users
- Mixes built-in concerns with user-defined properties
- Frame counter needs to live in `Vm`, not the property struct

### As a Compiler-Inserted Property

Have the compiler automatically add `frame` to the property list:

**Rejected because**:

- Requires proc macro to handle frame specially
- Property storage should be controlled by the host
- Separating built-in from user-defined keeps concerns clean

### Per-State Frame Counter

Each `State` tracks its own frame:

**Rejected because**:

- Spawned threads would see different frame values
- Doesn't match game developer expectation of "current frame"
- Complicates reasoning about script behavior

### `u64` Frame Counter

Use 64-bit counter to avoid overflow:

**Rejected because**:

- 32-bit signed provides ~414 days at 60 FPS - sufficient for games
- Keeps instruction encoding simple (result fits in register)
- On GBA, 64-bit operations are expensive

## Future Extensions

### Frame Since Spawn

A `local_frame` variable counting frames since the current thread spawned:

```tapir
spawn effect();

fn effect() {
    while local_frame < 30 {
        # animate for 30 frames from spawn
        wait;
    }
}
```

The `BuiltinVariable` infrastructure is designed to make adding this straightforward - just add a new enum variant and its corresponding bytecode instruction.

## Dependencies

- No external crate dependencies
- Internal: minimal changes, follows existing patterns for properties and built-in operations

## Estimated Complexity

- Phase 1 (VM Frame Counter): Low - add field and increment
- Phase 2 (Bytecode Instruction): Low - mechanical opcode addition
- Phase 3 (VM Dispatch): Low - single match arm
- Phase 4 (Built-in Infrastructure): Medium - new module with generic design
- Phase 5 (Type Checking): Low - return type from enum, reject assignment
- Phase 6 (IR Generation): Low - new instruction variant
- Phase 7 (Bytecode Emission): Low - single emit case
- Phase 8 (Proc Macro): None expected - verification only
