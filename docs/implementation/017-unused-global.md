# Implementation Plan: Global Variable Optimizations

## Overview

Add two new optimization passes for global variables:

1. **Read-only global constant propagation**: Replace `GetGlobal` with `Constant` when a global is never written to
2. **Dead global elimination**: Remove unused globals entirely from the output (saves VM space)

These optimizations reduce bytecode size and eliminate unnecessary global storage for scripts that declare globals but never modify them or never read from them.

## Motivation

Consider this common pattern:

```tapir
global SPEED = 5;
global DEBUG = false;

fn move_player() {
    x = x + SPEED;  # SPEED is read-only
}
```

Currently, `SPEED` generates a `GetGlobal` instruction at runtime, even though its value is known at compile time and never changes. With constant propagation, this becomes a simple `Constant` instruction.

Similarly, if a global is written but never read:

```tapir
global debug_counter = 0;

fn tick() {
    debug_counter = debug_counter + 1;  # Never read anywhere
    # ... actual work ...
}
```

The `debug_counter` global can be eliminated entirely, along with its `SetGlobal` instructions.

## Design Decisions

### Ordering Rationale

Run constant propagation first because:
- It replaces `GetGlobal` with `Constant`, removing the read
- This makes previously-read globals now "unread"
- Dead global elimination can then remove those globals entirely

### Edge Cases

- **Empty program or no globals**: Return `DidNothing` early
- **SetGlobal value symbol**: The `value` symbol in `SetGlobal { value, .. }` may still be needed by other instructions; `dead_store_elimination` handles cleanup
- **GlobalId in SymbolId encoding**: Only used during IR lowering (before optimizations), so renumbering is safe

## Implementation

### New Files

#### `compiler/src/compile/ir/optimisations/global_constant_propagation.rs`

```rust
pub fn propagate_readonly_globals(
    program: &mut [TapIrFunction],
    symtab: &mut SymTab,
) -> OptimisationResult
```

**Algorithm:**
1. Scan all functions to build `HashSet<usize>` of global indices that have `SetGlobal`
2. For each `GetGlobal { target, global_index }` where `global_index` is NOT in the set:
   - Look up `GlobalInfo` from symtab
   - Convert `initial_value` to `Constant` based on type:
     - `Type::Int` -> `Constant::Int(value)`
     - `Type::Fix` -> `Constant::Fix(Num::from_raw(value))`
     - `Type::Bool` -> `Constant::Bool(value != 0)`
   - Replace instruction with `TapIr::Constant(target, constant)`

#### `compiler/src/compile/ir/optimisations/dead_global_elimination.rs`

```rust
pub fn eliminate_dead_globals(
    program: &mut [TapIrFunction],
    symtab: &mut SymTab,
) -> OptimisationResult
```

**Algorithm:**
1. Scan all functions to collect which global indices have `GetGlobal` (are read)
2. Build a remapping: `old_index -> new_index` for globals that ARE read
3. Remove `SetGlobal` instructions for unread globals
4. Update all remaining `GetGlobal`/`SetGlobal` instructions with new indices
5. Remove unused globals from symtab via new `retain_globals()` method

**Index remapping example:**
- Globals: `[A, B, C]` where B is never read
- After: `[A, C]` with new indices 0, 1
- Remap: `{0 -> 0, 2 -> 1}`

### Modified Files

#### `compiler/src/compile/ir/optimisations.rs`

Add module declarations:

```rust
mod dead_global_elimination;
mod global_constant_propagation;
```

Add to `OPTIMISATIONS` array after `remove_unreferenced_functions`:

```rust
(
    "propagate_readonly_globals",
    &(global_constant_propagation::propagate_readonly_globals
        as fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult),
),
(
    "eliminate_dead_globals",
    &(dead_global_elimination::eliminate_dead_globals
        as fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult),
),
```

#### `compiler/src/compile/symtab_visitor.rs`

Add method to `SymTab`:

```rust
/// Remove globals that don't pass the predicate and renumber remaining ones.
/// Returns a map from old index to new index.
pub fn retain_globals<F>(&mut self, mut keep: F) -> HashMap<usize, usize>
where
    F: FnMut(&GlobalInfo) -> bool,
{
    let mut old_to_new = HashMap::new();
    let mut new_index = 0;

    self.globals.retain(|g| {
        if keep(g) {
            old_to_new.insert(g.id.0, new_index);
            new_index += 1;
            true
        } else {
            false
        }
    });

    // Update GlobalInfo.id for remaining globals
    for (i, g) in self.globals.iter_mut().enumerate() {
        g.id = GlobalId(i);
    }

    // Update global_names map
    self.global_names.retain(|_, id| old_to_new.contains_key(&id.0));
    for id in self.global_names.values_mut() {
        *id = GlobalId(old_to_new[&id.0]);
    }

    old_to_new
}
```

## Testing Strategy

### Snapshot Tests

Create test cases in `compiler/src/compile/ir/snapshot_tests/optimisations/`:

1. **Read-only global replaced with constant**
   ```tapir
   # propagate_readonly_globals
   global CONST = 42;
   x = CONST;
   ```
   Expected: `GetGlobal` replaced with `Constant(42)`

2. **Written global NOT replaced**
   ```tapir
   # propagate_readonly_globals
   global counter = 0;
   counter = counter + 1;
   x = counter;
   ```
   Expected: `GetGlobal` and `SetGlobal` remain

3. **Unread global removed entirely**
   ```tapir
   # eliminate_dead_globals
   global unused = 0;
   unused = 5;
   ```
   Expected: `SetGlobal` removed, global removed from symtab

4. **Read global kept**
   ```tapir
   # eliminate_dead_globals
   global used = 10;
   x = used;
   ```
   Expected: Global remains, `GetGlobal` unchanged

5. **Combined optimizations**
   ```tapir
   # propagate_readonly_globals, eliminate_dead_globals
   global CONST = 42;
   global unused = 0;
   global writable = 0;

   x = CONST;
   unused = 5;
   writable = writable + 1;
   y = writable;
   ```
   Expected: CONST inlined, unused eliminated, writable remains

### Verification Steps

1. Run `cargo test -p compiler` to ensure existing tests pass
2. Run `cargo insta test -p compiler` and review snapshots

## Implementation Order

### Phase 1: SymTab Method

1. Add `retain_globals()` method to `SymTab`
2. Unit test the method in isolation

### Phase 2: Constant Propagation Pass

1. Create `global_constant_propagation.rs`
2. Add module declaration to `optimisations.rs`
3. Add to `OPTIMISATIONS` array
4. Add snapshot tests

### Phase 3: Dead Global Elimination Pass

1. Create `dead_global_elimination.rs`
2. Add module declaration to `optimisations.rs`
3. Add to `OPTIMISATIONS` array (after constant propagation)
4. Add snapshot tests

### Phase 4: Integration Testing

1. Test combined optimization passes
2. Verify no regressions in existing tests
3. Review all new snapshots

## Implementation Notes

This section will document differences between the plan and actual implementation.
