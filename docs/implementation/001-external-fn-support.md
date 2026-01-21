# External Function Support

## Overview

This document outlines the implementation plan for completing `extern fn` support in tapir-script. External functions allow tapir scripts to call into host Rust code, enabling scripts to interact with game systems, access resources, and perform operations that cannot be expressed in the scripting language itself.

## Current State

### What Works

- **Tokenization**: The `extern` keyword is recognized by the lexer
- **Parsing**: Grammar rules accept `extern fn name(args) -> ret;` syntax and reject extern functions with bodies
- **AST**: `ExternFunctionDefinition` struct exists and extern functions are collected into `Script.extern_functions`

### What's Missing

- Type checking integration
- Symbol resolution
- IR generation for extern calls
- Bytecode opcode for extern dispatch
- Runtime mechanism to invoke host functions

## Design Decisions

### Extern Function ID Space

Extern functions use a separate `ExternFunctionId` type distinct from regular `FunctionId`:
- Clear distinction at type level
- No risk of ID collision
- Call sites handle both types via an enum

### Bytecode Approach

A new `ExternCall` opcode will be added:
- Takes extern function index and register containing first argument
- Clean separation from internal `Call`
- Host uses the index to look up the registered function

### Extern Function Dispatch

Extern functions map to methods on the properties object. A declaration like:

```tapir
extern fn foo(a: fix, b: int) -> int;
```

Assumes a corresponding method exists on the properties struct:

```rust
impl FooProperties {
    fn foo(&self, a: Num<i32, 8>, b: i32) -> i32 {
        // ...
    }
}
```

The proc macro generates dispatch code that calls these methods directly, popping arguments from the stack and pushing return values.

### Error Handling

Extern functions that encounter errors should panic. This keeps the implementation simple and matches the expected use case of game scripting where errors are fatal.

### Async Behavior

Extern functions are synchronous only. They cannot cause the script to yield.

## Implementation Plan

### Phase 1: Type System Integration

**Files**: `compiler/src/compile/type_visitor.rs`, `compiler/src/compile/symtab_visitor.rs`

1. Add `ExternFunctionId` type to `ast.rs`:

   ```rust
   #[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, Serialize, PartialOrd, Ord)]
   pub struct ExternFunctionId(pub usize);
   ```

2. Update `SymTabVisitor` to:
   - Accept `extern_functions` in constructor
   - Assign `ExternFunctionId` to each extern function via metadata
   - Register extern function names in `function_names` map (need to distinguish from regular functions)

3. Update `TypeVisitor` to:
   - Accept `extern_functions` in constructor
   - Store extern function signatures (separate from regular functions or unified with a discriminant)
   - Allow calls to extern functions with proper type checking

4. Update call resolution in AST:
   - `MaybeResolved` for calls needs to handle both `FunctionId` and `ExternFunctionId`
   - Use an enum: `ResolvedFunction::Internal(FunctionId) | External(ExternFunctionId)`

### Phase 2: IR Generation

**Files**: `compiler/src/compile/ir.rs`

1. Add new IR instruction:

   ```rust
   ExternCall {
       target: Box<[SymbolId]>,  // where to put return values
       f: ExternFunctionId,
       args: Box<[SymbolId]>,    // argument registers
   }
   ```

2. Update `create_ir_for_expression` and `create_ir_for_statement` to:
   - Check if resolved call target is external
   - Emit `ExternCall` instead of `Call` for extern functions

3. Update IR analysis (liveness, etc.) to handle `ExternCall` similarly to `Call`

### Phase 3: Bytecode Generation

**Files**: `bytecode/src/lib.rs`, `compiler/src/compile/disassemble.rs`, bytecode emission code

1. Add `ExternCall` opcode to `bytecode/src/lib.rs`:

   ```rust
   ExternCall,  // extern_call extern_id, first_arg
   ```

2. Add `Type1::extern_call` constructor:

   ```rust
   pub const fn extern_call(extern_id: u8, first_arg: u8) -> Self {
       Self::new2(Opcode::ExternCall, extern_id, first_arg)
   }
   ```

3. Update bytecode emission to:
   - Emit `ExternCall` for `TapIrInstr::ExternCall`
   - Include extern function ID in the instruction

4. Update disassembler to handle `ExternCall`

### Phase 4: Compiler Output

**Files**: `compiler/src/compile.rs`, compiled output structures

1. Add extern function metadata to compiled output:
   - List of extern function names (for error messages / debugging)
   - Extern function signatures (for runtime validation if desired)
   - Count of extern functions (for host to validate registration)

2. Update `CompiledScript` (or equivalent) to include extern function info

### Phase 5: Runtime / VM Support

**Files**: VM implementation (location TBD - may be in a separate crate or game code)

1. Define trait for extern function dispatch:

   ```rust
   pub trait ExternFunctions<P> {
       fn call(&self, id: ExternFunctionId, props: &mut P, stack: &mut Stack);
   }
   ```

2. Update VM to:
   - Accept extern function table at construction
   - Handle `ExternCall` opcode by invoking registered function

### Phase 6: Proc Macro Integration

**Files**: `tapir-script-macros` crate (or similar)

1. Parse `extern fn` declarations from script
2. Generate extern function dispatch table matching script order
3. Validate signatures match between script declarations and Rust implementations
4. Generate compile error if extern function is declared but not provided

## Testing Strategy

### Unit Tests

1. **Parsing**: Already exists - `extern_functions.tapir` snapshot test
2. **Symbol resolution**: Test that extern function names resolve correctly
3. **Type checking**:
   - Correct argument types accepted
   - Wrong argument types rejected
   - Return type properly inferred
4. **IR generation**: Snapshot test showing `ExternCall` instructions
5. **Bytecode**: Test `ExternCall` opcode encoding/decoding

### Integration Tests

1. Script calling extern function with various argument/return types
2. Script calling extern function that modifies properties
3. Error case: calling undefined extern function
4. Error case: wrong argument count/types to extern function

## Dependencies

- No external crate dependencies expected
- Internal: requires understanding of existing function call flow through all compilation stages

## Estimated Complexity

- Phase 1 (Type System): Medium - touches several files but follows existing patterns
- Phase 2 (IR): Low - straightforward addition of new instruction type
- Phase 3 (Bytecode): Low - mechanical addition of new opcode
- Phase 4 (Compiler Output): Low - adding metadata to output
- Phase 5 (Runtime): Medium - depends on VM structure
- Phase 6 (Proc Macro): Medium-High - proc macro work is fiddly
