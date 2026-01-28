# Implementation Plan: Builtin Functions

## Overview

Add a builtin function system allowing the prelude to declare functions that map directly to compiler-known implementations. These enable mathematical functions like `sin`, `cos`, `sqrt` without making them full bytecode instructions, while supporting compile-time constant folding.

## Design Decisions

- **Syntax**: `builtin(<id>) fn name(args) -> ret;` (declaration only, no body)
- **Location**: Only allowed in prelude (`stdlib/prelude.tapir`)
- **ID semantics**: Positive IDs are pure (can be constant-folded), negative IDs have side effects
- **Type source of truth**: The signature in the declaration
- **New crate**: `builtins` crate shared between compiler and VM
- **Error handling**: `Result` type with rich errors - compiler emits warnings for invalid constant args (e.g., `sqrt(-1)`), VM panics on invalid ID
- **Split execution**: Separate `execute_pure` (no context, for compiler) and `execute_impure` (with context, for VM)

## New Crate: `builtins`

### Crate Structure

```
builtins/
├── Cargo.toml
└── src/
    └── lib.rs
```

### Dependencies

```toml
[package]
name = "builtins"
version = "0.1.0"
edition = "2024"

[dependencies]
agb_fixnum = "0.2"
```

### Core Types

```rust
// builtins/src/lib.rs

use agb_fixnum::Num;

pub type Fix = Num<i32, 8>;

/// Error when a builtin fails
#[derive(Clone, Debug)]
pub enum BuiltinError {
    /// Unknown builtin ID
    UnknownId(i8),
    /// Invalid argument for the operation (e.g., sqrt of negative)
    InvalidArgument {
        id: u8,
        reason: &'static str,
    },
}

/// Execute a pure builtin (id >= 0). Used by compiler for constant folding.
/// No context needed since pure functions have no side effects.
///
/// Returns an error if the operation is mathematically invalid (e.g., sqrt(-1)),
/// allowing the compiler to emit a warning.
pub fn execute_pure(id: u8, args: &[i32]) -> Result<i32, BuiltinError> {
    match id {
        0 => Ok(Fix::from_raw(args[0]).sin().to_raw()),
        1 => Ok(Fix::from_raw(args[0]).cos().to_raw()),
        2 => {
            let x = Fix::from_raw(args[0]);
            if x < Fix::new(0) {
                return Err(BuiltinError::InvalidArgument {
                    id: 2,
                    reason: "cannot take square root of negative number",
                });
            }
            Ok(x.sqrt().to_raw())
        }
        3 => Ok(Fix::from_raw(args[0]).floor()),   // floor(fix) -> int
        4 => Ok(Fix::from_raw(args[0]).ceil()),    // ceil(fix) -> int
        5 => Ok(Fix::from_raw(args[0]).round()),   // round(fix) -> int
        _ => Err(BuiltinError::UnknownId(id as i8)),
    }
}

/// Execute an impure builtin (id < 0). Only called at runtime by the VM.
/// These cannot be constant-folded because they depend on external state.
pub fn execute_impure(id: i8, _args: &[i32], context: &Context) -> Result<i32, BuiltinError> {
    match id {
        -1 => Ok(context.frame),  // frame() -> int
        _ => Err(BuiltinError::UnknownId(id)),
    }
}

/// Context for impure builtins that need external state
#[derive(Clone, Copy, Default)]
pub struct Context {
    pub frame: i32,
}
```

## Simplifications: Remove BuiltinVariable

With builtin functions, the existing `BuiltinVariable` system becomes redundant. Currently `frame` is a special builtin variable with its own symbol ID encoding (bit 63) and dedicated IR/bytecode instructions. Instead, `frame` can become a zero-argument builtin function.

### What to Remove

**`compiler/src/builtins.rs`** - Delete entirely:

- `BuiltinVariable` enum
- `RESERVED_BIT` constant and symbol ID encoding
- `from_symbol_id`, `from_name`, `symbol_id` methods

**`compiler/src/compile/ir.rs`**:

- Remove `GetBuiltin { target: SymbolId, builtin: BuiltinVariable }` variant

**`compiler/src/compile/symtab_visitor.rs`**:

- Remove special handling for `BuiltinVariable::from_name()` checks

**`compiler/src/compile/type_visitor.rs`**:

- Remove `BuiltinVariable` resolution in identifier lookup

**`bytecode/src/lib.rs`**:

- Remove `GetBuiltin` opcode
- Remove `Type1::get_builtin()` constructor

**`vm/src/state.rs`**:

- Remove `O::GetBuiltin` handler

### Benefits

1. **Simpler symbol IDs** - No reserved bit for builtin variables
2. **Fewer IR instructions** - One `CallBuiltin` instead of `CallBuiltin` + `GetBuiltin`
3. **Fewer bytecode opcodes** - One instruction type for all builtins
4. **Unified mental model** - Everything from the prelude is a builtin function
5. **Easier to extend** - Adding new "variables" like `frame` just means adding a zero-arg builtin

### Migration

Code that uses `frame` as a variable:

```tapir
let f = frame;
```

Now calls a function (but syntax stays the same if we treat zero-arg builtins specially, or requires parentheses):

```tapir
let f = frame();
```

**Decision**: Require parentheses for all builtin function calls (`frame()`). No backwards compatibility needed.

## Simplifications: Unify Function Namespaces

Currently there are three function namespaces requiring separate lookup logic:

1. **Internal functions** (`FunctionId`) - user-defined functions
2. **External functions** (`ExternalFunctionId`) - extern declarations
3. **Builtin functions** (`BuiltinFunctionId`) - new

The existing `InternalOrExternalFunctionId` enum partially addresses this. Extend it to include builtins.

### Current State

```rust
// In ast.rs
pub enum InternalOrExternalFunctionId {
    Internal(FunctionId),
    External(ExternalFunctionId),
}

// In symtab_visitor.rs
function_names: HashMap<&'input str, InternalOrExternalFunctionId>,

// In type_visitor.rs
functions: HashMap<InternalOrExternalFunctionId, FunctionInfo<'input>>,
```

### Proposed Change

```rust
// Rename to ResolvedFunctionId (or just FunctionRef)
pub enum ResolvedFunctionId {
    Internal(FunctionId),
    External(ExternalFunctionId),
    Builtin(BuiltinFunctionId),
}
```

### Benefits

1. **Single lookup path** - `function_names.get(name)` returns a `ResolvedFunctionId` for any callable
2. **Unified FunctionInfo** - `TypeVisitor::functions` can store info for all function types
3. **Simpler call resolution** - No special-case checking for builtins before checking the map
4. **Consistent code generation** - Match on `ResolvedFunctionId` to emit the right instruction

### Changes Required

**`ast.rs`**:

- Rename `InternalOrExternalFunctionId` to `ResolvedFunctionId`
- Add `Builtin(BuiltinFunctionId)` variant

**`symtab_visitor.rs`**:

- After processing extern/internal functions, add builtin functions to `function_names`
- Build `BuiltinFunctionInfo` from prelude declarations

**`type_visitor.rs`**:

- Add builtin functions to the `functions` HashMap in `TypeVisitor::new`
- `type_for_call` handles all three variants uniformly

**`ir/lowering.rs`**:

- Match on `ResolvedFunctionId` to emit `Call`, `CallExternal`, or `CallBuiltin` IR

### Symbol ID Simplification

With `BuiltinVariable` removed:

```rust
// GlobalId can simplify its check
impl GlobalId {
    const GLOBAL_BIT: u64 = 1 << 62;

    pub fn from_symbol_id(id: SymbolId) -> Option<Self> {
        // No longer need to check BuiltinVariable::RESERVED_BIT
        if id.0 & Self::GLOBAL_BIT != 0 {
            Some(GlobalId((id.0 & !Self::GLOBAL_BIT) as usize))
        } else {
            None
        }
    }
}
```

The reserved bit 63 becomes available for future use (or can be removed entirely if not needed).

## Grammar Changes

### `grammar.lalrpop`

Add new token and production rules:

```lalrpop
// New token
"builtin" => Token::Builtin,

// New top-level statement variant
TopLevelStatement: TopLevelStatement<'input> = {
    // ... existing variants ...
    <BuiltinFunctionDeclaration> => TopLevelStatement::BuiltinFunction(<>),
};

// Builtin function declaration (no body)
BuiltinFunctionDeclaration: BuiltinFunction<'input> = {
    <start:@L> "builtin" "(" <id:IntLiteral> ")" "fn" <name:Ident>
    "(" <arguments:Comma<TypedIdent>> ")" <return_type:("->" <Type>)?> ";" <end:@R>
    => BuiltinFunction {
        id: id.parse().unwrap(),
        name,
        arguments,
        return_type,
        span: (start, end).into(),
    }
};
```

### `lexer.rs`

Add the `builtin` keyword:

```rust
#[token("builtin")]
Builtin,
```

## AST Changes

### `ast.rs`

```rust
/// A builtin function declaration
#[derive(Debug, Clone)]
pub struct BuiltinFunction<'input> {
    pub id: i8,
    pub name: &'input str,
    pub arguments: Vec<TypedIdent<'input>>,
    pub return_type: Option<Type>,
    pub span: Span,
}

// Add to TopLevelStatement enum
pub enum TopLevelStatement<'input> {
    // ... existing variants ...
    BuiltinFunction(BuiltinFunction<'input>),
}

// Add to Script struct
pub struct Script<'input> {
    // ... existing fields ...
    pub builtin_functions: Vec<BuiltinFunction<'input>>,
}

// Update merge_from to handle builtin_functions
impl Script<'_> {
    pub fn merge_from(&mut self, other: Script<'input>) {
        // ... existing merge logic ...
        self.builtin_functions.splice(0..0, other.builtin_functions);
    }
}
```

## Symbol Table Changes

### New ID Type

```rust
// In compile/symtab.rs or a new file

/// ID for a builtin function, wrapping the raw i8 from the declaration
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BuiltinFunctionId(pub i8);

impl BuiltinFunctionId {
    pub fn is_pure(self) -> bool {
        self.0 >= 0
    }
}
```

### Symbol Table Updates

```rust
// In symtab.rs

pub struct SymTab {
    // ... existing fields ...
    pub builtin_functions: HashMap<String, BuiltinFunctionInfo>,
}

pub struct BuiltinFunctionInfo {
    pub id: BuiltinFunctionId,
    pub name: String,              // For diagnostic messages
    pub argument_types: Vec<Type>,
    pub return_type: Option<Type>,
    pub span: Span,
}
```

### `symtab_visitor.rs`

Add handling in the symbol table construction pass:

```rust
fn visit_builtin_function(&mut self, builtin: &BuiltinFunction<'_>) {
    // Check we're in prelude file
    if self.current_file_id != PRELUDE_FILE_ID {
        ErrorKind::BuiltinOutsidePrelude
            .at(builtin.span)
            .emit(self.diagnostics);
        return;
    }

    // Check for duplicate names
    if self.symtab.builtin_functions.contains_key(builtin.name) {
        ErrorKind::DuplicateBuiltinFunction { name: builtin.name.to_string() }
            .at(builtin.span)
            .emit(self.diagnostics);
        return;
    }

    let info = BuiltinFunctionInfo {
        id: BuiltinFunctionId(builtin.id),
        name: builtin.name.to_string(),
        argument_types: builtin.arguments.iter().map(|a| a.ty.clone()).collect(),
        return_type: builtin.return_type.clone(),
        span: builtin.span,
    };

    self.symtab.builtin_functions.insert(builtin.name.to_string(), info);
}
```

## Type Checking Changes

### `type_visitor.rs`

When resolving a function call, check builtin functions:

```rust
fn resolve_call(&mut self, call: &FunctionCall) -> Option<ResolvedCall> {
    // Check builtins first (they shadow user functions, but only exist in prelude)
    if let Some(info) = self.symtab.builtin_functions.get(call.name) {
        // Validate argument count
        if call.arguments.len() != info.argument_types.len() {
            ErrorKind::WrongArgumentCount {
                expected: info.argument_types.len(),
                got: call.arguments.len(),
            }
            .at(call.span)
            .emit(self.diagnostics);
            return None;
        }

        // Validate argument types
        for (arg, expected_ty) in call.arguments.iter().zip(&info.argument_types) {
            let arg_ty = self.type_of(arg);
            if arg_ty != *expected_ty {
                ErrorKind::TypeMismatch { expected: *expected_ty, got: arg_ty }
                    .at(arg.span())
                    .emit(self.diagnostics);
            }
        }

        return Some(ResolvedCall::Builtin {
            id: info.id,
            return_type: info.return_type,
        });
    }

    // ... existing function/extern resolution ...
}
```

## IR Changes

**Prerequisite**: This feature depends on [011-spanned-ir.md](011-spanned-ir.md), which adds symbol-based span tracking via `SymbolSpans`. Complete that first.

### `ir.rs`

Add new IR instruction variant:

```rust
pub enum TapIr {
    // ... existing variants ...

    /// Call a builtin function
    CallBuiltin {
        target: SymbolId,           // Where to store result (if any)
        id: BuiltinFunctionId,      // Which builtin
        args: Box<[SymbolId]>,      // Arguments
    },
}
```

The span for warnings (e.g., `sqrt(-1)`) comes from looking up the argument symbol in `SymbolSpans`, which points to where that value was defined.

### `lowering.rs`

Generate IR for builtin calls. The argument symbols already have spans recorded from when they were lowered.

```rust
fn lower_call(&mut self, call: &ResolvedCall, args: Vec<SymbolId>, target: SymbolId, span: Span) {
    match call {
        ResolvedCall::Builtin { id, .. } => {
            // Record where the result value comes from
            self.symbol_spans.define(target, span);

            self.current_block.push(TapIr::CallBuiltin {
                target,
                id: *id,
                args: args.into_boxed_slice(),
            });
        }
        // ... existing cases ...
    }
}
```

## Optimization: Constant Folding

### `optimisations/constant_folding.rs`

Add case for builtin calls with constant arguments. The return type is looked up from the symbol table to correctly interpret the raw i32 result. When execution fails (e.g., `sqrt(-1)`), emit a compile-time warning.

```rust
fn fold_builtin_call(
    instr: &TapIr,
    constants: &HashMap<SymbolId, Constant>,
    symbol_spans: &SymbolSpans,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) -> Option<TapIr> {
    let TapIr::CallBuiltin { target, id, args } = instr else {
        return None;
    };

    // Only fold pure builtins (non-negative IDs)
    // Negative IDs (like frame = -1) are impure and cannot be folded
    if id.0 < 0 {
        return None;
    }

    // Check all arguments are constants
    let const_args: Option<Vec<i32>> = args.iter()
        .map(|arg| constants.get(arg).map(|c| c.to_raw_i32()))
        .collect();

    let const_args = const_args?;

    // Try to evaluate using the pure execution path (no context needed)
    match builtins::execute_pure(id.0 as u8, &const_args) {
        Ok(result) => {
            // Look up return type from symtab to interpret the result
            let return_type = symtab.get_builtin_by_id(*id)?.return_type?;

            let constant = match return_type {
                Type::Int => Constant::Int(result),
                Type::Fix => Constant::Fix(Fix::from_raw(result)),
                Type::Bool => Constant::Bool(result != 0),
            };

            Some(TapIr::Constant(*target, constant))
        }
        Err(BuiltinError::InvalidArgument { reason, id: arg_id }) => {
            // Find which argument caused the error and get its definition span
            // arg_id tells us which builtin failed, but we need to find the problematic arg
            // For sqrt, it's always arg 0
            let problematic_arg = args.first()?;
            if let Some(span) = symbol_spans.get(*problematic_arg) {
                let builtin_name = symtab.get_builtin_by_id(*id)
                    .map(|info| info.name.as_str())
                    .unwrap_or("unknown");
                WarningKind::BuiltinWillFail {
                    name: builtin_name.to_string(),
                    reason: reason.to_string(),
                }
                .at(span)
                .emit(diagnostics);
            }
            None // Don't fold, let it fail at runtime
        }
        COMMENT: I think I'm fine with a panic here
        Err(BuiltinError::UnknownId(bad_id)) => {
            // Unknown ID - warn using the target's span as fallback
            if let Some(span) = symbol_spans.get(*target) {
                WarningKind::UnknownBuiltinId { id: bad_id }
                    .at(span)
                    .emit(diagnostics);
            }
            None
        }
    }
}
```

### Symbol Table Helper

```rust
// In symtab.rs
impl SymTab {
    /// Look up a builtin function by its ID
    pub fn get_builtin_by_id(&self, id: BuiltinFunctionId) -> Option<&BuiltinFunctionInfo> {
        self.builtin_functions.values().find(|info| info.id == id)
    }
}
```

### Constant Helper

```rust
// In ir.rs or constant.rs
impl Constant {
    pub fn to_raw_i32(&self) -> i32 {
        match self {
            Constant::Int(v) => *v,
            Constant::Fix(v) => v.to_raw(),
            Constant::Bool(v) => *v as i32,
        }
    }
}
```

## Bytecode Changes

### `bytecode/src/lib.rs`

Add new opcode:

```rust
pub enum Opcode {
    // ... existing opcodes ...

    /// Call a builtin function
    /// Type1: target register, builtin_id (as i16 split across a,b), first_arg register
    /// Actually needs: target, id_high, id_low, first_arg, arg_count
    /// This doesn't fit Type1. Options:
    ///   1. Use Type3 for the ID, follow with Type1 for registers
    ///   2. Add a new instruction format
    ///   3. Limit to 256 builtins (single byte ID)
    /// Going with option 3 for simplicity - use Type1:
    ///   target: result register
    ///   a: builtin_id (u8, but we interpret as i8 for sign)
    /// COMMENT: does this need an arg count?
    ///   b: first_arg register | (arg_count << 4)
    CallBuiltin,
}

impl Type1 {
    pub fn call_builtin(target: u8, id: i8, first_arg: u8, arg_count: u8) -> Self {
        assert!(arg_count <= 15, "Max 15 arguments for builtin");
        Self {
            opcode: Opcode::CallBuiltin,
            target,
            a: id as u8,
            b: first_arg | (arg_count << 4),
        }
    }
}
```

**Note**: Using i8 (-128 to 127) allows 128 pure builtins and 127 impure ones.

## VM Changes

### `Cargo.toml`

Add dependency:

```toml
[dependencies]
builtins = { path = "../builtins" }
```

### `state.rs`

Add instruction handler using the split execution API:

```rust
// In the instruction dispatch
O::CallBuiltin => {
    type1!(target, id, b);
    let first_arg = b & 0x0F;
    let arg_count = (b >> 4) as usize;

    // Pass slice directly from stack to avoid allocation
    let start = self.stack_offset + first_arg as usize;
    let args = &self.stack[start..start + arg_count];

    // id byte is interpreted as signed
    let signed_id = id as i8;

    let result = if signed_id >= 0 {
        // Pure builtin - no context needed
        builtins::execute_pure(id, args)
    } else {
        // Impure builtin - needs runtime context
        let context = builtins::Context { frame };
        builtins::execute_impure(signed_id, args, &context)
    };

    match result {
        Ok(value) => self.set_reg(target, value),
        Err(BuiltinError::UnknownId(id)) => panic!("Unknown builtin ID: {}", id),
        Err(BuiltinError::InvalidArgument { id, reason }) => {
            panic!("Builtin {} failed: {}", id, reason)
        }
    }
}
```

## Prelude Updates

### `stdlib/prelude.tapir`

```tapir
// Mathematical functions (fix type)
builtin(0) fn sin(x: fix) -> fix;
builtin(1) fn cos(x: fix) -> fix;
builtin(2) fn sqrt(x: fix) -> fix;

// Rounding functions (fix -> int)
builtin(3) fn floor(x: fix) -> int;
builtin(4) fn ceil(x: fix) -> int;
builtin(5) fn round(x: fix) -> int;

// Runtime state - impure (negative ID), replaces BuiltinVariable::Frame
builtin(-1) fn frame() -> int;

// Future additions:
// builtin(7) fn tan(x: fix) -> fix;
// builtin(8) fn atan2(y: fix, x: fix) -> fix;
// builtin(9) fn abs_fix(x: fix) -> fix;
// builtin(10) fn abs_int(x: int) -> int;
// builtin(11) fn min_int(a: int, b: int) -> int;
// builtin(12) fn max_int(a: int, b: int) -> int;
// builtin(13) fn min_fix(a: fix, b: fix) -> fix;
// builtin(14) fn max_fix(a: fix, b: fix) -> fix;
```

## Compiler Updates

### Update Crate Dependencies

```toml
# compiler/Cargo.toml
[dependencies]
builtins = { path = "../builtins" }
```

### Code Generation (`compile/compile.rs` or `codegen.rs`)

```rust
fn compile_ir(&mut self, ir: &TapIr) {
    match ir {
        TapIr::CallBuiltin { target, id, args } => {
            let target_reg = self.reg_alloc.get(*target);
            let first_arg = self.prepare_args(args);
            let arg_count = args.len() as u8;

            self.emit(Type1::call_builtin(
                target_reg,
                id.0,
                first_arg,
                arg_count,
            ));
        }
        // ... existing cases ...
    }
}
```

## Diagnostic Changes

**Prerequisite**: The warning infrastructure (`Severity`, `WarningKind`, `Diagnostics::warn()`) is defined in [011-spanned-ir.md](011-spanned-ir.md). Complete that first.

### Error Types

```rust
pub enum ErrorKind {
    // ... existing variants ...

    /// Builtin declaration outside prelude
    BuiltinOutsidePrelude,

    /// Duplicate builtin function name
    DuplicateBuiltinFunction { name: String },
}
```

### Warning Types (additions to WarningKind)

```rust
// Add these variants to WarningKind (defined in 011-spanned-ir.md)
pub enum WarningKind {
    // ... existing variants from 011 ...

    /// Builtin call with constant args will fail at runtime
    /// e.g., sqrt(-1.0) - detected during constant folding
    BuiltinWillFail {
        name: String,
        reason: String,
    },

    /// Unknown builtin ID in prelude (shouldn't happen with correct prelude)
    UnknownBuiltinId { id: i8 },
}
```

### Example Warning Output

```
warning: sqrt(-1.0) will fail at runtime: cannot take square root of negative number
  --> script.tapir:1:9
   |
 1 | let x = sqrt(-1.0);
   |         ^^^^^^^^^^
```

## Implementation Order

### Prerequisites

- Complete [011-spanned-ir.md](011-spanned-ir.md) first (SymbolSpans, warning infrastructure)

### Phase 1: Core Builtin Function System

1. **Create `builtins` crate** with `execute_pure`, `execute_impure`, `BuiltinError`, and `Context` types
2. **Update workspace** `Cargo.toml` to include new crate
3. **Lexer**: Add `builtin` keyword
4. **Grammar**: Add `BuiltinFunctionDeclaration` production
5. **AST**: Add `BuiltinFunction` struct and update `Script`
6. **Symbol table**: Add `BuiltinFunctionInfo`, collection, and `get_builtin_by_id`
7. **`symtab_visitor`**: Handle builtin declarations, enforce prelude-only
8. **`type_visitor`**: Resolve calls to builtins, type check arguments
9. **IR**: Add `CallBuiltin` variant
10. **Lowering**: Generate `CallBuiltin` IR from resolved calls
11. **Bytecode**: Add `CallBuiltin` opcode and `Type1` constructor
12. **Code generation**: Emit `CallBuiltin` instructions
13. **VM**: Add `CallBuiltin` handler using split `execute_pure`/`execute_impure` API
14. **Add builtin warning types** (`BuiltinWillFail`, `UnknownBuiltinId`) to `WarningKind`
15. **Constant folding**: Add builtin call folding for pure functions, emit warnings on failure
16. **Prelude**: Add initial builtin declarations (`sin`, `cos`, `sqrt`, `floor`, `ceil`, `round`, `frame`)
17. **Tests**: Add snapshot tests for parsing, type checking, codegen, runtime, and warnings

### Phase 2: Remove BuiltinVariable

17. **Remove** `BuiltinVariable` enum from `compiler/src/builtins.rs`
18. **Remove** `GetBuiltin` IR variant
19. **Remove** `GetBuiltin` bytecode opcode
20. **Remove** special symbol ID encoding (bit 63)
21. **Update** symtab_visitor and type_visitor to remove BuiltinVariable handling
22. **Remove** VM `O::GetBuiltin` handler
23. **Simplify** `GlobalId::from_symbol_id` (no longer needs to check builtin bit)
24. **Update tests** to use `frame()` function syntax

### Phase 3: Unify Function Namespaces

25. **Rename** `InternalOrExternalFunctionId` to `ResolvedFunctionId`
26. **Add** `Builtin(BuiltinFunctionId)` variant to `ResolvedFunctionId`
27. **Update** `symtab_visitor` to add builtins to `function_names` map
28. **Update** `TypeVisitor::new` to include builtin function info
29. **Simplify** call resolution - single lookup path for all function types
30. **Update** IR lowering to match on unified `ResolvedFunctionId`

## Testing Strategy

### Parser Tests

- Valid builtin declaration parses correctly
- Builtin outside prelude produces error
- Duplicate builtin name produces error

### Type Checking Tests

- Calling builtin with correct types succeeds
- Wrong argument count produces error
- Wrong argument types produce error
- Return type correctly inferred (`floor(1.5)` is `int`)

### Codegen Tests

- Builtin call generates correct bytecode
- Multiple arguments handled correctly
- Zero-argument builtins work (`frame()`)

### Constant Folding Tests

- `sin(0.0)` folds to `0.0`
- `cos(0.0)` folds to `1.0`
- `floor(1.5)` folds to `1`
- Non-constant arguments don't fold
- Impure builtins like `frame()` (negative IDs) don't fold

### Warning Tests

- `sqrt(-1.0)` emits warning about negative square root
- Warning includes correct source span
- Compilation continues after warning (code still generated)
- Warning message includes builtin name and reason

### Runtime Tests

- `sin`, `cos`, `sqrt` produce correct values
- `floor`, `ceil`, `round` produce correct values
- `frame()` returns current frame counter
- Invalid builtin ID panics with clear message

## Future Considerations

- **More math builtins**: `tan`, `atan2`, `abs_fix`, `abs_int`, `min`, `max`
- **Impure builtins**: Random number generation (`builtin(-2) fn rand() -> int;`)
- **Documentation**: Generate docs from builtin declarations
- **IDE support**: Hover info, go-to-definition for builtins
- **Builtin validation**: Optionally verify at compile time that declared builtin IDs exist in the `builtins` crate
