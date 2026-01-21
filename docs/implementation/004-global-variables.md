# Global Variables

## Overview

This document outlines the implementation plan for global variables in tapir-script. Global variables allow scripts to declare shared state that is visible to all threads and persists across frames, without requiring the host to define properties.

## Motivation

Currently, the only way to share state between threads or persist values across frames is through properties, which require host-side definition:

```rust
// Host code
#[derive(TapirScript)]
struct MyScript {
    #[tapir(int)]
    shared_counter: i32,
}
```

```tapir
# Script code
shared_counter = shared_counter + 1;
```

This is cumbersome for script-internal state that the host doesn't care about. Global variables provide script-local shared state:

```tapir
global shared_counter = 0;

spawn worker();
spawn worker();
wait;
wait;
# shared_counter is now 2

fn worker() {
    shared_counter = shared_counter + 1;
}
```

## Design Decisions

### Syntax

Global declarations use the `global` keyword at the top level of a script:

```tapir
global foo = 3;
global bar = 1.5;
global flag = true;
```

Globals must be declared before any function definitions, similar to how properties are conceptually "at the top" of a script.

### Constant Initialization Only

Global initializers must be compile-time constants (literals). This avoids complexity around:

- What happens if the initializer calls a function that `wait`s?
- When exactly does initialization run relative to spawned threads?
- Order-of-initialization dependencies between globals

```tapir
global foo = 3;           # OK
global bar = 3 + 2;       # ERROR: expression not allowed
global baz = some_fn();   # ERROR: function call not allowed
```

If users need computed initial values, they can assign in the top level:

```tapir
global foo = 0;

foo = expensive_computation();
```

### Shared Across All Threads

All spawned threads see the same global values. This is the defining characteristic of globals - they provide shared state:

```tapir
global counter = 0;

spawn increment();
spawn increment();
wait;
# counter is now 2

fn increment() {
    counter = counter + 1;
}
```

Note: since tapir is always single threaded and threads run sequentially, the counter here will always be 2.

### Persistence Across Frames

Global values persist across `run()` calls, just like properties:

```tapir
global frame_count = 0;

loop {
    frame_count = frame_count + 1;
    wait;
}
```

After N calls to `run()`, `frame_count` equals N.

### Type Inference

Types can be only be inferred from the initializer

```tapir
global foo = 3;        # inferred as int
```

### VM-Managed Storage

Globals are stored in the `Vm` struct and accessed via new `GetGlobal`/`SetGlobal` bytecode instructions. This keeps them separate from user-defined properties and makes the compiler solely responsible for their management.

### Name Conflicts

If a global has the same name as a user-defined property, compilation fails with an error:

```tapir
# Given property "health" defined in CompileSettings
global health = 100;  # ERROR: 'health' conflicts with property
```

Globals also cannot shadow built-in variables:

```tapir
global frame = 0;  # ERROR: 'frame' is a built-in variable
```

## Current State

### What Exists

- **Properties**: External state accessed via `GetProp`/`SetProp` bytecode
- **Built-in variables**: `frame` accessed via `GetBuiltin` infrastructure (from 003-builtin-frame-counting)
- **Symbol resolution**: `NameTable` maps identifiers to `SymbolId`
- **Type system**: Three types: `int`, `fix`, `bool`
- **Constant literals**: Parser handles int, fix, and bool literals

### What's Missing

- Grammar support for `global` declarations
- AST node for global declarations
- Compiler tracking of declared globals
- Storage in bytecode for global metadata (count, initial values)
- `GetGlobal`/`SetGlobal` bytecode instructions
- Global storage in `Vm`
- Symbol resolution for globals
- Type checking for globals
- IR instructions for global access
- Error messages for conflicts and invalid initializers

## Implementation Plan

### Phase 1: Grammar & AST

**Files**: `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`

1. Add `GlobalDeclaration` AST node:

   ```rust
   // ast.rs
   #[derive(stuff)]
   pub struct GlobalDeclaration<'input> {
       pub name: Ident<'input>,
       pub value: Expression<'input>,
       pub span: Span,
   }
   ```

2. Extend `Script` AST to include globals:

   ```rust
   pub struct Script<'input> {
       pub globals: Vec<GlobalDeclaration<'input>>,
       pub functions: Vec<Function<'input>>,
   }
   ```

3. Add grammar rule as a `TopLevelStatement` variant:

   ```lalrpop
   GlobalDecl: GlobalDeclaration<'input> = {
       <start:@L> "global" <name:Ident> "=" <value:Expression> ";" <end:@R>
           => GlobalDeclaration { name, value, span: Span::new(file_id, start, end) },
   };

   TopLevelStatement: TopLevelStatement<'input> = {
       <GlobalDecl> => TopLevelStatement::GlobalDecl(<>),
       // ... other variants ...
   }
   ```

   Globals can appear anywhere at the top level (interleaved with functions, top-level statements, etc.) and are implicitly hoisted - they're all collected and processed before any code runs.

4. Add `global` as a keyword in the lexer (`lexer.rs`).

### Phase 2: Bytecode Format

**Files**: `bytecode/src/lib.rs`

1. Add opcodes:

   ```rust
   pub enum Opcode {
       // ... existing opcodes ...
       GetGlobal,   // get_global(target: u8, global_index: u8)
       SetGlobal,   // set_global(value: u8, global_index: u8)
   }
   ```

2. Add instruction constructors:

   ```rust
   impl Type1 {
       pub const fn get_global(target: u8, global_index: u8) -> Self {
           Self::new2(Opcode::GetGlobal, target, global_index)
       }

       pub const fn set_global(value: u8, global_index: u8) -> Self {
           Self::new2(Opcode::SetGlobal, value, global_index)
       }
   }
   ```

3. Add a bytecode header. Currently there is no header - bytecode starts directly with instructions. This work introduces a header format:

   ```
   [header]
   - global_count: u32 (first word, only low byte used)
   - global_initial_values: [i32; global_count]  // All types stored as i32
   [code]
   - instructions...
   ```

   The VM will need to parse this header to determine where code starts and to initialize global storage.

### Phase 3: VM Storage & Execution

**Files**: `vm/src/lib.rs`, `vm/src/state.rs`

1. Add global storage to `Vm`:

   ```rust
   pub struct Vm<'a> {
       bytecode: &'a [u32],
       states: Vec<State>,
       frame: u32,
       globals: Box<[i32]>,  // NEW: global variable storage (fixed size, known at init)
   }
   ```

2. Initialize globals from bytecode:

   ```rust
   impl<'a> Vm<'a> {
       pub fn new(bytecode: &'a [u32]) -> Self {
           let (global_count, initial_values, code_start) = parse_header(bytecode);
           Self {
               bytecode: &bytecode[code_start..],
               states: vec![State::new(0, vec![-1])],
               frame: 0,
               globals: initial_values,
           }
       }
   }
   ```

3. Pass globals to `State::run_until_wait`:

   ```rust
   // In Vm::run_until_wait
   match self.states[state_index].run_until_wait(
       self.bytecode,
       properties,
       self.frame,
       &mut self.globals,  // NEW
   ) {
   ```

4. Handle opcodes in `State`:

   ```rust
   O::GetGlobal => {
       type1!(target, global_index);
       self.set_reg(target, globals[global_index as usize]);
   }
   O::SetGlobal => {
       type1!(value, global_index);
       globals[global_index as usize] = self.get_reg(value);
   }
   ```

### Phase 4: Symbol Resolution

**Files**: `compiler/src/compile/symtab_visitor.rs`

1. Create a `GlobalId` newtype to cleanly identify globals (avoiding bit manipulation everywhere):

   ```rust
   /// Identifies a global variable by its index in the globals array.
   #[derive(Clone, Copy, Debug, PartialEq, Eq)]
   pub struct GlobalId(pub usize);

   /// Metadata about a declared global variable.
   pub struct GlobalInfo {
       pub id: GlobalId,
       pub name: String,
       pub ty: Type,
       pub initial_value: i32,
       pub span: Span,
   }
   ```

2. Store globals in `SymTab` with lookup by name:

   ```rust
   pub struct SymTab<'input> {
       // ... existing fields ...
       globals: Vec<GlobalInfo>,
       global_names: HashMap<Cow<'input, str>, GlobalId>,
   }

   impl SymTab {
       pub fn get_global_by_name(&self, name: &str) -> Option<&GlobalInfo> {
           self.global_names.get(name).map(|id| &self.globals[id.0])
       }

       pub fn get_global(&self, id: GlobalId) -> &GlobalInfo {
           &self.globals[id.0]
       }

       pub fn globals(&self) -> &[GlobalInfo] {
           &self.globals
       }
   }
   ```

3. Populate globals into the symbol table before processing functions. Check for conflicts with properties and built-ins, and validate constant initializers (reuse existing `CannotShadowBuiltin` error):

   ```rust
   // In SymTabVisitor::visit
   for (index, global) in script.globals.iter().enumerate() {
       // Check for conflicts with properties
       if settings.properties.iter().any(|p| p.name == global.name.name) {
           self.diagnostics.add_message(
               CompilerErrorKind::GlobalConflictsWithProperty { name: global.name.name },
               global.span,
           );
           continue;
       }

       // Check for conflicts with built-ins (reuse existing error)
       if BuiltinVariable::from_name(global.name.name).is_some() {
           self.diagnostics.add_message(
               CompilerErrorKind::CannotShadowBuiltin { name: global.name.name },
               global.span,
           );
           continue;
       }

       // Validate initializer is a constant and infer type.
       // Returns Type::Error and reports diagnostic if not a constant.
       let (ty, initial_value) = evaluate_constant_initializer(
           &global.value,
           &mut self.diagnostics,
       );

       let global_id = GlobalId(index);
       self.symtab.add_global(GlobalInfo {
           id: global_id,
           name: global.name.name.to_string(),
           ty,
           initial_value,
           span: global.span,
       });
   }
   ```

   The `evaluate_constant_initializer` function:
   - If the expression is a literal, returns its type and value
   - If not a constant, reports `GlobalInitializerNotConstant` error and returns `(Type::Error, 0)`

4. Update `NameTable::get` to check globals. Local variables can shadow globals, so check locals/properties first, then globals:

   ```rust
   pub fn get(&self, name: &str) -> Option<SymbolId> {
       // Check built-ins first (cannot be shadowed)
       if let Some(builtin) = BuiltinVariable::from_name(name) {
           return Some(builtin.symbol_id());
       }

       // Check local variables and properties (can shadow globals)
       if let Some(id) = self.lookup_local_or_property(name) {
           return Some(id);
       }

       // Check globals last (can be shadowed by locals)
       if let Some(global) = self.symtab.get_global_by_name(name) {
           return Some(global.id.to_symbol_id());
       }

       None
   }
   ```

   This allows patterns like:

   ```tapir
   global counter = 0;

   fn example() {
       var counter = 10;  # shadows the global
       # uses local counter here
   }
   # global counter still accessible outside
   ```

5. Add conversion methods to `GlobalId` for encoding/decoding from `SymbolId`:

   ```rust
   impl GlobalId {
       const GLOBAL_BIT: usize = 1 << (usize::BITS - 2);

       /// Convert this GlobalId to a SymbolId for use in the symbol table.
       pub fn to_symbol_id(self) -> SymbolId {
           SymbolId(Self::GLOBAL_BIT | self.0)
       }

       /// Try to extract a GlobalId from a SymbolId.
       /// Returns None if this SymbolId doesn't represent a global.
       pub fn from_symbol_id(id: SymbolId) -> Option<Self> {
           if id.0 & Self::GLOBAL_BIT != 0 {
               Some(GlobalId(id.0 & !Self::GLOBAL_BIT))
           } else {
               None
           }
       }
   }
   ```

   Usage: `GlobalId::from_symbol_id(symbol)` instead of `symbol.as_global()`.

### Phase 5: Type Checking

**Files**: `compiler/src/compile/type_visitor.rs`

1. Handle global types in `type_for_symbol`. Check globals last (after locals, properties, built-ins):

   ```rust
   fn type_for_symbol(&self, symbol: SymbolId) -> Option<Type> {
       // Check built-ins
       if let Some(builtin) = BuiltinVariable::from_symbol_id(symbol) {
           return Some(builtin.ty());
       }

       // Check locals and properties
       if let Some(ty) = self.lookup_local_or_property_type(symbol) {
           return Some(ty);
       }

       // Check globals last
       if let Some(global_id) = GlobalId::from_symbol_id(symbol) {
           return Some(self.symtab.get_global(global_id).ty);
       }

       None
   }
   ```

Note: Constant validation for global initializers happens in symbol resolution (Phase 4), not type checking. Type checking just uses the already-inferred type from `GlobalInfo`.

### Phase 6: IR Generation

**Files**: `compiler/src/compile/ir.rs`, `compiler/src/compile/ir/lowering.rs`

1. Add IR instructions:

   ```rust
   pub enum TapIr {
       // ... existing instructions ...
       GetGlobal { target: SymbolId, global_id: GlobalId },
       SetGlobal { global_id: GlobalId, value: SymbolId },
   }
   ```

2. Handle global reads in expression lowering:

   ```rust
   ast::ExpressionKind::Variable(_) => {
       let source = *expr.meta.get().expect("Should've resolved variable");

       if let Some(builtin) = BuiltinVariable::from_symbol_id(source) {
           self.current_block.push(TapIr::GetBuiltin { target: target_symbol, builtin });
       } else if let Some(global_id) = GlobalId::from_symbol_id(source) {
           self.current_block.push(TapIr::GetGlobal {
               target: target_symbol,
               global_id,
           });
       } else if let Some(property) = symtab.get_property(source) {
           self.current_block.push(TapIr::GetProp {
               target: target_symbol,
               prop_index: property.index,
           });
       } else {
           self.current_block.push(TapIr::Move { target: target_symbol, source });
       }
   }
   ```

3. Handle global writes in assignment lowering:

   ```rust
   if let Some(global_id) = GlobalId::from_symbol_id(target_symbol) {
       let temp = symtab.new_temporary();
       // ... evaluate expression into temp ...
       self.current_block.push(TapIr::SetGlobal {
           global_id,
           value: temp,
       });
   }
   ```

4. Update `targets()` and `sources()` for the new instructions.

5. **Optimization considerations**: Like properties, globals can change between `wait` points (another thread could modify them). Optimization passes must treat `GetGlobal` similarly to `GetProp` - the value cannot be cached across wait boundaries. Ensure existing optimization passes that handle property loads apply the same logic to global loads.

### Phase 7: Bytecode Emission

**Files**: `compiler/src/compile.rs`

1. Emit bytecode header with global metadata:

   ```rust
   fn emit_header(&mut self, globals: &[GlobalInfo]) {
       // Encode global count and initial values
       self.bytecode.push(globals.len() as u32);
       for global in globals {
           self.bytecode.push(global.initial_value as u32);
       }
   }
   ```

2. Emit instructions:

   ```rust
   TapIr::GetGlobal { target, global_id } => {
       self.bytecode.push(
           bytecode::Type1::get_global(v(target), global_id.0 as u8).encode()
       );
   }
   TapIr::SetGlobal { global_id, value } => {
       self.bytecode.push(
           bytecode::Type1::set_global(v(value), global_id.0 as u8).encode()
       );
   }
   ```

### Phase 8: Error Messages

**Files**: `compiler/src/reporting.rs`, `compiler/src/reporting/format.rs`

Add new error variants. Reuse existing errors where possible:

```rust
pub enum CompilerErrorKind {
    // ... existing variants ...

    // NEW: specific to globals
    GlobalConflictsWithProperty { name: String },
    GlobalInitializerNotConstant { name: String },

    // REUSE existing errors:
    // - CannotShadowBuiltin: for globals that conflict with built-ins like `frame`
    // - TypeMismatch: if we add explicit type annotations later
}
```

Example error output:

```
error: global variable conflicts with property
  --> script.tapir:1:8
   |
 1 | global health = 100;
   |        ^^^^^^
   |
   = note: 'health' is already defined as a property

error: global initializer must be a constant
  --> script.tapir:2:16
   |
 2 | global foo = 3 + 2;
   |              ^^^^^
   |
   = note: use a literal value, then assign at the top level if needed
```

### Phase 9: Disassembler & Pretty Printing

**Files**: `compiler/src/compile/disassemble.rs`, `compiler/src/compile/ir/pretty_print.rs`

Update the disassembler to handle the new bytecode header and instructions.

Update IR pretty printing for the new instructions.

## Testing Strategy

### Unit Tests

1. **Parsing**:
   - `global foo = 3;` - OK
   - `global bar = 1.5;` - OK
   - `global baz = true;` - OK
   - `global bad = 3 + 2;` - parses OK (expression accepted)

2. **Symbol Resolution / Constant Validation**:
   - Global visible in all functions
   - Global visible in spawned threads
   - Local variable can shadow global (creates separate local, global still accessible elsewhere)
   - Global cannot shadow property - ERROR
   - Global cannot shadow built-in - ERROR
   - `global bad = 3 + 2;` - ERROR (expression, not constant)
   - `global bad = some_fn();` - ERROR (function call, not constant)
   - Inferred types match initializers (int, fix, bool)

3. **Type Checking**:
   - Global types correctly resolved in expressions
   - Type mismatches with globals reported correctly

4. **IR Generation**: Snapshot tests showing `GetGlobal`/`SetGlobal` instructions

5. **Optimizations**:
   - Globals not incorrectly cached across `wait` boundaries
   - Multiple reads of same global within one wait-bounded section can be optimized
   - Global writes correctly invalidate cached reads

### Integration Tests

1. **Basic read/write**:

   ```tapir
   global counter = 0;

   counter = counter + 1;
   int_prop = counter;
   ```

   After one `run()`: `int_prop` equals 1.

2. **Persistence across frames**:

   ```tapir
   global counter = 0;

   loop {
       counter = counter + 1;
       int_prop = counter;
       wait;
   }
   ```

   After N calls to `run()`: `int_prop` equals N.

3. **Shared across threads**:

   ```tapir
   global counter = 0;

   spawn increment();
   spawn increment();
   wait;
   int_prop = counter;

   fn increment() {
       counter = counter + 1;
   }
   ```

   After one `run()`: `int_prop` equals 2.

4. **Multiple globals**:

   ```tapir
   global a = 1;
   global b = 2;
   global c = 3;

   int_prop = a + b + c;
   ```

   After one `run()`: `int_prop` equals 6.

5. **Fix type global**:

   ```tapir
   global scale = 1.5;

   fix_prop = scale * 2;
   ```

6. **Bool type global**:

   ```tapir
   global enabled = true;

   if enabled {
       int_prop = 1;
   } else {
       int_prop = 0;
   }
   ```

7. **Local shadowing**:

   ```tapir
   global counter = 100;

   int_prop = counter;  # global, equals 100

   fn uses_local() {
       var counter = 5;  # shadows global
       int_prop = counter;  # local, equals 5
   }
   ```

8. **Capture global into local** (important pattern):

   ```tapir
   global value = 10;

   spawn modifier();
   capture_test();

   fn capture_test() {
       var value = value;  # capture global into local, shadows it
       wait;               # global may change here
       wait;
       int_prop = value;   # still 10, the captured value
   }

   fn modifier() {
       value = 99;  # modifies global
   }
   ```

   After `run()` calls complete: `int_prop` equals 10 (the captured value), not 99.

   This pattern lets functions "snapshot" a global's value at entry and use it consistently regardless of concurrent modifications.

9. **Globals with wait (optimization correctness)**:

   ```tapir
   global value = 0;

   spawn modifier();
   var before = value;
   wait;
   var after = value;
   int_prop = after - before;  # should be 1, not 0

   fn modifier() {
       value = value + 1;
   }
   ```

   This tests that the optimizer doesn't incorrectly cache `value` across the `wait`.

### Error Message Tests

Verify clear, helpful error messages for all failure modes.

## Alternatives Considered

### Hidden Properties

Have the compiler automatically inject globals as properties with reserved indices:

**Rejected because**:

- Complicates the property trait implementation
- Mixes script-internal concerns with host-facing API
- Requires proc macro changes to handle reserved property indices
- VM-managed is cleaner separation of concerns

### Expression Initializers

Allow `global foo = 3 + 2;` or function calls:

**Rejected because**:

- Initialization order becomes complex
- What if initializer calls `wait`?
- When do spawned threads see initialized vs uninitialized?
- Constants are simple and predictable

### Reset on Each Run

Globals reset to initial values each `run()` call:

**Rejected because**:

- This is what local variables already do
- Persistence is the unique value proposition of globals
- Users expect "global" to mean persistent state

## Dependencies

- No external crate dependencies

## Implementation Order

The phases should be implemented in order, as each builds on the previous:

1. Grammar & AST - enables parsing
2. Bytecode format - defines the target
3. VM storage - enables execution
4. Symbol resolution - enables name lookup
5. Type checking - enables validation
6. IR generation - enables code generation
7. Bytecode emission - enables full compilation
8. Error messages - enables good UX
9. Disassembler - enables debugging

Testing should be added incrementally with each phase.
