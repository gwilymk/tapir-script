# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

```bash
# Build a specific crate
cargo build -p compiler
cargo build -p vm
cargo build -p bytecode
cargo build -p tapir-script

# Run all tests for a crate
cargo test -p compiler
cargo test -p vm

# Run a single test
cargo test -p compiler test_name

# Update snapshots (uses insta)
cargo insta test -p compiler
cargo insta review
```

## CLI Usage

The `tapirc` CLI is in the `tapir-cli` crate. Prefer using stdin with `-` for quick tests:

```bash
# Quick compile test (preferred - reads from stdin)
echo 'property x: int; x = 42;' | cargo run -p tapir-cli -- compile -

# Compile a file
cargo run -p tapir-cli -- compile path/to/script.tapir

# Disable optimizations (shows unoptimized bytecode)
echo 'property x: int; x = 42;' | cargo run -p tapir-cli -- compile --no-opt -

# Disable the prelude (no built-in functions)
cargo run -p tapir-cli -- compile --no-prelude script.tapir
```

## Architecture

Tapir-script is a scripting language for defining game animations asynchronously. Scripts execute frame-by-frame using `wait` to yield control, making it easy to express animations, cutscenes, and state machines. It compiles to bytecode that runs on a register-based virtual machine.

### Crate Dependencies

```
tapir-script (public facade)
    └── vm (runtime)
    │   └── bytecode (instruction encoding)
    └── tapir-script-macros
        └── tapir-script-macros-core

compiler (standalone, used at build time)
    └── bytecode

tapir-cli (command-line tool)
    └── compiler
```

### Compilation Pipeline

```
Source → Lexer (logos) → Parser (LALRPOP) → AST → Symbol Resolution → Type Checking → IR → Optimizations → Register Allocation → Bytecode
```

Key compiler stages in `compiler/src/`:

- `lexer.rs` - Tokenization via logos
- `grammar.lalrpop` - Parser grammar (generates `grammar.rs` via build.rs)
- `ast.rs` - AST node definitions
- `compile/compile.rs` - Main compilation orchestrator
- `compile/symtab_visitor.rs` - Symbol table construction
- `compile/type_visitor.rs` - Type checking
- `compile/ir.rs` - Intermediate representation
- `compile/ir/regalloc.rs` - Register allocation
- `compile/ir/optimisations/` - Optimization passes

### Bytecode Format

32-bit instructions with 8-bit opcode prefix. Two instruction formats:

- **Type1**: opcode(8) + target(8) + a(8) + b(8) - most operations
- **Type3**: opcode(8) + immediate(24) - jumps

See `docs/bytecode-reference.md` for the complete bytecode specification.

### VM Runtime

Register-based VM in `vm/src/`:

- `lib.rs` - `TapirScript` trait and `Script` wrapper
- `state.rs` - Execution state, instruction dispatch

Features: concurrent script execution (spawn), property access, event handlers, external function calls.

### Type System

Static, strongly-typed with three types: `int` (32-bit), `fix` (24.8 fixed-point via agb_fixnum), `bool`.

### Language Reference

See `docs/tapir-reference.md` for the complete tapir-script language syntax and features.

### Snapshot Testing

Tests use `insta` with RON format. Snapshots are in `snapshot_tests/` directories within each crate's source tree.
Look at the `.snap.new` files yourself to decide if you're happy with the test results, but NEVER accept them. Always
stop and ask me to review the snapshots and accept them myself.
