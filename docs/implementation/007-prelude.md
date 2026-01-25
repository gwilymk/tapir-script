# Implementation Plan: Prelude Support for Tapir-Script

## Overview

Add a prelude file (`compiler/stdlib/prelude.tapir`) containing utility functions that is automatically included in every compilation. The prelude is treated as a separate file with its own `FileId` so error messages correctly reference the prelude source when errors occur there.

## Architecture

```
Prelude (FileId=0, "stdlib/prelude.tapir")
    └── Lexer → Parser → AST (spans contain FileId=0)

User Code (FileId=1, user's filename)
    └── Lexer → Parser → AST (spans contain FileId=1)

        ↓ Merge ASTs (prelude first, then user)

    Combined AST → Symbol Table → Type Check → IR → Bytecode

DiagnosticCache contains both files for error reporting
```

## Entry Points

Two main entry points need prelude support:

- `compile::compile()` - full compilation to bytecode
- `compile::analyse::analyse()` - analysis for LSP tooling

Internal test modules (ir.rs, optimisations.rs, ssa.rs, etc.) should NOT use the prelude - they test compiler internals in isolation.

## Files to Create

### 1. `compiler/stdlib/prelude.tapir`

Empty prelude file as a starting point for utility functions.

```tapir
# Tapir-script prelude - utility functions available to all scripts
```

### 2. `compiler/src/prelude.rs`

New module to handle prelude loading and parsing.

```rust
use std::path::Path;

use crate::{
    ast::Script,
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::FileId,
};

const PRELUDE_SOURCE: &str = include_str!("../stdlib/prelude.tapir");
const PRELUDE_FILENAME: &str = "stdlib/prelude.tapir";

pub const PRELUDE_FILE_ID: FileId = FileId::new(0);
pub const USER_FILE_ID: FileId = FileId::new(1);

/// Parse user code with the prelude merged in.
///
/// Returns the merged AST on success, or None if parsing failed
/// (errors will be added to diagnostics).
pub fn parse_with_prelude<'input>(
    user_filename: impl AsRef<Path>,
    user_input: &'input str,
    diagnostics: &mut Diagnostics,
) -> Option<Script<'input>> {
    // Add prelude source to diagnostics cache for error reporting
    diagnostics.add_file(PRELUDE_FILE_ID, PRELUDE_FILENAME, PRELUDE_SOURCE);

    let parser = grammar::ScriptParser::new();

    // Parse prelude
    let prelude_lexer = Lexer::new(PRELUDE_SOURCE, PRELUDE_FILE_ID);
    let prelude_ast = match parser.parse(PRELUDE_FILE_ID, diagnostics, prelude_lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, PRELUDE_FILE_ID);
            return None;
        }
    };

    // Parse user code
    let user_lexer = Lexer::new(user_input, USER_FILE_ID);
    let mut user_ast = match parser.parse(USER_FILE_ID, diagnostics, user_lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, USER_FILE_ID);
            return None;
        }
    };

    // Merge prelude into user AST
    user_ast.merge_from(prelude_ast);

    Some(user_ast)
}
```

## Files to Modify

### 3. `compiler/src/lib.rs`

Add the prelude module.

```rust
mod prelude;
```

### 4. `compiler/src/reporting/format.rs`

Add method to insert additional files into `DiagnosticCache`.

```rust
impl DiagnosticCache {
    pub fn add_file(&mut self, file_id: FileId, filename: impl AsRef<Path>, content: &str) {
        self.map.insert(
            file_id,
            (filename.as_ref().to_string_lossy().into_owned(), Source::from(content.to_string())),
        );
    }
}
```

### 5. `compiler/src/reporting.rs`

Expose `add_file` through `Diagnostics`.

```rust
impl Diagnostics {
    pub fn add_file(&mut self, file_id: FileId, filename: impl AsRef<Path>, content: &str) {
        self.cache.add_file(file_id, filename, content);
    }
}
```

### 6. `compiler/src/ast.rs`

Add method to merge two `Script` ASTs.

```rust
impl<'input> Script<'input> {
    /// Merge another script into this one. The other script's definitions
    /// are prepended (so prelude functions come before user functions).
    pub fn merge_from(&mut self, other: Script<'input>) {
        // For functions: keep self's @toplevel at [0], insert other's non-toplevel functions after
        let other_functions: Vec<_> = other.functions.into_iter()
            .filter(|f| f.name != "@toplevel")
            .collect();

        // Insert after @toplevel
        self.functions.splice(1..1, other_functions);

        // Prepend other declarations
        self.property_declarations.splice(0..0, other.property_declarations);
        self.globals.splice(0..0, other.globals);
        self.extern_functions.splice(0..0, other.extern_functions);
    }
}
```

### 7. `compiler/src/compile.rs`

Update `compile()` to use `parse_with_prelude()`.

```rust
use crate::prelude::{self, USER_FILE_ID};

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> Result<Bytecode, Diagnostics> {
    let mut diagnostics = Diagnostics::new(USER_FILE_ID, &filename, input);

    let mut ast = match prelude::parse_with_prelude(&filename, input, &mut diagnostics) {
        Some(ast) => ast,
        None => return Err(diagnostics),
    };

    // Rest of compilation unchanged...
    let mut sym_tab_visitor = SymTabVisitor::new(settings, &mut ast, &mut diagnostics);
    // ...
}
```

### 8. `compiler/src/compile/analyse.rs`

Update `analyse()` to use `parse_with_prelude()`.

```rust
use crate::prelude::{self, USER_FILE_ID};

pub fn analyse(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> AnalysisResult {
    let mut diagnostics = Diagnostics::new(USER_FILE_ID, filename, input);

    let mut ast = match prelude::parse_with_prelude(&filename, input, &mut diagnostics) {
        Some(ast) => ast,
        None => {
            return AnalysisResult {
                diagnostics,
                symbols: vec![],
                // ... other empty fields
            };
        }
    };

    // Rest of analysis unchanged...
}
```

## Implementation Order

1. Create `compiler/stdlib/prelude.tapir` (empty/minimal)
2. Add `DiagnosticCache::add_file()` in `reporting/format.rs`
3. Add `Diagnostics::add_file()` in `reporting.rs`
4. Add `Script::merge_from()` in `ast.rs`
5. Create `compiler/src/prelude.rs` module
6. Add `mod prelude;` to `lib.rs`
7. Update `compile()` in `compile.rs`
8. Update `analyse()` in `analyse.rs`

## Verification

1. **Build test**: `cargo build -p compiler`
2. **Run existing tests**: `cargo test -p compiler` - all should pass
3. **Test error in prelude**: Temporarily add invalid syntax to prelude, verify error message shows `core/prelude.tapir` as the source
4. **Test prelude function**: Add a simple function to prelude, call it from user code, verify it compiles and runs

## Edge Cases Handled

- **Empty prelude**: Works fine, merge is a no-op
- **Prelude parse errors**: Reported with correct file location
- **Name conflicts**: Symbol table will catch duplicate function names with proper spans
- **@toplevel handling**: Prelude's @toplevel is filtered out during merge (only user's top-level code runs)
