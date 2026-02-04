# Importing Other Tapir Script Files

## Overview

This document describes the design for allowing one tapir script file to import definitions from another. The primary motivation is sharing definitions across multiple scripts that operate on the same game object.

## Motivation

Games often have multiple scripts that share the same underlying data structure. For example, a character might have separate scripts for movement, combat, and animation—all operating on the same properties:

```tapir
# Without imports, every script repeats:
struct Point { x: int, y: int }
property pos: Point;
property health: int;
extern fn play_sound(id: int);

# Actual script code...
```

With imports, common definitions live in a shared file:

```tapir
# character/common.tapir
struct Point { x: int, y: int }
property pos: Point;
property health: int;
extern fn play_sound(id: int);
```

```tapir
# character/movement.tapir
import common;

# Can use pos, Point, etc.
pos.x = pos.x + 1;
```

```tapir
# character/combat.tapir
import common;

# Same properties, different behavior
if health <= 0 {
    play_sound(DEATH_SOUND);
}
```

This enables a generic Rust struct to host multiple scripts that share property definitions.

## What Can Be Imported?

All top-level declarations are importable:

| Declaration | Importable? | Notes                                    |
| ----------- | ----------- | ---------------------------------------- |
| `struct`    | Yes         | Type definitions                         |
| `fn`        | Yes         | Utility functions                        |
| `property`  | Yes         | Shared across scripts on the same object |
| `global`    | Yes         | Shared state                             |
| `extern fn` | Yes         | External function declarations           |
| `event fn`  | Yes         | Event handlers (on frame, on init, etc.) |

## Syntax

```tapir
import common;              # imports ./common.tapir
import utils/math;          # imports ./utils/math.tapir
import ../shared/types;     # imports ../shared/types.tapir
```

The import statement takes a path (without the `.tapir` extension). Paths use `/` as separator and are relative to the importing file's directory.

All definitions from the imported file become available in the importing file's namespace.

### Path Resolution

Paths are relative to the importing file's directory:

```
project/
├── common.tapir
├── character/
│   ├── common.tapir
│   ├── movement.tapir
│   └── combat.tapir
└── utils/
    └── math.tapir
```

Examples from `character/movement.tapir`:

- `import common;` → `character/common.tapir`
- `import ../common;` → `common.tapir` (project root)
- `import ../utils/math;` → `utils/math.tapir`

### Deduplication

If multiple files import the same module, it's only processed once. This eliminates the "diamond problem" without needing C-style include guards.

```tapir
# a.tapir imports common.tapir
# b.tapir imports common.tapir
# main.tapir imports both a and b

import a;      # loads a.tapir, which loads common.tapir
import b;      # loads b.tapir, common.tapir already loaded - skip
```

Implementation: track imported files by canonicalized path. Before processing an import, check if already imported.

### Circular Import Detection

Circular imports must be detected and reported as errors:

```tapir
# a.tapir
import b;

# b.tapir
import a;  # ERROR: circular import detected: a.tapir -> b.tapir -> a.tapir
```

Implementation: maintain a stack of files being processed. Before processing an import, check if the target is already on the stack.

### Prelude

The prelude is treated as an implicit `import prelude;` at the start of every file. It uses the same deduplication logic as regular imports—if multiple files are compiled together, the prelude is only processed once.

The prelude provides built-in functions like `abs()`, `min()`, `max()`, etc.

The `--no-prelude` flag disables this implicit import.

### Duplicate Definition Handling

If different files define the same name locally, that's a conflict:

```tapir
# a.tapir
struct Point { x: int, y: int }  # defined locally in a

# b.tapir
struct Point { x: int, y: int }  # defined locally in b (different file!)

# main.tapir
import a;  # brings Point from a
import b;  # ERROR: Point already defined (from a.tapir)
```

Local definitions also conflict with imports:

```tapir
import a;                        # brings Point
struct Point { x: int, y: int }  # ERROR: Point already imported from a.tapir
```

Note: Diamond imports are **not** conflicts because of deduplication. If both `a` and `b` import `common` (which defines Point), importing both `a` and `b` is fine—`common` is only loaded once.

### Transitive Imports

Imports are transitive. If `a.tapir` imports `common.tapir`, and `main.tapir` imports `a.tapir`, then `main.tapir` gets all definitions from both files.

```tapir
# common.tapir
struct Point { x: int, y: int }

# a.tapir
import common;
fn make_point() -> Point { return Point(0, 0); }

# main.tapir
import a;
var p = make_point();  # OK - make_point from a.tapir
var q: Point;          # OK - Point came transitively via a -> common
```

Combined with deduplication, diamond imports work cleanly:

```tapir
# main.tapir
import a;  # brings common transitively
import b;  # also imports common, but it's already loaded - no conflict
```

## Implementation Considerations

### Current Compile API

The current `compile` function takes a filename and source string:

```rust
pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> Result<CompileOutput, Diagnostics>
```

This works well for single-file compilation and testing (you can pass source strings directly). With imports, we need to load additional files while keeping this testability.

### Compilation Model

Use a load-and-merge approach:

1. Parse the main file
2. For each import, recursively parse the imported file (if not already loaded)
3. Merge imported definitions into main file's AST
4. Continue with normal compilation (struct registration, symbol resolution, type checking, codegen)

### FileLoader Trait

Abstract file loading to support both real filesystem and test mocks:

```rust
pub trait FileLoader {
    /// Load file contents. Returns None if file doesn't exist.
    fn load(&self, path: &Path) -> Option<String>;

    /// Canonicalize a path for deduplication.
    /// Returns None if path doesn't exist.
    fn canonicalize(&self, path: &Path) -> Option<PathBuf>;
}

/// Loads files from the real filesystem.
pub struct FsFileLoader;

impl FileLoader for FsFileLoader {
    fn load(&self, path: &Path) -> Option<String> {
        std::fs::read_to_string(path).ok()
    }

    fn canonicalize(&self, path: &Path) -> Option<PathBuf> {
        path.canonicalize().ok()
    }
}

/// Test file loader with in-memory files.
///
/// Designed for easy test case definition:
/// ```rust
/// let loader = TestFileLoader::new()
///     .with_file("common.tapir", "struct Point { x: int, y: int }")
///     .with_file("main.tapir", "import common; property pos: Point;");
///
/// let result = compile("main.tapir", loader.get("main.tapir"), &settings, &loader);
/// ```
pub struct TestFileLoader {
    files: HashMap<PathBuf, String>,
}

impl TestFileLoader {
    pub fn new() -> Self {
        Self { files: HashMap::new() }
    }

    /// Add a file to the loader. Paths are relative to a virtual root.
    /// Returns self for chaining.
    pub fn with_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.files.insert(path.into(), content.into());
        self
    }

    /// Get the content of a file (for passing to compile as the main input).
    pub fn get(&self, path: impl AsRef<Path>) -> &str {
        self.files.get(path.as_ref())
            .map(|s| s.as_str())
            .expect("test file not found")
    }
}

impl FileLoader for TestFileLoader {
    fn load(&self, path: &Path) -> Option<String> {
        self.files.get(path).cloned()
    }

    fn canonicalize(&self, path: &Path) -> Option<PathBuf> {
        // For tests, just normalize the path (resolve .. segments)
        let mut result = PathBuf::new();
        for component in path.components() {
            match component {
                std::path::Component::ParentDir => { result.pop(); }
                std::path::Component::Normal(s) => result.push(s),
                std::path::Component::CurDir => {}
                _ => result.push(component),
            }
        }
        if self.files.contains_key(&result) {
            Some(result)
        } else {
            None
        }
    }
}

// Example test:
#[test]
fn test_import_struct() {
    let loader = TestFileLoader::new()
        .with_file("types.tapir", "struct Point { x: int, y: int }")
        .with_file("main.tapir", r#"
            import types;
            property pos: Point;
        "#);

    let settings = CompileSettings::default();
    let result = compile("main.tapir", loader.get("main.tapir"), &settings, &loader);
    assert!(result.is_ok());
}

#[test]
fn test_circular_import_error() {
    let loader = TestFileLoader::new()
        .with_file("a.tapir", "import b;")
        .with_file("b.tapir", "import a;");

    let settings = CompileSettings::default();
    let result = compile("a.tapir", loader.get("a.tapir"), &settings, &loader);
    assert!(result.is_err());
    // Check for circular import error message
}

#[test]
fn test_diamond_import() {
    let loader = TestFileLoader::new()
        .with_file("common.tapir", "struct Point { x: int, y: int }")
        .with_file("a.tapir", "import common;")
        .with_file("b.tapir", "import common;")
        .with_file("main.tapir", r#"
            import a;
            import b;
            property pos: Point;  # Point available via transitive import
        "#);

    let settings = CompileSettings::default();
    let result = compile("main.tapir", loader.get("main.tapir"), &settings, &loader);
    assert!(result.is_ok());
}
```

### Compile API Changes

The compile function gains a file loader parameter:

```rust
/// Compile with a file loader for resolving imports.
pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
    file_loader: &dyn FileLoader,
) -> Result<CompileOutput, Diagnostics>

/// Convenience: compile from filesystem (loads main file via loader).
pub fn compile_file(
    path: impl AsRef<Path>,
    settings: &CompileSettings,
) -> Result<CompileOutput, Diagnostics> {
    let loader = FsFileLoader;
    let input = loader.load(path.as_ref())
        .ok_or_else(|| /* file not found error */)?;
    compile(path, &input, settings, &loader)
}
```

### Source Storage and Lifetimes

The AST uses `'input` lifetimes tied to source strings. With multiple files, we need storage that outlives parsing:

```rust
/// Stores source code for all loaded files.
pub struct SourceStore {
    sources: Vec<String>,  // Indexed by FileId
    paths: Vec<PathBuf>,
}

impl SourceStore {
    pub fn new() -> Self {
        Self { sources: Vec::new(), paths: Vec::new() }
    }

    /// Add a source file, returns its FileId.
    pub fn add(&mut self, path: PathBuf, source: String) -> FileId {
        let id = FileId(self.sources.len() as u32);
        self.sources.push(source);
        self.paths.push(path);
        id
    }

    /// Get source by FileId.
    pub fn get(&self, id: FileId) -> &str {
        &self.sources[id.0 as usize]
    }

    pub fn path(&self, id: FileId) -> &Path {
        &self.paths[id.0 as usize]
    }
}
```

The compilation context holds the source store:

```rust
struct CompilationContext<'loader> {
    source_store: SourceStore,
    loaded_files: HashSet<PathBuf>,  // Canonicalized paths for dedup
    import_stack: Vec<PathBuf>,       // For cycle detection
    file_loader: &'loader dyn FileLoader,
    diagnostics: Diagnostics,
}
```

### AST Changes

```rust
#[derive(Clone, Debug, Serialize)]
pub struct ImportDeclaration<'input> {
    pub path: ImportPath<'input>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct ImportPath<'input> {
    pub segments: Vec<ImportPathSegment<'input>>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub enum ImportPathSegment<'input> {
    Ident(&'input str),
    DotDot,  // ..
}

// Add to Script
pub struct Script<'input> {
    pub imports: Vec<ImportDeclaration<'input>>,  // NEW
    pub struct_declarations: Vec<StructDeclaration<'input>>,
    // ... rest unchanged
}
```

### Import Resolution Algorithm

Following the tapir error handling philosophy, import resolution should accumulate as many errors as possible rather than bailing out early. Report all file-not-found errors, all circular imports, and all duplicate definitions in one pass.

```rust
impl<'loader> CompilationContext<'loader> {
    /// Resolve imports for a file. Errors go into diagnostics.
    /// Returns true if resolution succeeded (no errors added).
    fn resolve_imports<'input>(
        &mut self,
        ast: &mut Script<'input>,
        file_path: &Path,
        file_id: FileId,
    ) -> bool {
        let canonical = match self.file_loader.canonicalize(file_path) {
            Some(c) => c,
            None => {
                // File doesn't exist - report but continue with other imports
                self.diagnostics.report_file_not_found(file_path, ...);
                return false;
            }
        };

        // Cycle detection - report error but continue processing other imports
        if self.import_stack.contains(&canonical) {
            self.diagnostics.report_circular_import(&self.import_stack, &canonical);
            return false;
        }

        // Already fully processed? Skip (not an error).
        if self.loaded_files.contains(&canonical) {
            return true;
        }

        self.import_stack.push(canonical.clone());
        let mut success = true;

        // Process ALL imports, even if some fail
        for import in &ast.imports {
            let import_path = self.resolve_import_path(file_path, &import.path);

            // Try to load file
            let source = match self.file_loader.load(&import_path) {
                Some(s) => s,
                None => {
                    self.diagnostics.report_file_not_found(&import_path, import.span);
                    success = false;
                    continue;  // Try other imports
                }
            };

            let import_file_id = self.source_store.add(import_path.clone(), source);
            self.diagnostics.add_file(import_file_id, &import_path,
                self.source_store.get(import_file_id));

            // Parse - continue even if parsing fails (parser accumulates errors)
            let imported_ast = match parse(self.source_store.get(import_file_id), ...) {
                Some(ast) => ast,
                None => {
                    success = false;
                    continue;
                }
            };

            // Recursively resolve (accumulates its own errors)
            if !self.resolve_imports(&imported_ast, &import_path, import_file_id) {
                success = false;
                // Continue anyway - try to merge what we can
            }

            // Merge definitions - reports duplicate errors but continues
            if !self.merge_definitions(ast, &imported_ast) {
                success = false;
            }
        }

        self.import_stack.pop();
        self.loaded_files.insert(canonical);
        success
    }

    fn merge_definitions<'a, 'b>(
        &mut self,
        target: &mut Script<'a>,
        source: &Script<'b>,
    ) -> bool {
        let mut success = true;

        // Check each definition for conflicts, report all of them
        for struct_def in &source.struct_declarations {
            if self.has_definition(target, struct_def.name) {
                self.diagnostics.report_duplicate_definition(...);
                success = false;
                // Don't add duplicate, but continue checking others
            } else {
                // Add to target
            }
        }
        // Same for functions, properties, globals, extern fns, event handlers...

        success
    }
}
```

### Error Reporting Across Files

`Diagnostics` already supports multiple files via `add_file()`. Each loaded file gets registered with the diagnostic cache so errors can point to the correct source locations.

### Lifetime Challenge: AST Merging

The AST uses `'input` lifetimes tied to the source string. When merging ASTs from different files, they have different lifetimes:

```rust
// main.tapir source -> Script<'main>
// imported.tapir source -> Script<'imported>
// Can't directly merge these!
```

**Solution: Arena allocation with `bumpalo`**

Use `bumpalo` which provides `&self` allocation via interior mutability:

```rust
use bumpalo::Bump;

let arena = Bump::new();
let s1: &str = arena.alloc_str(&source1);
let s2: &str = arena.alloc_str(&source2);  // Fine, both borrow &arena

// All AST nodes share the arena lifetime
let ast1: Script<'_> = parse(s1);
let ast2: Script<'_> = parse(s2);
// Can merge ast1 and ast2 since they have the same lifetime
```

This works because `bumpalo` uses interior mutability and careful unsafe code to allow concurrent borrows during allocation.

- Create a `Bump` arena at the start of compilation
- Add each source file to the arena via `arena.alloc_str()`, getting back `&'arena str`
- All ASTs share the `'arena` lifetime
- Merging ASTs with the same lifetime is straightforward

This keeps the existing AST structure intact and doesn't require changing `&'input str` to owned strings throughout the codebase. Add `bumpalo` as a dependency to the compiler crate.

### Project Root and LSP

The LSP operates lazily, loading files as it encounters them - the same approach it uses today.

When the LSP opens `character/movement.tapir`:

1. Parse the file and discover its imports
2. Load and parse each imported file (recursively following their imports)
3. Type-check with the resolved definitions

This means each file is treated as its own "entry point". If `movement.tapir` uses `Point` but doesn't import `common` (relying on some parent file to provide it transitively), it will show as an error. This encourages explicit imports and self-contained files.

**No workspace scanning required**: The LSP doesn't need to discover all `.tapir` files upfront. It just follows imports from the currently open file, loading them on demand.

**Future enhancement**: A project marker file (`tapir.toml`) could specify:

- Project root (for potential absolute path support)
- Entry point scripts
- Compiler options

## Implementation Plan

### Phase 1: Syntax and Parsing

Add `import` keyword and parse import declarations. Store in AST but don't process yet.

**Lexer changes:**

```rust
// Add to lexer.rs
#[token("import")]
Import,

#[token("..")]
DotDot,
```

**Grammar:**

```
import_decl = "import" import_path ";" ;
import_path = path_segment ("/" path_segment)* ;
path_segment = IDENT | DOTDOT ;
```

**AST:**

```rust
pub struct ImportDeclaration<'input> {
    pub path: ImportPath<'input>,
    pub span: Span,
}

pub struct ImportPath<'input> {
    pub segments: Vec<ImportPathSegment<'input>>,
    pub span: Span,
}

pub enum ImportPathSegment<'input> {
    Ident(&'input str),
    DotDot,
}
```

**Tests:**

- Parse `import foo;`
- Parse `import common/types;`
- Parse `import ../shared/utils;`
- Parse `import ../../deeply/nested;`
- Error on malformed imports (trailing slash, empty path, lone `.`)

### Phase 2: FileLoader Infrastructure

Add `FileLoader` trait and `TestFileLoader` for testing.

**Files:**

- `compiler/src/file_loader.rs` - trait and implementations

**Tests:**

- `FsFileLoader` loads real files
- `TestFileLoader` loads from in-memory map
- Canonicalization works correctly

### Phase 3: SourceStore and Multi-file Diagnostics

Add `SourceStore` to manage source strings across files.

**Changes:**

- `Diagnostics::add_file()` already exists, verify it works for this use case
- Add `SourceStore` struct
- Verify error messages show correct file paths

**Tests:**

- Error in imported file shows correct filename and line number
- Multiple files can have errors reported

### Phase 4: Import Resolution

Implement the core import resolution with merging at the symbol/type level.

**Approach (two-phase):**

1. Parse all files, each with its own AST lifetime
2. Run struct registration across all files into shared `StructRegistry`
3. Run symbol resolution across all files into shared `SymTab`
4. Type checking uses the shared registries

**Key functions:**

```rust
pub fn compile_with_loader(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
    file_loader: &dyn FileLoader,
) -> Result<CompileOutput, Diagnostics>
```

**Tests:**

- Import struct from another file
- Import function from another file
- Import property, global, extern fn, event handler
- Transitive imports (A imports B, B imports C, A can use C's definitions)
- Diamond imports - no conflict
- Circular import detection and error
- File not found error
- Duplicate definition error

### Phase 5: Prelude as Import

Refactor prelude to use the same import machinery. The prelude should be loaded **first**, before any user imports, so that:

1. User code can reference prelude functions
2. If a user file somehow imports the prelude explicitly, deduplication handles it
3. The prelude is consistently available in all files

**Order of operations:**

1. Load prelude into `SourceArena` (unless `--no-prelude`)
2. Parse prelude, add its definitions to the shared registries
3. Mark prelude as "loaded" for deduplication
4. Load and parse the main file
5. Resolve user imports (prelude already loaded, won't be duplicated)

**Changes:**

- Prelude is loaded via the file loader (special embedded path or bundled string)
- `--no-prelude` skips the implicit prelude import
- Prelude deduplicates like any other import

**Tests:**

- Prelude functions available by default
- `--no-prelude` disables prelude
- Prelude only loaded once even with multiple files
- User file importing prelude explicitly doesn't cause duplicates

### Phase 6: CLI Integration

Update `tapir-cli` to use `compile_with_loader` or `compile_file`.

**Tests:**

- CLI can compile files with imports
- Error messages show correct paths

## Decided Questions

1. **Methods on imported structs**: Methods are tied to the struct. Importing a file that defines a struct with methods makes those methods available.

2. **Prelude in imported files**: Each file gets the implicit prelude import. Since imports are deduplicated, the prelude is only loaded once.

## Possible Future Extensions

1. **Absolute paths / project root**: Support paths like `import /common/types;` relative to a project root determined by a marker file (`tapir.toml`). Not needed for initial implementation.

2. **Selective imports**: Support `import Point from common;` to import only specific items. Scripts aren't likely to get complicated enough to need this yet.

## Alternatives Considered

### Explicit item imports (JS/Python style)

```tapir
import Point, Rectangle from "common/types.tapir";
```

**Pros:** Clear what's imported, no namespace pollution
**Cons:** Verbose, need to update imports when adding items

We chose whole-file imports because they're simpler and better match the use case of shared property definitions across scripts.

### Text inclusion (C `#include`)

```tapir
include "common/types.tapir";
```

**Pros:** Dead simple
**Cons:** Diamond problem, requires include guards, order-dependent

### Qualified imports

```tapir
import "common/types" as types;
var p: types.Point;
```

**Pros:** No name collisions
**Cons:** Verbose usage, complex implementation

Could be added later if needed.

## Summary

```tapir
import common;           # imports ./common.tapir
import utils/math;       # imports ./utils/math.tapir
```

Design:

- Simple path-based syntax (no string quotes, no `.tapir` extension)
- Import all definitions from the file (structs, functions, properties, globals, extern fns, event handlers)
- Transitive: importing A gives you everything A imported
- Deduplication: same file imported multiple times is loaded once (handles diamond imports)
- Circular imports are errors
- Prelude is an implicit `import prelude;`

This supports the primary use case: multiple scripts sharing property definitions on the same game object.
