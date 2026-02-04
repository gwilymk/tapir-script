//! Import resolution for multi-file compilation.
//!
//! Resolves import declarations by loading files through a FileLoader,
//! handling deduplication, circular import detection, and AST merging.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::ast::{ImportDeclaration, ImportPathSegment, Script};
use crate::file_loader::FileLoader;
use crate::grammar;
use crate::lexer::Lexer;
use crate::reporting::{DiagnosticMessage, Diagnostics, ErrorKind};
use crate::tokens::{FileId, Span};

/// Resolves imports for a parsed AST, loading and merging imported files.
///
/// The resolver uses the FileLoader to load files and tracks which files
/// have been processed to handle deduplication and detect circular imports.
///
/// All source strings come from the FileLoader, so all ASTs share the
/// same `'src` lifetime.
pub struct ImportResolver<'src> {
    loader: &'src dyn FileLoader,
    /// Canonical paths of files that have been fully processed.
    loaded_files: HashSet<PathBuf>,
    /// Stack of files currently being processed (for cycle detection).
    import_stack: Vec<PathBuf>,
    /// Next file ID to assign.
    next_file_id: usize,
}

impl<'src> ImportResolver<'src> {
    pub fn new(loader: &'src dyn FileLoader, starting_file_id: usize) -> Self {
        Self {
            loader,
            loaded_files: HashSet::new(),
            import_stack: Vec::new(),
            next_file_id: starting_file_id,
        }
    }

    /// Allocate a new FileId.
    fn alloc_file_id(&mut self) -> FileId {
        let id = FileId::new(self.next_file_id);
        self.next_file_id += 1;
        id
    }

    /// Resolve imports for a script, recursively loading and merging imported files.
    ///
    /// The `ast` must have been parsed from a source string obtained from the same
    /// FileLoader, ensuring all ASTs share the `'src` lifetime.
    ///
    /// Errors are accumulated in `diagnostics`. Check `diagnostics.has_errors()`
    /// after calling to determine if resolution succeeded.
    pub fn resolve_imports(
        &mut self,
        ast: &mut Script<'src>,
        file_path: &Path,
        diagnostics: &mut Diagnostics,
    ) {
        let canonical = match self.loader.canonicalise(file_path) {
            Some(c) => c,
            None => {
                // File doesn't exist - this shouldn't happen for the main file
                // but could happen if called incorrectly
                return;
            }
        };

        // Check for circular import
        if self.import_stack.contains(&canonical) {
            // Build the cycle description
            let cycle: Vec<_> = self
                .import_stack
                .iter()
                .skip_while(|p| *p != &canonical)
                .map(|p| p.display().to_string())
                .collect();
            let cycle_str = format!("{} -> {}", cycle.join(" -> "), canonical.display());

            // We can't easily get the span here, so use a generic error
            // The actual span would be from the import that caused this
            ErrorKind::CircularImport {
                cycle: cycle_str.clone(),
            }
            .at(
                Span::new(FileId::new(0), 0, 0), // Placeholder span
                DiagnosticMessage::CircularImport { cycle: cycle_str },
            )
            .emit(diagnostics);
            return;
        }

        // Already fully processed? Nothing to do.
        if self.loaded_files.contains(&canonical) {
            return;
        }

        // Push onto import stack for cycle detection
        self.import_stack.push(canonical.clone());

        let file_dir = file_path.parent().unwrap_or(Path::new(""));

        // Process all imports (collect first to avoid borrow issues)
        let imports: Vec<_> = ast.imports.drain(..).collect();

        for import in imports {
            self.process_import(&import, file_dir, ast, diagnostics);
        }

        // Pop from import stack and mark as loaded
        self.import_stack.pop();
        self.loaded_files.insert(canonical);
    }

    /// Process a single import declaration.
    fn process_import(
        &mut self,
        import: &ImportDeclaration<'_>,
        file_dir: &Path,
        target_ast: &mut Script<'src>,
        diagnostics: &mut Diagnostics,
    ) {
        // Convert import path to filesystem path
        let import_path = self.resolve_import_path(file_dir, import);

        // Check if already loaded (deduplication)
        if let Some(canonical) = self.loader.canonicalise(&import_path)
            && self.loaded_files.contains(&canonical)
        {
            return; // Already loaded, skip
        }

        // Load the file - source string has 'src lifetime from the loader
        let source: &'src str = match self.loader.load(&import_path) {
            Some(s) => s,
            None => {
                ErrorKind::ImportFileNotFound {
                    path: import_path.display().to_string(),
                }
                .at(import.span, DiagnosticMessage::ImportFileNotFound)
                .emit(diagnostics);
                return;
            }
        };

        // Allocate file ID and register with diagnostics
        let file_id = self.alloc_file_id();
        diagnostics.add_file(file_id, &import_path, source);

        // Parse the imported file - AST has 'src lifetime
        let parser = grammar::ScriptParser::new();
        let mut lexer = Lexer::new(source, file_id);

        let mut imported_ast: Script<'src> = match parser.parse(file_id, diagnostics, lexer.iter())
        {
            Ok(ast) => ast,
            Err(e) => {
                diagnostics.add_lalrpop(e, file_id);
                return;
            }
        };

        // Recursively resolve imports in the imported file
        self.resolve_imports(&mut imported_ast, &import_path, diagnostics);

        // Merge imported definitions into target AST
        self.merge_definitions(target_ast, imported_ast, import.span, file_id, diagnostics);
    }

    /// Convert an import path to a filesystem path.
    fn resolve_import_path(&self, file_dir: &Path, import: &ImportDeclaration<'_>) -> PathBuf {
        let mut path = file_dir.to_path_buf();

        for segment in &import.path.segments {
            match segment {
                ImportPathSegment::Ident(name) => path.push(name),
                ImportPathSegment::DotDot => {
                    path.pop();
                }
            }
        }

        // Add .tapir extension
        path.set_extension("tapir");
        path
    }

    /// Merge definitions from source into target, checking for duplicates.
    fn merge_definitions(
        &self,
        target: &mut Script<'src>,
        source: Script<'src>,
        import_span: Span,
        file_id: FileId,
        diagnostics: &mut Diagnostics,
    ) {
        // Check for duplicate structs
        for struct_def in source.struct_declarations {
            let name = struct_def.name.name;
            if target
                .struct_declarations
                .iter()
                .any(|s| s.name.name == name)
            {
                ErrorKind::DuplicateImportedDefinition {
                    name: name.to_string(),
                    kind: "struct".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: name.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.struct_declarations.push(struct_def);
            }
        }

        // Check for duplicate functions (excluding @toplevel)
        for function in source.functions {
            if function.name == "@toplevel" {
                // Imported files should not have top-level statements
                if !function.statements.is_empty() {
                    ErrorKind::ToplevelStatementsInImport
                        .at(
                            Span::new(file_id, 0, 0),
                            DiagnosticMessage::ToplevelStatementsInImport,
                        )
                        .emit(diagnostics);
                }
                continue;
            }
            let mangled = function.mangled_name();
            if target
                .functions
                .iter()
                .any(|f| f.name != "@toplevel" && f.mangled_name() == mangled)
            {
                ErrorKind::DuplicateImportedDefinition {
                    name: mangled.to_string(),
                    kind: "function".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: mangled.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.functions.push(function);
            }
        }

        // Check for duplicate properties
        for prop in source.property_declarations {
            let name = prop.name.name();
            if target
                .property_declarations
                .iter()
                .any(|p| p.name.name() == name)
            {
                ErrorKind::DuplicateImportedDefinition {
                    name: name.to_string(),
                    kind: "property".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: name.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.property_declarations.push(prop);
            }
        }

        // Check for duplicate globals
        for global in source.globals {
            let name = global.name.name();
            if target.globals.iter().any(|g| g.name.name() == name) {
                ErrorKind::DuplicateImportedDefinition {
                    name: name.to_string(),
                    kind: "global".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: name.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.globals.push(global);
            }
        }

        // Check for duplicate extern functions
        for extern_fn in source.extern_functions {
            let name = extern_fn.name;
            if target.extern_functions.iter().any(|e| e.name == name) {
                ErrorKind::DuplicateImportedDefinition {
                    name: name.to_string(),
                    kind: "extern function".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: name.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.extern_functions.push(extern_fn);
            }
        }

        // Check for duplicate builtin functions
        for builtin in source.builtin_functions {
            let mangled = builtin.mangled_name();
            if target
                .builtin_functions
                .iter()
                .any(|b| b.mangled_name() == mangled)
            {
                ErrorKind::DuplicateImportedDefinition {
                    name: mangled.to_string(),
                    kind: "builtin function".to_string(),
                }
                .at(
                    import_span,
                    DiagnosticMessage::DuplicateImportedDefinition {
                        name: mangled.to_string(),
                    },
                )
                .emit(diagnostics);
            } else {
                target.builtin_functions.push(builtin);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_loader::TestFileLoader;

    fn parse_script<'a>(
        source: &'a str,
        file_id: FileId,
        diagnostics: &mut Diagnostics,
    ) -> Option<Script<'a>> {
        let parser = grammar::ScriptParser::new();
        let mut lexer = Lexer::new(source, file_id);
        match parser.parse(file_id, diagnostics, lexer.iter()) {
            Ok(ast) => Some(ast),
            Err(e) => {
                diagnostics.add_lalrpop(e, file_id);
                None
            }
        }
    }

    #[test]
    fn test_simple_import() {
        let loader = TestFileLoader::new()
            .with_file("types.tapir", "struct Point { x: int, y: int }")
            .with_file("main.tapir", "import types; property pos: Point;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();
        assert_eq!(ast.imports.len(), 1);
        assert_eq!(ast.struct_declarations.len(), 0);

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(
            !diagnostics.has_errors(),
            "Import resolution failed: {:?}",
            diagnostics
        );
        assert_eq!(ast.imports.len(), 0); // Imports consumed
        assert_eq!(ast.struct_declarations.len(), 1); // Point imported
        assert_eq!(ast.struct_declarations[0].name.name, "Point");
    }

    #[test]
    fn test_nested_import() {
        let loader = TestFileLoader::new()
            .with_file(
                "utils/math.tapir",
                "fn add(a: int, b: int) -> int { return a + b; }",
            )
            .with_file("main.tapir", "import utils/math;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(
            !diagnostics.has_errors(),
            "Import resolution failed: {:?}",
            diagnostics
        );
        // Should have @toplevel + add function
        assert!(ast.functions.iter().any(|f| f.name == "add"));
    }

    #[test]
    fn test_parent_dir_import() {
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "struct Point { x: int, y: int }")
            .with_file("sub/main.tapir", "import ../common;");

        let main_source = loader.get("sub/main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "sub/main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("sub/main.tapir"), &mut diagnostics);

        assert!(
            !diagnostics.has_errors(),
            "Import resolution failed: {:?}",
            diagnostics
        );
        assert_eq!(ast.struct_declarations.len(), 1);
    }

    #[test]
    fn test_file_not_found() {
        let loader = TestFileLoader::new().with_file("main.tapir", "import nonexistent;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(diagnostics.has_errors());
    }

    #[test]
    fn test_circular_import() {
        let loader = TestFileLoader::new()
            .with_file("a.tapir", "import b;")
            .with_file("b.tapir", "import a;");

        let main_source = loader.get("a.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "a.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("a.tapir"), &mut diagnostics);

        assert!(diagnostics.has_errors());
    }

    #[test]
    fn test_diamond_import() {
        // Diamond: main imports a and b, both import common
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "struct Point { x: int, y: int }")
            .with_file("a.tapir", "import common;")
            .with_file("b.tapir", "import common;")
            .with_file("main.tapir", "import a; import b;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        // Should succeed - common is only loaded once via deduplication
        assert!(
            !diagnostics.has_errors(),
            "Diamond import failed: {:?}",
            diagnostics
        );
        // Should have exactly one Point struct
        assert_eq!(ast.struct_declarations.len(), 1);
    }

    #[test]
    fn test_transitive_import() {
        // main imports a, a imports common, main should get common's definitions
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "struct Point { x: int, y: int }")
            .with_file(
                "a.tapir",
                "import common; fn make_point() -> Point { return Point(0, 0); }",
            )
            .with_file("main.tapir", "import a;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(
            !diagnostics.has_errors(),
            "Transitive import failed: {:?}",
            diagnostics
        );
        // Should have Point struct (from common via a)
        assert_eq!(ast.struct_declarations.len(), 1);
        // Should have make_point function (from a)
        assert!(ast.functions.iter().any(|f| f.name == "make_point"));
    }

    #[test]
    fn test_duplicate_definition_error() {
        // Both a and b define Point locally (not via shared import)
        let loader = TestFileLoader::new()
            .with_file("a.tapir", "struct Point { x: int, y: int }")
            .with_file("b.tapir", "struct Point { a: int, b: int }")
            .with_file("main.tapir", "import a; import b;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(diagnostics.has_errors());
    }

    #[test]
    fn test_toplevel_statements_in_import_error() {
        // Imported file has top-level statements which should produce an error
        let loader = TestFileLoader::new()
            .with_file("lib.tapir", "let x: int = 42;") // Has a top-level statement
            .with_file("main.tapir", "import lib;");

        let main_source = loader.get("main.tapir");
        let file_id = FileId::new(0);
        let mut diagnostics = Diagnostics::new(file_id, "main.tapir", main_source);

        let mut ast = parse_script(main_source, file_id, &mut diagnostics).unwrap();

        let mut resolver = ImportResolver::new(&loader, 1);
        resolver.resolve_imports(&mut ast, Path::new("main.tapir"), &mut diagnostics);

        assert!(diagnostics.has_errors());
    }
}
