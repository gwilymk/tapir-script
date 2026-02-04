//! File loading abstraction for import resolution.
//!
//! This module provides traits and implementations for loading files during
//! compilation. The abstraction allows both real filesystem access and
//! in-memory test fixtures.
//!
//! Both loaders own their source strings and return `&str` references with
//! lifetimes tied to the loader, eliminating the need for separate source storage.

use std::path::{Path, PathBuf};

use elsa::FrozenMap;

/// Trait for loading files during import resolution.
///
/// Implementations own the source strings and return references with lifetimes
/// tied to `&self`. This means all ASTs can share the loader's lifetime.
pub trait FileLoader {
    /// Load file contents. Returns None if file doesn't exist.
    /// The returned `&str` has a lifetime tied to the loader.
    fn load(&self, path: &Path) -> Option<&str>;

    /// Canonicalise a path for deduplication.
    /// Returns None if path doesn't exist.
    fn canonicalise(&self, path: &Path) -> Option<PathBuf>;
}

/// Loads files from the real filesystem, with support for inserted content.
///
/// Inserted content (e.g., prelude) takes precedence over filesystem reads.
/// All loaded/inserted content is cached and returned as `&str` references.
#[derive(Default)]
pub struct FsFileLoader {
    /// Cache of loaded/inserted files. Keys are canonical paths for filesystem
    /// files, or the inserted path for fake files.
    files: FrozenMap<PathBuf, String>,
}

impl FsFileLoader {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert content at a path. This content will be returned by `load()`
    /// for this path instead of reading from the filesystem.
    ///
    /// Useful for embedding the prelude or other built-in content.
    pub fn insert(&self, path: impl Into<PathBuf>, content: impl Into<String>) -> &str {
        let path = path.into();
        self.files.insert(path.clone(), content.into());
        // Safe because we just inserted it
        self.files.get(&path).unwrap()
    }
}

impl FileLoader for FsFileLoader {
    fn load(&self, path: &Path) -> Option<&str> {
        // Check cache first (includes inserted content)
        if let Some(content) = self.files.get(path) {
            return Some(content);
        }

        // Try to load from filesystem
        let content = std::fs::read_to_string(path).ok()?;
        self.files.insert(path.to_path_buf(), content);

        // Return reference to cached content
        self.files.get(path)
    }

    fn canonicalise(&self, path: &Path) -> Option<PathBuf> {
        // For inserted paths, they're already canonical
        if self.files.get(path).is_some() {
            return Some(path.to_path_buf());
        }
        // For filesystem paths, use the OS canonicalisation
        path.canonicalize().ok()
    }
}

/// Test file loader with in-memory files.
///
/// Designed for easy test case definition:
/// ```ignore
/// let loader = TestFileLoader::new()
///     .with_file("common.tapir", "struct Point { x: int, y: int }")
///     .with_file("main.tapir", "import common; property pos: Point;");
///
/// let result = compile("main.tapir", loader.load("main.tapir").unwrap(), &settings, &loader);
/// ```
#[derive(Default)]
pub struct TestFileLoader {
    files: FrozenMap<PathBuf, String>,
}

impl TestFileLoader {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a file to the loader. Paths are relative to a virtual root.
    /// Returns self for chaining.
    pub fn with_file(self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.files.insert(path.into(), content.into());
        self
    }

    /// Insert content at a path. Same as `with_file` but takes `&self`.
    pub fn insert(&self, path: impl Into<PathBuf>, content: impl Into<String>) -> &str {
        let path = path.into();
        self.files.insert(path.clone(), content.into());
        self.files.get(&path).unwrap()
    }

    /// Get the content of a file (convenience for tests).
    /// Panics if the file doesn't exist.
    pub fn get(&self, path: impl AsRef<Path>) -> &str {
        self.files.get(path.as_ref()).expect("test file not found")
    }

    /// Get the content of a file, returning None if it doesn't exist.
    pub fn try_get(&self, path: impl AsRef<Path>) -> Option<&str> {
        self.files.get(path.as_ref())
    }
}

impl FileLoader for TestFileLoader {
    fn load(&self, path: &Path) -> Option<&str> {
        self.files.get(path)
    }

    fn canonicalise(&self, path: &Path) -> Option<PathBuf> {
        // Normalise the path (resolve .. segments)
        let normalised = normalise_path(path);
        if self.files.get(&normalised).is_some() {
            Some(normalised)
        } else {
            None
        }
    }
}

/// Normalise a path by resolving `.` and `..` segments.
/// Does not access the filesystem.
fn normalise_path(path: &Path) -> PathBuf {
    use std::path::Component;

    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                result.pop();
            }
            Component::Normal(s) => result.push(s),
            Component::CurDir => {}
            Component::RootDir => result.push("/"),
            Component::Prefix(p) => result.push(p.as_os_str()),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_loader_load() {
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "struct Point { x: int, y: int }")
            .with_file("main.tapir", "import common;");

        assert_eq!(
            loader.load(Path::new("common.tapir")),
            Some("struct Point { x: int, y: int }")
        );
        assert_eq!(loader.load(Path::new("main.tapir")), Some("import common;"));
        assert_eq!(loader.load(Path::new("nonexistent.tapir")), None);
    }

    #[test]
    fn test_file_loader_get() {
        let loader = TestFileLoader::new().with_file("test.tapir", "property x: int;");

        assert_eq!(loader.get("test.tapir"), "property x: int;");
    }

    #[test]
    fn test_file_loader_insert() {
        let loader = TestFileLoader::new();
        let content = loader.insert("prelude.tapir", "fn abs(x: int) -> int { return x; }");

        assert_eq!(content, "fn abs(x: int) -> int { return x; }");
        assert_eq!(
            loader.get("prelude.tapir"),
            "fn abs(x: int) -> int { return x; }"
        );
    }

    #[test]
    fn test_file_loader_canonicalise() {
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "")
            .with_file("utils/math.tapir", "");

        // Existing files
        assert_eq!(
            loader.canonicalise(Path::new("common.tapir")),
            Some(PathBuf::from("common.tapir"))
        );
        assert_eq!(
            loader.canonicalise(Path::new("utils/math.tapir")),
            Some(PathBuf::from("utils/math.tapir"))
        );

        // Non-existing file
        assert_eq!(loader.canonicalise(Path::new("nonexistent.tapir")), None);
    }

    #[test]
    fn test_file_loader_canonicalise_with_parent_dirs() {
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "")
            .with_file("utils/math.tapir", "");

        // Path with .. that resolves to existing file
        assert_eq!(
            loader.canonicalise(Path::new("utils/../common.tapir")),
            Some(PathBuf::from("common.tapir"))
        );

        // Nested path with ..
        assert_eq!(
            loader.canonicalise(Path::new("foo/../utils/math.tapir")),
            Some(PathBuf::from("utils/math.tapir"))
        );
    }

    #[test]
    fn test_normalise_path() {
        assert_eq!(normalise_path(Path::new("a/b/c")), PathBuf::from("a/b/c"));
        assert_eq!(normalise_path(Path::new("a/../b")), PathBuf::from("b"));
        assert_eq!(normalise_path(Path::new("a/./b")), PathBuf::from("a/b"));
        assert_eq!(
            normalise_path(Path::new("a/b/../c/d")),
            PathBuf::from("a/c/d")
        );
        assert_eq!(
            normalise_path(Path::new("../a/b")),
            PathBuf::from("a/b") // Leading .. gets dropped (no parent to go to)
        );
    }

    #[test]
    fn test_nested_directory_structure() {
        let loader = TestFileLoader::new()
            .with_file("common.tapir", "struct Point { x: int, y: int }")
            .with_file("character/common.tapir", "property health: int;")
            .with_file("character/movement.tapir", "import common;")
            .with_file("utils/math.tapir", "fn abs(x: int) -> int { return x; }");

        // Load files from different directories
        assert!(loader.load(Path::new("common.tapir")).is_some());
        assert!(loader.load(Path::new("character/common.tapir")).is_some());
        assert!(loader.load(Path::new("character/movement.tapir")).is_some());
        assert!(loader.load(Path::new("utils/math.tapir")).is_some());

        // Non-existing paths
        assert!(
            loader
                .load(Path::new("character/nonexistent.tapir"))
                .is_none()
        );
    }

    #[test]
    fn test_fs_file_loader_insert() {
        let loader = FsFileLoader::new();
        let content = loader.insert("@prelude", "builtin(0) fn abs(x: int) -> int;");

        assert_eq!(content, "builtin(0) fn abs(x: int) -> int;");
        assert_eq!(
            loader.load(Path::new("@prelude")),
            Some("builtin(0) fn abs(x: int) -> int;")
        );
    }

    #[test]
    fn test_fs_file_loader_canonicalise_inserted() {
        let loader = FsFileLoader::new();
        loader.insert("@prelude", "");

        // Inserted paths are already canonical
        assert_eq!(
            loader.canonicalise(Path::new("@prelude")),
            Some(PathBuf::from("@prelude"))
        );
    }
}
