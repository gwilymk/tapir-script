use std::fmt::{self, Display};

use serde::Serialize;

use crate::tokens::Span;

/// Unique identifier for a struct type.
///
/// This is an index into the StructRegistry. Struct types are identified
/// by their ID rather than by name, enabling nominal typing (two structs
/// with identical fields are distinct types).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, PartialOrd, Ord)]
pub struct StructId(pub u32);

/// Definition of a struct type.
#[derive(Clone, Debug, Serialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}

/// A field within a struct definition.
#[derive(Clone, Debug, Serialize)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Registry of all struct definitions.
///
/// The registry owns all struct definitions and provides access by StructId.
/// Struct names are registered in a first pass (with empty fields), then
/// fields are resolved in a second pass once all struct names are known.
#[derive(Default, Debug, Serialize)]
pub struct StructRegistry {
    structs: Vec<StructDef>,
}

impl StructRegistry {
    /// Register a new struct definition and return its ID.
    pub fn register(&mut self, def: StructDef) -> StructId {
        let id = StructId(self.structs.len() as u32);
        self.structs.push(def);
        id
    }

    /// Get a struct definition by ID.
    pub fn get(&self, id: StructId) -> &StructDef {
        &self.structs[id.0 as usize]
    }

    /// Get a mutable reference to a struct definition by ID.
    pub fn get_mut(&mut self, id: StructId) -> &mut StructDef {
        &mut self.structs[id.0 as usize]
    }

    /// Iterate over all struct definitions.
    pub fn iter(&self) -> impl Iterator<Item = &StructDef> {
        self.structs.iter()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default, Serialize)]
pub enum Type {
    Int,
    Fix,
    Bool,
    Struct(StructId),
    Task,

    #[default]
    Error,
}

impl Type {
    /// Parse a builtin type name. Returns None for non-builtin names.
    ///
    /// Used by type resolution and struct registration (to prevent shadowing).
    pub fn parse_builtin(name: &str) -> Option<Type> {
        match name {
            "int" => Some(Type::Int),
            "fix" => Some(Type::Fix),
            "bool" => Some(Type::Bool),
            "task" => Some(Type::Task),
            _ => None,
        }
    }

    /// Returns whether this is a struct type.
    pub fn is_struct(self) -> bool {
        matches!(self, Type::Struct(_))
    }

    /// Returns the StructId if this is a struct type.
    pub fn as_struct(self) -> Option<StructId> {
        match self {
            Type::Struct(id) => Some(id),
            _ => None,
        }
    }

    /// Returns the type's name as a string.
    ///
    /// For struct types, looks up the name in the registry.
    /// For primitive types, returns the canonical name ("int", "fix", "bool").
    pub fn name(self, registry: &StructRegistry) -> &str {
        match self {
            Type::Int => "int",
            Type::Fix => "fix",
            Type::Bool => "bool",
            Type::Task => "task",
            Type::Struct(id) => &registry.get(id).name,
            Type::Error => "error",
        }
    }

}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Fix => write!(f, "fix"),
            Type::Bool => write!(f, "bool"),
            Type::Task => write!(f, "task"),
            Type::Struct(_) => write!(f, "struct"),
            Type::Error => write!(f, "error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::FileId;

    fn test_span() -> Span {
        Span::new(FileId::new(0), 0, 0)
    }

    #[test]
    fn struct_registry_register_and_get() {
        let mut registry = StructRegistry::default();
        let def = StructDef {
            name: "Point".to_string(),
            fields: vec![
                StructField {
                    name: "x".to_string(),
                    ty: Type::Int,
                    span: test_span(),
                },
                StructField {
                    name: "y".to_string(),
                    ty: Type::Int,
                    span: test_span(),
                },
            ],
            span: test_span(),
        };

        let id = registry.register(def);
        assert_eq!(id, StructId(0));

        let retrieved = registry.get(id);
        assert_eq!(retrieved.name, "Point");
        assert_eq!(retrieved.fields.len(), 2);
        assert_eq!(retrieved.fields[0].name, "x");
        assert_eq!(retrieved.fields[1].name, "y");
    }

    #[test]
    fn struct_registry_multiple_structs() {
        let mut registry = StructRegistry::default();

        let id1 = registry.register(StructDef {
            name: "First".to_string(),
            fields: vec![],
            span: test_span(),
        });
        let id2 = registry.register(StructDef {
            name: "Second".to_string(),
            fields: vec![],
            span: test_span(),
        });

        assert_eq!(id1, StructId(0));
        assert_eq!(id2, StructId(1));
        assert_eq!(registry.get(id1).name, "First");
        assert_eq!(registry.get(id2).name, "Second");
    }

    #[test]
    fn struct_registry_get_mut() {
        let mut registry = StructRegistry::default();
        let id = registry.register(StructDef {
            name: "Mutable".to_string(),
            fields: vec![],
            span: test_span(),
        });

        registry.get_mut(id).fields.push(StructField {
            name: "added".to_string(),
            ty: Type::Bool,
            span: test_span(),
        });

        assert_eq!(registry.get(id).fields.len(), 1);
        assert_eq!(registry.get(id).fields[0].name, "added");
    }

    #[test]
    fn type_is_struct() {
        assert!(Type::Struct(StructId(0)).is_struct());
        assert!(Type::Struct(StructId(42)).is_struct());

        assert!(!Type::Int.is_struct());
        assert!(!Type::Fix.is_struct());
        assert!(!Type::Bool.is_struct());
        assert!(!Type::Error.is_struct());
    }

    #[test]
    fn type_as_struct() {
        assert_eq!(Type::Struct(StructId(0)).as_struct(), Some(StructId(0)));
        assert_eq!(Type::Struct(StructId(5)).as_struct(), Some(StructId(5)));

        assert_eq!(Type::Int.as_struct(), None);
        assert_eq!(Type::Fix.as_struct(), None);
        assert_eq!(Type::Bool.as_struct(), None);
        assert_eq!(Type::Error.as_struct(), None);
    }

    #[test]
    fn type_parse_builtin() {
        assert_eq!(Type::parse_builtin("int"), Some(Type::Int));
        assert_eq!(Type::parse_builtin("fix"), Some(Type::Fix));
        assert_eq!(Type::parse_builtin("bool"), Some(Type::Bool));

        assert_eq!(Type::parse_builtin("Point"), None);
        assert_eq!(Type::parse_builtin("struct"), None);
        assert_eq!(Type::parse_builtin("unknown"), None);
        assert_eq!(Type::parse_builtin(""), None);
    }

    #[test]
    fn type_name_with_registry() {
        let mut registry = StructRegistry::default();
        let id = registry.register(StructDef {
            name: "Point".to_string(),
            fields: vec![],
            span: test_span(),
        });

        assert_eq!(Type::Int.name(&registry), "int");
        assert_eq!(Type::Fix.name(&registry), "fix");
        assert_eq!(Type::Bool.name(&registry), "bool");
        assert_eq!(Type::Error.name(&registry), "error");
        assert_eq!(Type::Struct(id).name(&registry), "Point");
    }

    #[test]
    fn type_display_without_registry() {
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::Fix.to_string(), "fix");
        assert_eq!(Type::Bool.to_string(), "bool");
        assert_eq!(Type::Error.to_string(), "error");
        assert_eq!(Type::Struct(StructId(0)).to_string(), "struct");
    }
}
