mod recursive;

use std::collections::HashMap;

use crate::{
    ast::Script,
    reporting::{Diagnostics, ErrorKind},
    tokens::Span,
    types::{StructDef, StructField, StructId, StructRegistry, Type},
};

/// First pass: Register all struct names and allocate StructIds.
///
/// Fields are left empty - we just need the names to exist so structs
/// can reference each other (e.g., `struct Rect { origin: Point, ... }`).
///
/// Checks that struct names don't shadow builtin types or other structs.
pub fn register_structs<'input>(
    script: &Script<'input>,
    registry: &mut StructRegistry,
    diagnostics: &mut Diagnostics,
) -> HashMap<&'input str, StructId> {
    // Track both StructId and the name span for error reporting
    let mut struct_names: HashMap<&'input str, (StructId, Span)> = HashMap::new();

    for decl in &script.struct_declarations {
        let name = decl.name.ident;

        // Check for shadowing builtin types
        if Type::parse_builtin(name).is_some() {
            ErrorKind::StructShadowsBuiltinType {
                name: name.to_string(),
            }
            .at(decl.name.span)
            .emit(diagnostics);
            continue;
        }

        // Check for duplicate struct names
        if let Some(&(_, existing_name_span)) = struct_names.get(name) {
            ErrorKind::DuplicateStructName {
                name: name.to_string(),
            }
            .at(decl.name.span)
            .label(
                decl.name.span,
                crate::reporting::DiagnosticMessage::AlsoDeclaredHere,
            )
            .label(
                existing_name_span,
                crate::reporting::DiagnosticMessage::OriginallyDeclaredHere,
            )
            .emit(diagnostics);
            continue;
        }

        // Register the struct with empty fields (populated in second pass)
        let def = StructDef {
            name: name.to_string(),
            fields: vec![],
            span: decl.span,
        };
        let id = registry.register(def);
        struct_names.insert(name, (id, decl.name.span));
    }

    // Return just the StructIds for use by resolve_struct_fields
    struct_names
        .into_iter()
        .map(|(name, (id, _span))| (name, id))
        .collect()
}

/// Second pass: Now that all struct names are registered, resolve field types.
///
/// This converts AST type annotations (which may be struct names like "Point")
/// into actual Type values (Type::Struct(id)). Also detects duplicate field names
/// and recursive struct definitions.
pub fn resolve_struct_fields<'input>(
    script: &Script<'input>,
    registry: &mut StructRegistry,
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    for decl in &script.struct_declarations {
        let name = decl.name.ident;

        // Skip structs that failed registration (duplicates, shadowing)
        let Some(&struct_id) = struct_names.get(name) else {
            continue;
        };

        let mut fields = Vec::new();
        let mut field_names: HashMap<&str, usize> = HashMap::new();

        for (idx, field_decl) in decl.fields.iter().enumerate() {
            let field_name = field_decl.name();
            let field_span = field_decl.span();

            // Check for duplicate field names
            if let Some(&first_idx) = field_names.get(field_name) {
                let first_span = decl.fields[first_idx].span();
                ErrorKind::DuplicateStructField {
                    struct_name: name.to_string(),
                    field_name: field_name.to_string(),
                }
                .at(field_span)
                .label(
                    field_span,
                    crate::reporting::DiagnosticMessage::AlsoDeclaredHere,
                )
                .label(
                    first_span,
                    crate::reporting::DiagnosticMessage::OriginallyDeclaredHere,
                )
                .emit(diagnostics);
                continue;
            }
            field_names.insert(field_name, idx);

            // Resolve field type (if not already resolved)
            let type_with_loc = field_decl.ty_required();
            let field_type = if let Some(ty) = type_with_loc.t {
                ty
            } else {
                resolve_type_name(type_with_loc, struct_names, diagnostics)
            };

            fields.push(StructField {
                name: field_name.to_string(),
                ty: field_type,
                span: field_span,
            });
        }

        // Update the struct definition with resolved fields
        registry.get_mut(struct_id).fields = fields;
    }

    // Check for recursive struct definitions after all fields are resolved
    recursive::check_recursive_structs(registry, diagnostics);
}

/// Third pass: Resolve all type annotations throughout the script.
///
/// This resolves `None` types (which represent user-defined types)
/// to `Some(Type::Struct(id))` where appropriate, or emits errors for unknown types.
/// For methods, also resolves the receiver type and fills in self's type.
/// For operators, resolves operand types and validates constraints.
pub fn resolve_all_types<'input>(
    script: &mut crate::ast::Script<'input>,
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    // Resolve function argument and return types
    for function in &mut script.functions {
        // For operators, resolve operand types from the pattern
        if let Some(ref op_def) = function.operator_def {
            let left_type = resolve_receiver_type(
                op_def.left_type.ident,
                struct_names,
                op_def.left_type.span,
                diagnostics,
            );
            let right_type = resolve_receiver_type(
                op_def.right_type.ident,
                struct_names,
                op_def.right_type.span,
                diagnostics,
            );

            // At least one operand must be a struct
            if !left_type.is_struct() && !right_type.is_struct() {
                ErrorKind::OperatorRequiresStruct {
                    left: op_def.left_type.ident.to_string(),
                    right: op_def.right_type.ident.to_string(),
                }
                .at(function.span)
                .label(
                    op_def.left_type.span,
                    crate::reporting::DiagnosticMessage::HasType { ty: left_type },
                )
                .label(
                    op_def.right_type.span,
                    crate::reporting::DiagnosticMessage::HasType { ty: right_type },
                )
                .emit(diagnostics);
            }

            // Fill in argument types from the pattern
            if function.arguments.len() == 2 {
                function.arguments[0].ty = Some(crate::ast::TypeWithLocation {
                    t: Some(left_type),
                    name: op_def.left_type.ident,
                    span: function.arguments[0].span(),
                });
                function.arguments[1].ty = Some(crate::ast::TypeWithLocation {
                    t: Some(right_type),
                    name: op_def.right_type.ident,
                    span: function.arguments[1].span(),
                });
            }
        }
        // For methods, resolve receiver type and fill in self's type
        else if let Some(ref receiver) = function.receiver_type {
            let receiver_type =
                resolve_receiver_type(receiver.ident, struct_names, receiver.span, diagnostics);

            // Fill in self's type from receiver type (self never has explicit type)
            if let Some(self_param) = function.arguments.first_mut()
                && self_param.name() == "self"
            {
                // Create a TypeWithLocation for self with the resolved receiver type
                self_param.ty = Some(crate::ast::TypeWithLocation {
                    t: Some(receiver_type),
                    name: receiver.ident,
                    span: self_param.span(),
                });
            }
        }

        for arg in &mut function.arguments {
            if let Some(ref mut ty) = arg.ty {
                resolve_type_in_place(ty, struct_names, diagnostics);
            }
        }
        for ret_ty in &mut function.return_types.types {
            resolve_type_in_place(ret_ty, struct_names, diagnostics);
        }
    }

    // Resolve extern function argument and return types
    for function in &mut script.extern_functions {
        for arg in &mut function.arguments {
            if let Some(ref mut ty) = arg.ty {
                resolve_type_in_place(ty, struct_names, diagnostics);
            }
        }
        for ret_ty in &mut function.return_types.types {
            resolve_type_in_place(ret_ty, struct_names, diagnostics);
        }
    }

    // Resolve builtin function argument and return types
    for function in &mut script.builtin_functions {
        // For builtin methods, resolve receiver type and fill in self's type
        if let Some(ref receiver) = function.receiver_type {
            let receiver_type =
                resolve_receiver_type(receiver.ident, struct_names, receiver.span, diagnostics);

            // Fill in self's type from receiver type (self never has explicit type)
            if let Some(self_param) = function.arguments.first_mut()
                && self_param.name() == "self"
            {
                // Create a TypeWithLocation for self with the resolved receiver type
                self_param.ty = Some(crate::ast::TypeWithLocation {
                    t: Some(receiver_type),
                    name: receiver.ident,
                    span: self_param.span(),
                });
            }
        }

        for arg in &mut function.arguments {
            if let Some(ref mut ty) = arg.ty {
                resolve_type_in_place(ty, struct_names, diagnostics);
            }
        }
        for ret_ty in &mut function.return_type.types {
            resolve_type_in_place(ret_ty, struct_names, diagnostics);
        }
    }

    // Resolve global declaration types
    for global in &mut script.globals {
        if let Some(ref mut ty) = global.name.ty {
            resolve_type_in_place(ty, struct_names, diagnostics);
        }
    }

    // Resolve property declaration types
    for property in &mut script.property_declarations {
        if let Some(ref mut ty) = property.name.ty {
            resolve_type_in_place(ty, struct_names, diagnostics);
        }
    }

    // Resolve variable declaration types in function bodies
    for function in &mut script.functions {
        resolve_types_in_statements(&mut function.statements, struct_names, diagnostics);
    }
}

fn resolve_types_in_statements<'input>(
    statements: &mut [crate::ast::Statement<'input>],
    struct_names: &HashMap<&'input str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    use crate::ast::StatementKind;

    for statement in statements {
        match &mut statement.kind {
            StatementKind::VariableDeclaration { idents, .. } => {
                for ident in idents {
                    if let Some(ref mut ty) = ident.ty {
                        resolve_type_in_place(ty, struct_names, diagnostics);
                    }
                }
            }
            StatementKind::Block { block } => {
                resolve_types_in_statements(block, struct_names, diagnostics);
            }
            StatementKind::If {
                true_block,
                false_block,
                ..
            } => {
                resolve_types_in_statements(true_block, struct_names, diagnostics);
                resolve_types_in_statements(false_block, struct_names, diagnostics);
            }
            StatementKind::Loop { block, .. } => {
                resolve_types_in_statements(block, struct_names, diagnostics);
            }
            _ => {}
        }
    }
}

/// Resolve a type annotation in place.
///
/// If the type is `None` (unresolved), tries to resolve it as a struct name.
/// Updates the TypeWithLocation.t field in place.
fn resolve_type_in_place(
    type_with_loc: &mut crate::ast::TypeWithLocation,
    struct_names: &HashMap<&str, StructId>,
    diagnostics: &mut Diagnostics,
) {
    if type_with_loc.t.is_none() {
        type_with_loc.t = Some(resolve_type_name(type_with_loc, struct_names, diagnostics));
    }
}

/// Resolve a receiver type name to a Type value.
///
/// This handles both builtin types (int, fix, bool) and struct types.
/// For unknown names, emits an error and returns Type::Error.
fn resolve_receiver_type(
    name: &str,
    struct_names: &HashMap<&str, StructId>,
    span: Span,
    diagnostics: &mut Diagnostics,
) -> Type {
    // First try builtin types
    if let Some(ty) = Type::parse_builtin(name) {
        return ty;
    }

    // Then try struct names
    if let Some(&struct_id) = struct_names.get(name) {
        return Type::Struct(struct_id);
    }

    // Unknown type name - emit error
    ErrorKind::UnknownMethodType {
        name: name.to_string(),
    }
    .at(span)
    .emit(diagnostics);
    Type::Error
}

/// Resolve a type annotation to a Type value.
///
/// Looks up the name as a struct and returns Type::Struct(id).
/// For unknown names, emits an error and returns Type::Error.
fn resolve_type_name(
    type_with_loc: &crate::ast::TypeWithLocation,
    struct_names: &HashMap<&str, StructId>,
    diagnostics: &mut Diagnostics,
) -> Type {
    // Try to resolve as a struct name
    if let Some(&struct_id) = struct_names.get(type_with_loc.name) {
        Type::Struct(struct_id)
    } else {
        // Unknown type name - emit error
        ErrorKind::UnknownTypeToken {
            token: type_with_loc.name.to_string(),
        }
        .at(type_with_loc.span)
        .label(
            type_with_loc.span,
            crate::reporting::DiagnosticMessage::UnknownTypeLabel2,
        )
        .emit(diagnostics);
        Type::Error
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Ident, StructDeclaration, TypeWithLocation, TypedIdent};
    use crate::tokens::{FileId, Span};

    fn test_span() -> Span {
        Span::new(FileId::new(0), 0, 0)
    }

    fn make_script_with_structs(decls: Vec<StructDeclaration<'static>>) -> Script<'static> {
        Script {
            struct_declarations: decls,
            property_declarations: vec![],
            globals: vec![],
            functions: vec![],
            extern_functions: vec![],
            builtin_functions: vec![],
        }
    }

    fn type_name(ty: Type) -> &'static str {
        match ty {
            Type::Int => "int",
            Type::Fix => "fix",
            Type::Bool => "bool",
            Type::Struct(_) => "struct",
            Type::Error => "error",
        }
    }

    fn make_struct_decl<'a>(name: &'a str, fields: Vec<(&'a str, Type)>) -> StructDeclaration<'a> {
        StructDeclaration {
            name: Ident {
                ident: name,
                span: test_span(),
            },
            fields: fields
                .into_iter()
                .map(|(field_name, ty)| TypedIdent {
                    ident: Ident {
                        ident: field_name,
                        span: test_span(),
                    },
                    ty: Some(TypeWithLocation {
                        t: Some(ty),
                        name: type_name(ty),
                        span: test_span(),
                    }),
                })
                .collect(),
            span: test_span(),
        }
    }

    #[test]
    fn register_single_struct() {
        let script = make_script_with_structs(vec![make_struct_decl(
            "Point",
            vec![("x", Type::Int), ("y", Type::Int)],
        )]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);

        assert!(!diagnostics.has_errors());
        assert_eq!(names.len(), 1);
        assert!(names.contains_key("Point"));
        assert_eq!(registry.get(names["Point"]).name, "Point");
    }

    #[test]
    fn register_multiple_structs() {
        let script = make_script_with_structs(vec![
            make_struct_decl("Point", vec![("x", Type::Int), ("y", Type::Int)]),
            make_struct_decl("Size", vec![("w", Type::Int), ("h", Type::Int)]),
        ]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);

        assert!(!diagnostics.has_errors());
        assert_eq!(names.len(), 2);
        assert!(names.contains_key("Point"));
        assert!(names.contains_key("Size"));
    }

    #[test]
    fn duplicate_struct_name_error() {
        let script = make_script_with_structs(vec![
            make_struct_decl("Point", vec![("x", Type::Int)]),
            make_struct_decl("Point", vec![("y", Type::Int)]),
        ]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);

        assert!(diagnostics.has_errors());
        // Only one struct should be registered
        assert_eq!(names.len(), 1);
    }

    #[test]
    fn struct_shadows_builtin_error() {
        let script =
            make_script_with_structs(vec![make_struct_decl("int", vec![("x", Type::Int)])]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);

        assert!(diagnostics.has_errors());
        assert!(names.is_empty());
    }

    #[test]
    fn resolve_fields_basic() {
        let script = make_script_with_structs(vec![make_struct_decl(
            "Point",
            vec![("x", Type::Int), ("y", Type::Fix)],
        )]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);
        resolve_struct_fields(&script, &mut registry, &names, &mut diagnostics);

        assert!(!diagnostics.has_errors());
        let struct_def = registry.get(names["Point"]);
        assert_eq!(struct_def.fields.len(), 2);
        assert_eq!(struct_def.fields[0].name, "x");
        assert_eq!(struct_def.fields[0].ty, Type::Int);
        assert_eq!(struct_def.fields[1].name, "y");
        assert_eq!(struct_def.fields[1].ty, Type::Fix);
    }

    #[test]
    fn duplicate_field_name_error() {
        let script = make_script_with_structs(vec![make_struct_decl(
            "Bad",
            vec![("x", Type::Int), ("x", Type::Int)],
        )]);
        let mut registry = StructRegistry::default();
        let mut diagnostics = Diagnostics::new(FileId::new(0), "test.tapir", "");

        let names = register_structs(&script, &mut registry, &mut diagnostics);
        resolve_struct_fields(&script, &mut registry, &names, &mut diagnostics);

        assert!(diagnostics.has_errors());
        // Only first field should be registered
        let struct_def = registry.get(names["Bad"]);
        assert_eq!(struct_def.fields.len(), 1);
    }

    use crate::{grammar, lexer::Lexer};
    use insta::{assert_ron_snapshot, assert_snapshot, glob};
    use std::fs;

    #[test]
    fn struct_visitor_success_snapshot_tests() {
        glob!("snapshot_tests", "struct_visitor/*_success.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let file_id = FileId::new(0);
            let lexer = Lexer::new(&input, file_id);
            let parser = grammar::ScriptParser::new();

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let mut registry = StructRegistry::default();
            let names = register_structs(&script, &mut registry, &mut diagnostics);
            resolve_struct_fields(&script, &mut registry, &names, &mut diagnostics);
            resolve_all_types(&mut script, &names, &mut diagnostics);

            assert!(
                !diagnostics.has_errors(),
                "Expected no errors but got: {}",
                diagnostics.pretty_string(false)
            );

            assert_ron_snapshot!(registry, {
                ".**.span" => "[span]",
            });
        });
    }

    #[test]
    fn struct_visitor_fail_snapshot_tests() {
        glob!("snapshot_tests", "struct_visitor/*_fail.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let file_id = FileId::new(0);
            let lexer = Lexer::new(&input, file_id);
            let parser = grammar::ScriptParser::new();

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser
                .parse(FileId::new(0), &mut diagnostics, lexer)
                .unwrap();

            let mut registry = StructRegistry::default();
            let names = register_structs(&script, &mut registry, &mut diagnostics);
            resolve_struct_fields(&script, &mut registry, &names, &mut diagnostics);
            resolve_all_types(&mut script, &names, &mut diagnostics);

            assert_snapshot!(diagnostics.pretty_string(false));
        });
    }
}
