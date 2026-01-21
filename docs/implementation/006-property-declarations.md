# Property Declarations

## Overview

This document outlines the implementation plan for explicit property declarations in tapir-script. Property declarations make the .tapir file the source of truth for which properties exist and their types, enabling better tooling support (LSP, linting) and clearer understanding of scripts without needing to examine the Rust code.

## Motivation

Currently, properties are defined implicitly by the Rust struct with `#[derive(TapirScript)]`:

```rust
#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    #[tapir(int)]
    health: i32,
    #[tapir(fix)]
    position: Fix,
}
```

```tapir
# In script.tapir - no indication that health or position exist
health = health - 10;
position = position + 1.0;
```

This creates several problems:

1. **LSP/tooling**: A language server analyzing `script.tapir` cannot know what properties exist or their types without parsing Rust code
2. **Readability**: Developers reading a .tapir file must consult the Rust struct to understand available properties
3. **Discoverability**: No autocomplete or type hints for properties in editors

With explicit property declarations, the .tapir file becomes self-documenting:

```tapir
property health: int;
property position: fix;

health = health - 10;
position = position + 1.0;
```

## Design Decisions

### Syntax

Property declarations use the `property` keyword at the top level:

```tapir
property health: int;
property position: fix;
property alive: bool;
```

Like globals, property declarations can appear anywhere at the top level and are semantically "hoisted" - they're all collected before any code runs.

### .tapir File as Source of Truth

The .tapir file becomes authoritative for:

- Which properties the script can access
- The type of each property

The Rust struct must match these declarations. This inverts the current relationship where Rust defines properties and the compiler receives them via `CompileSettings`.

### Rust Struct Field Matching

All struct fields are potential properties, matched by name. The `#[tapir(int)]`, `#[tapir(fix)]`, `#[tapir(bool)]` attributes are no longer needed.

```rust
// Before
#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    #[tapir(int)]
    health: i32,
    #[tapir(fix)]
    position: Fix,
}

// After
#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    health: i32,
    position: Fix,
}
```

### Validation Rules

1. **Property declared in .tapir but not in Rust struct**: Compile error

   ```tapir
   property nonexistent: int;  # ERROR: no field 'nonexistent' in struct
   ```

2. **Property in Rust struct but not declared in .tapir**: Not an error - the field simply isn't accessible to the script. This allows Rust structs to have internal fields.

3. **Type mismatch between .tapir declaration and Rust field**: The generated Rust code will fail to compile with a type error when trying to set the property value. This may result in somewhat confusing error messages pointing to the generated code rather than the source of the mismatch.

4. **Declaration order**: Does not need to match Rust field order. Properties can be declared in any order in .tapir.

### Type Compatibility Table

| .tapir type | Compatible Rust types |
| ----------- | --------------------- |
| `int`       | `i32`                 |
| `fix`       | `Fix`, `Num<i32, 8>`  |
| `bool`      | `bool`                |

### Property Indices

Property indices are determined by declaration order in the .tapir file, not by Rust field order. This means:

```tapir
property health: int;    # index 0
property position: fix;  # index 1
```

The macro must generate getter/setter code that maps these indices to the corresponding Rust fields.

## Current State

### What Exists

- **Properties via CompileSettings**: The macro extracts properties from Rust struct attributes and passes them to the compiler
- **Property access in scripts**: Properties are accessed by name, resolved via symbol table
- **Property bytecode**: `GetProp`/`SetProp` instructions use property indices
- **Global declarations**: Similar syntax at top level (`global foo = 3;`)
- **Type system**: Three types - `int`, `fix`, `bool`

### What's Missing

- `property` keyword in lexer
- Grammar rule for property declarations
- AST node for property declarations
- Script AST storing property declarations
- Compiler extracting properties from parsed AST instead of CompileSettings
- Macro parsing .tapir file to extract property declarations
- Macro validating property declarations against Rust struct fields
- Macro inferring property types from declarations (not from `#[tapir(...)]`)
- Error messages for property mismatches

## Implementation Plan

### Phase 1: Grammar & AST

**Files**: `compiler/src/tokens.rs`, `compiler/src/grammar.lalrpop`, `compiler/src/ast.rs`

1. Add `property` keyword to the lexer:

   ```rust
   // tokens.rs
   #[derive(Logos, ...)]
   pub enum Token<'input> {
       // ... existing tokens ...
       #[token("property")]
       KeywordProperty,
   }
   ```

2. Add `PropertyDeclaration` AST node:

   ```rust
   // ast.rs
   #[derive(Clone, Debug, Serialize)]
   pub struct PropertyDeclaration<'input> {
       pub name: Ident<'input>,
       pub ty: TypeWithLocation,
       pub span: Span,
   }
   ```

3. Extend `Script` AST to include property declarations:

   ```rust
   pub struct Script<'input> {
       pub property_declarations: Vec<PropertyDeclaration<'input>>,
       pub globals: Vec<GlobalDeclaration<'input>>,
       pub functions: Vec<Function<'input>>,
       pub extern_functions: Vec<ExternFunctionDefinition<'input>>,
   }
   ```

4. Add grammar rule for property declarations:

   ```lalrpop
   // grammar.lalrpop

   extern {
       enum Token<'input> {
           // ... existing tokens ...
           property => Token::KeywordProperty,
       }
   }

   PropertyDecl: PropertyDeclaration<'input> = {
       <start:@L> property <name:Identifier> ":" <ty:Type> ";" <end:@R> =>
           PropertyDeclaration { name, ty, span: Span::new(file_id, start, end) },
   }

   TopLevelStatement: TopLevelStatement<'input> = {
       // ... existing variants ...
       <PropertyDecl> => TopLevelStatement::PropertyDeclaration(<>),
   }
   ```

5. Update `TopLevelStatement` enum:

   ```rust
   pub enum TopLevelStatement<'input> {
       Statement(Statement<'input>),
       FunctionDefinition(Function<'input>),
       ExternFunctionDefinition(ExternFunctionDefinition<'input>),
       GlobalDeclaration(GlobalDeclaration<'input>),
       PropertyDeclaration(PropertyDeclaration<'input>),  // NEW
       Error,
   }
   ```

6. Update `Script::from_top_level` to collect property declarations.

### Phase 2: Compiler Property Extraction

**Files**: `compiler/src/compile.rs`, `compiler/src/compile/symtab_visitor.rs`

The compiler extracts properties from the parsed AST and validates them against the available fields provided by the macro.

1. Modify `CompileSettings` to accept optional field names for validation:

   ```rust
   pub struct CompileSettings {
       /// Field names available in the Rust struct. If `Some`, the compiler validates
       /// that declared properties exist in this list. If `None`, validation is skipped
       /// (useful for LSP/tooling where the Rust struct info isn't available).
       pub available_fields: Option<Vec<String>>,
       pub enable_optimisations: bool,
   }
   ```

2. Create a function to extract and optionally validate properties from the AST:

   ```rust
   // In compile.rs or a new module
   fn extract_properties_from_ast(
       declarations: &[PropertyDeclaration],
       available_fields: Option<&[String]>,
       diagnostics: &mut Diagnostics,
   ) -> Vec<Property> {
       declarations
           .iter()
           .enumerate()
           .filter_map(|(index, decl)| {
               // Skip properties with parse errors
               if decl.ty.t == Type::Error {
                   return None;
               }

               let name = decl.name.ident.to_string();

               // Validate property exists in available fields (if provided)
               // Report error but continue - include the property anyway to find more errors
               if let Some(fields) = available_fields {
                   if !fields.contains(&name) {
                       diagnostics.add_message(
                           CompilerErrorKind::PropertyNotInStruct {
                               name: name.clone(),
                           }
                           .into_message(decl.name.span),
                       );
                   }
               }

               Some(Property {
                   ty: decl.ty.t,
                   index,
                   name,
               })
           })
           .collect()
   }
   ```

3. Update the compile function:

   ```rust
   pub fn compile(
       filename: impl AsRef<Path>,
       input: &str,
       settings: &CompileSettings,
   ) -> Result<Bytecode, Diagnostics> {
       // ... parse AST ...

       // Extract properties from AST, validating against available fields if provided
       let properties = extract_properties_from_ast(
           &ast.property_declarations,
           settings.available_fields.as_deref(),
           &mut diagnostics,
       );

       // ... continue compilation with extracted properties ...
   }
   ```

4. Add validation in `symtab_visitor.rs`:
   - Check for duplicate property names
   - Check for conflicts with globals and built-ins

   ```rust
   // In SymTabVisitor::new or a new validation function
   for (i, decl) in ast.property_declarations.iter().enumerate() {
       // Check for duplicate property names
       if ast.property_declarations[..i]
           .iter()
           .any(|d| d.name.ident == decl.name.ident)
       {
           diagnostics.add_message(
               CompilerErrorKind::DuplicatePropertyDeclaration {
                   name: decl.name.ident.to_string(),
               }
               .into_message(decl.name.span),
           );
       }

       // Check for conflicts with globals (existing check can be reused/inverted)
       if ast.globals.iter().any(|g| g.name.ident == decl.name.ident) {
           diagnostics.add_message(
               CompilerErrorKind::PropertyConflictsWithGlobal {
                   name: decl.name.ident.to_string(),
               }
               .into_message(decl.name.span),
           );
       }

       // Check for conflicts with built-ins
       if BuiltinVariable::from_name(decl.name.ident).is_some() {
           diagnostics.add_message(
               CompilerErrorKind::CannotShadowBuiltin {
                   name: decl.name.ident.to_string(),
               }
               .into_message(decl.name.span),
           );
       }
   }
   ```

### Phase 3: Compile Result Extension

**Files**: `compiler/src/lib.rs`

Add property declarations to the compile result so the macro can access them:

```rust
pub struct CompileResult {
    pub bytecode: Box<[u32]>,
    pub globals: Box<[i32]>,
    pub event_handlers: Box<[EventHandler]>,
    pub triggers: Box<[Trigger]>,
    pub extern_functions: Box<[ExternFunction]>,
    pub properties: Box<[PropertyInfo]>,  // NEW
}

/// Information about a declared property, for use by the macro.
pub struct PropertyInfo {
    pub name: String,
    pub ty: Type,
    pub index: usize,
}
```

### Phase 4: Macro Updates

**Files**: `tapir-script-macros-core/src/lib.rs`

The macro changes are minimal since validation is delegated to the compiler:

1. Extract field names from the Rust struct (no types needed)
2. Pass field names to the compiler via `CompileSettings`
3. Use `PropertyInfo` from the compiler result to generate getter/setter code

```rust
pub fn tapir_script_derive(struct_def: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse2(struct_def).unwrap();

    // ... existing code to get file path ...

    let file_content = fs::read_to_string(&reduced_filename)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {e}", reduced_filename.display()));

    // Extract field names from the struct
    let named_fields = match &ast.data {
        syn::Data::Struct(data) => match &data.fields {
            syn::Fields::Named(named) => named,
            _ => panic!("TapirScript can only be derived on structs with named fields"),
        },
        _ => panic!("Can only be defined on structs"),
    };

    let field_names: Vec<String> = named_fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|i| i.to_string()))
        .collect();

    // Compile - pass field names for validation, compiler extracts properties from AST
    let compiled_content = match compiler::compile(
        &reduced_filename,
        &file_content,
        CompileSettings {
            available_fields: Some(field_names),
            enable_optimisations: true,
        },
    ) {
        Ok(content) => content,
        Err(mut diagnostics) => {
            eprintln!("{}", diagnostics.pretty_string(true));
            panic!("Compile error");
        }
    };

    // Generate getter/setter code using PropertyInfo from compiler
    let (getters, setters): (Vec<_>, Vec<_>) = compiled_content
        .properties
        .iter()
        .map(|prop| {
            let field_ident = syn::Ident::new(&prop.name, proc_macro2::Span::call_site());
            let index = prop.index as u8;

            let getter = quote! {
                #index => ::tapir_script::TapirProperty::to_i32(&self.#field_ident)
            };
            let setter = quote! {
                #index => { ::tapir_script::TapirProperty::set_from_i32(&mut self.#field_ident, value); }
            };

            (getter, setter)
        })
        .unzip();

    // ... rest of code generation using getters and setters ...
}
```

The compiler is responsible for:
- Parsing property declarations from the .tapir file
- Validating that each declared property has a corresponding field in `available_fields`
- Reporting errors for properties declared in .tapir that don't exist in the struct

### Phase 5: Error Messages

**Files**: `compiler/src/reporting.rs`, `compiler/src/reporting/format.rs`

Add new error variants:

```rust
pub enum CompilerErrorKind {
    // ... existing variants ...

    /// Property declared multiple times
    DuplicatePropertyDeclaration { name: String },

    /// Property name conflicts with a global variable
    PropertyConflictsWithGlobal { name: String },

    /// Property declared in .tapir but no corresponding field in Rust struct
    PropertyNotInStruct { name: String },
}
```

Example error output:

```
error: duplicate property declaration
  --> script.tapir:2:10
   |
 2 | property health: int;
   |          ^^^^^^
   |
   = note: 'health' is already declared as a property

error: property conflicts with global variable
  --> script.tapir:3:10
   |
 3 | property counter: int;
   |          ^^^^^^^
   |
   = note: 'counter' is declared as a global variable

error: property not found in struct
  --> script.tapir:4:10
   |
 4 | property nonexistent: int;
   |          ^^^^^^^^^^^
   |
   = note: 'nonexistent' is declared as a property but no corresponding field exists in the Rust struct
```

### Phase 6: Update Tests

**Files**: Various test files

1. Update existing compiler tests that use `CompileSettings`:
   - Change `properties: vec![...]` to `available_fields: Some(vec![...])`
   - Add property declarations to the .tapir test files
   - Tests without field validation can use `available_fields: None`

2. Add new snapshot tests for property declarations:
   - Valid declarations
   - Duplicate declarations
   - Conflicts with globals
   - Conflicts with built-ins
   - Property not in struct (declared in .tapir but not in available_fields)
   - Validation skipped when available_fields is None

### Phase 7: Documentation Updates

**Files**: `docs/tapir-reference.md`, `CLAUDE.md`

1. Update the language reference to document property declarations:

   ````markdown
   ## Properties

   Properties are variables shared between the script and Rust host. They must be
   declared at the top level:

   ```tapir
   property health: int;
   property position: fix;
   property alive: bool;
   ```
   ````

   Properties persist across frames and can be read/written from both script and Rust.

   ### Rust Integration

   Declared properties must have corresponding fields in the Rust struct:

   ```rust
   #[derive(TapirScript)]
   #[tapir("script.tapir")]
   struct MyScript {
       health: i32,      // matches property health: int
       position: Fix,    // matches property position: fix
       alive: bool,      // matches property alive: bool
       internal: i32,    // no declaration - not accessible from script
   }
   ```

   ```

   ```

2. Update CLAUDE.md if needed.

## Testing Strategy

### Unit Tests

1. **Parsing**:
   - `property foo: int;` - OK
   - `property bar: fix;` - OK
   - `property baz: bool;` - OK
   - `property bad: unknown;` - Parse error (unknown type)
   - `property;` - Parse error (missing name)
   - `property foo;` - Parse error (missing type)

2. **Symbol Resolution**:
   - Property visible in all functions
   - Property visible in spawned threads
   - Local variable can shadow property
   - Property cannot have same name as global - ERROR
   - Property cannot shadow built-in - ERROR
   - Duplicate property declarations - ERROR

3. **Type Checking**:
   - Property types correctly resolved in expressions
   - Type mismatches with properties reported correctly

### Integration Tests

1. **Basic property access**:

   ```tapir
   property counter: int;

   counter = counter + 1;
   ```

2. **Multiple properties**:

   ```tapir
   property a: int;
   property b: fix;
   property c: bool;

   a = 1;
   b = 2.5;
   c = true;
   ```

3. **Property with event handler**:

   ```tapir
   property damage_taken: int;

   event fn on_hit(amount: int) {
       damage_taken = damage_taken + amount;
   }
   ```

### Macro Tests

1. **Successful matching**:

   ```rust
   #[derive(TapirScript)]
   #[tapir("test.tapir")]  // contains: property health: int;
   struct Test {
       health: i32,
   }
   ```

2. **Missing Rust field** (compiler error via `PropertyNotInStruct`):

   ```rust
   #[derive(TapirScript)]
   #[tapir("test.tapir")]  // contains: property health: int;
   struct Test {
       // no health field - compiler error
   }
   ```

3. **Type mismatch** (Rust type error in generated code):

   ```rust
   #[derive(TapirScript)]
   #[tapir("test.tapir")]  // contains: property health: int;
   struct Test {
       health: Fix,  // Rust type error when setting i32 value on Fix field
   }
   ```

4. **Extra Rust fields OK**:
   ```rust
   #[derive(TapirScript)]
   #[tapir("test.tapir")]  // contains: property health: int;
   struct Test {
       health: i32,
       internal_state: i32,  // OK - not declared, not accessible from script
   }
   ```

## Alternatives Considered

### Keep Properties in Rust, Add Optional Declarations

Allow property declarations in .tapir as documentation, but still require `#[tapir(...)]` in Rust:

**Rejected because**:

- Doesn't achieve the goal of .tapir being self-documenting
- Creates redundancy between two sources of truth
- Validation would still be needed to keep them in sync

### Infer Types from Rust Automatically

Don't require declarations - infer everything from Rust struct:

**Rejected because**:

- Doesn't improve .tapir file readability
- Doesn't help LSP/tooling (still need to parse Rust)
- Was the original design that motivated this change

### Require Declaration Order to Match Rust

Property declaration order must match Rust field order:

**Rejected because**:

- Unnecessary coupling
- Makes refactoring harder
- Declaration order in .tapir is sufficient for index assignment

## Implementation Order

The phases should be implemented in order:

1. Grammar & AST - enables parsing property declarations
2. Compiler property extraction - enables compiler to validate and use AST properties
3. Compile result extension - enables macro to access property info
4. Macro updates - use PropertyInfo from compiler to generate code
5. Error messages - enables good UX
6. Update tests - verifies correctness
7. Documentation - enables users to adopt the feature

Testing should be added incrementally with each phase.
