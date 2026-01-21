# Error Handling Refactor

## Overview

This document outlines a refactor of the compiler's error reporting system to:

1. Separate semantic error data from presentation concerns
2. Use a builder pattern for flexible diagnostic construction
3. Support future internationalization (i18n) of error messages
4. Enable easy conversion to LSP diagnostics for editor integration

## Motivation

The current error reporting system has several pain points:

### Verbose Construction

Every error requires building an enum variant with all fields, then calling `.into_message(span)`:

```rust
diagnostics.add_message(
    CompilerErrorKind::FunctionAlreadyDeclared {
        function_name: function.name.to_string(),
        old_function_declaration: other_span,
        new_function_declaration: function.span,
    }
    .into_message(function.span),
);
```

### Span Confusion

Some spans are embedded in the enum variant (`expected_span`, `actual_span`), while others are passed to `into_message()`. It's unclear which span is "primary" and which are secondary labels.

### Presentation Mixed with Semantics

Secondary labels are baked into the error enum rather than being a separate concern. The `TypeError` variant contains `expected_span` and `actual_span` fields that are really presentation details.

### Disconnected Formatting

Error data lives in `reporting.rs`, but the human-readable message text is constructed separately in `format.rs` via ariadne's builder pattern. Easy to add a variant and forget to add formatting.

### No Translation Support

All user-facing strings are hardcoded in English throughout the formatting code.

## Design

### Core Types

```rust
/// Semantic error codes - just the data, no spans or presentation.
#[derive(Clone, Debug)]
pub enum ErrorKind {
    UnknownVariable { name: String },
    TypeError { expected: Type, actual: Type },
    FunctionAlreadyDeclared { name: String },
    InvalidTypeForIfCondition { got: Type },
    IncorrectNumberOfReturnTypes { expected: usize, actual: usize },
    // ... other variants with only semantic data
}

impl ErrorKind {
    /// Returns a unique, stable identifier for this error kind.
    ///
    /// Codes follow the pattern E0001, E0002, etc. (similar to rustc).
    /// These can be used to link to documentation, e.g.:
    /// `https://example.com/docs/errors/E0001`
    ///
    /// Codes are assigned manually and should never change once assigned,
    /// even if variants are reordered. A derive macro could be added later
    /// to reduce boilerplate.
    pub fn code(&self) -> &'static str {
        match self {
            Self::UnknownVariable { .. } => "E0001",
            Self::TypeError { .. } => "E0002",
            Self::FunctionAlreadyDeclared { .. } => "E0003",
            Self::InvalidTypeForIfCondition { .. } => "E0004",
            Self::IncorrectNumberOfReturnTypes { .. } => "E0005",
            // ... other variants
        }
    }
}

/// All user-facing translatable messages.
pub enum Message {
    // Primary messages (one per ErrorKind)
    UnknownVariable { name: String },
    TypeError { expected: Type, actual: Type },
    FunctionAlreadyDeclared { name: String },

    // Label messages (reusable across errors)
    FoundType { ty: Type },
    ExpectedType { ty: Type },
    DefinedAs { ty: Type },
    OriginallyDeclaredHere,
    AlsoDeclaredHere,
    UnknownVariableLabel,
    // ...
}

impl Message {
    /// Render the message to a string.
    /// Future: accept a locale/translator parameter.
    pub fn render(&self) -> String {
        match self {
            Message::UnknownVariable { name } => format!("unknown variable '{name}'"),
            Message::TypeError { expected, actual } => {
                format!("type mismatch: expected {expected}, found {actual}")
            }
            Message::FunctionAlreadyDeclared { name } => {
                format!("function '{name}' is already declared")
            }
            Message::FoundType { ty } => format!("found {ty}"),
            Message::ExpectedType { ty } => format!("expected {ty}"),
            Message::DefinedAs { ty } => format!("defined as {ty}"),
            Message::OriginallyDeclaredHere => "originally declared here".into(),
            Message::AlsoDeclaredHere => "also declared here".into(),
            Message::UnknownVariableLabel => "unknown variable".into(),
            // ...
        }
    }
}

impl ErrorKind {
    /// Get the primary message for this error.
    pub fn message(&self) -> Message {
        match self {
            Self::UnknownVariable { name } => Message::UnknownVariable { name: name.clone() },
            Self::TypeError { expected, actual } => Message::TypeError {
                expected: *expected,
                actual: *actual,
            },
            Self::FunctionAlreadyDeclared { name } => {
                Message::FunctionAlreadyDeclared { name: name.clone() }
            }
            // ...
        }
    }
}
```

### Builder Pattern

```rust
impl ErrorKind {
    /// Start building a diagnostic at the given span.
    pub fn at(self, span: Span) -> DiagnosticBuilder {
        DiagnosticBuilder {
            kind: self,
            primary_span: span,
            labels: vec![],
            notes: vec![],
            help: None,
        }
    }
}

/// A diagnostic under construction.
#[must_use = "diagnostics do nothing unless `.emit()`ed"]
pub struct DiagnosticBuilder {
    kind: ErrorKind,
    primary_span: Span,
    labels: Vec<(Span, Message)>,
    notes: Vec<Message>,
    help: Option<Message>,
}

impl DiagnosticBuilder {
    pub fn label(mut self, span: Span, message: Message) -> Self {
        self.labels.push((span, message));
        self
    }

    pub fn note(mut self, message: Message) -> Self {
        self.notes.push(message);
        self
    }

    pub fn help(mut self, message: Message) -> Self {
        self.help = Some(message);
        self
    }

    pub fn emit(self, diagnostics: &mut Diagnostics) {
        diagnostics.add(Diagnostic {
            kind: self.kind,
            primary_span: self.primary_span,
            labels: self.labels,
            notes: self.notes,
            help: self.help,
        });
    }
}
```

### Stored Diagnostic

```rust
/// A complete diagnostic ready for rendering.
pub struct Diagnostic {
    pub kind: ErrorKind,
    pub primary_span: Span,
    pub labels: Vec<(Span, Message)>,
    pub notes: Vec<Message>,
    pub help: Option<Message>,
}

impl Diagnostic {
    /// Get the primary message text.
    pub fn message(&self) -> String {
        self.kind.message().render()
    }
}
```

### Usage at Call Sites

**Before:**

```rust
diagnostics.add_message(
    CompilerErrorKind::FunctionAlreadyDeclared {
        function_name: function.name.to_string(),
        old_function_declaration: other_span,
        new_function_declaration: function.span,
    }
    .into_message(function.span),
);
```

**After:**

```rust
ErrorKind::FunctionAlreadyDeclared { name: function.name.into() }
    .at(function.span)
    .label(other_span, Message::OriginallyDeclaredHere)
    .label(function.span, Message::AlsoDeclaredHere)
    .emit(diagnostics);
```

**Before:**

```rust
diagnostics.add_message(
    CompilerErrorKind::TypeError {
        expected,
        expected_span: Some(ident.span),
        actual: value_type,
        actual_span: value_span,
    }
    .into_message(ident.span),
);
```

**After:**

```rust
ErrorKind::TypeError { expected, actual: value_type }
    .at(ident.span)
    .label(value_span, Message::FoundType { ty: value_type })
    .label(ident.span, Message::ExpectedType { ty: expected })
    .emit(diagnostics);
```

### Ariadne Integration

The `format.rs` module renders `Diagnostic` to ariadne:

```rust
impl Diagnostic {
    pub fn write<W: Write>(
        &self,
        w: W,
        cache: &mut DiagnosticCache,
        include_colour: bool,
    ) -> io::Result<()> {
        let mut report = ariadne::Report::build(ariadne::ReportKind::Error, self.primary_span)
            .with_code(self.kind.code())
            .with_message(self.message());

        for (span, message) in &self.labels {
            report = report.with_label(Label::new(*span).with_message(message.render()));
        }

        for note in &self.notes {
            report = report.with_note(note.render());
        }

        if let Some(help) = &self.help {
            report = report.with_help(help.render());
        }

        report
            .with_config(ariadne::Config::default().with_color(include_colour))
            .finish()
            .write_for_stdout(cache, w)
    }
}
```

This produces output like:

```
error[E0001]: unknown variable 'foo'
  --> script.tapir:5:10
   |
 5 |     x = foo + 1;
   |         ^^^ unknown variable
   |
```

### LSP Conversion

For future LSP support, conversion is straightforward:

```rust
impl Diagnostic {
    pub fn to_lsp(&self, cache: &DiagnosticCache) -> lsp_types::Diagnostic {
        let range = cache.span_to_range(self.primary_span);

        let related_information: Vec<_> = self.labels
            .iter()
            .map(|(span, message)| {
                lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location {
                        uri: cache.uri_for_span(*span),
                        range: cache.span_to_range(*span),
                    },
                    message: message.render(),
                }
            })
            .collect();

        lsp_types::Diagnostic {
            range,
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: Some(lsp_types::NumberOrString::String(self.kind.code().into())),
            code_description: Some(lsp_types::CodeDescription {
                href: lsp_types::Url::parse(
                    &format!("https://example.com/docs/errors/{}.html", self.kind.code())
                ).unwrap(),
            }),
            message: self.message(),
            related_information: if related_information.is_empty() {
                None
            } else {
                Some(related_information)
            },
            ..Default::default()
        }
    }
}

impl DiagnosticCache {
    /// Convert a byte-offset span to LSP line/column range.
    pub fn span_to_range(&self, span: Span) -> lsp_types::Range {
        let (start_line, start_col) = self.offset_to_position(span.file_id, span.start);
        let (end_line, end_col) = self.offset_to_position(span.file_id, span.end);

        lsp_types::Range {
            start: lsp_types::Position::new(start_line as u32, start_col as u32),
            end: lsp_types::Position::new(end_line as u32, end_col as u32),
        }
    }

    fn offset_to_position(&self, file_id: FileId, offset: usize) -> (usize, usize) {
        let (_, source) = self.map.get(&file_id).unwrap();
        // Use ariadne::Source methods to convert offset to line/column
        let line = source.get_offset_line(offset).map(|(_, line, _)| line).unwrap_or(0);
        let line_start = source.line(line).map(|l| l.offset()).unwrap_or(0);
        let column = offset - line_start;
        (line, column)
    }
}
```

## Current State

### What Exists

- `Diagnostics` struct collecting `Message` instances
- `Message` with `span` and `error: Box<MessageKind>`
- `MessageKind` enum with `ParseError`, `LexerError`, `ComplierError` variants
- `CompilerErrorKind` with many variants, some containing secondary spans
- `format.rs` building ariadne reports from each error variant
- `DiagnosticCache` implementing ariadne's `Cache` trait

### What Changes

- `CompilerErrorKind` becomes `ErrorKind` with simpler variants (no embedded spans)
- New `Message` enum for all user-facing text
- New `DiagnosticBuilder` builder
- New `Diagnostic` struct storing the complete diagnostic
- `format.rs` simplified to render `Diagnostic` generically
- All call sites updated to use builder pattern

## Implementation Plan

### Phase 1: Define New Types

**Files**: `compiler/src/reporting.rs`

1. Define `Message` enum with all user-facing strings
2. Implement `Message::render(&self) -> String`
3. Define new `ErrorKind` enum (copy of `CompilerErrorKind` without span fields)
4. Implement `ErrorKind::message(&self) -> Message`
5. Define `Diagnostic` struct
6. Define `DiagnosticBuilder` and builder methods

Keep existing types in place during migration.

### Phase 2: Update Format Module

**Files**: `compiler/src/reporting/format.rs`

1. Add `Diagnostic::write()` method using ariadne
2. Keep existing `Message::write_diagnostic()` working during migration

### Phase 3: Migrate Call Sites

**Files**: `compiler/src/compile/symtab_visitor.rs`, `compiler/src/compile/type_visitor.rs`, `compiler/src/compile/loop_visitor.rs`, `compiler/src/grammar.lalrpop`

Migrate each `diagnostics.add_message(...)` call to the new builder pattern. This can be done incrementally - both APIs can coexist during migration.

Example migration:

```rust
// Before
diagnostics.add_message(
    CompilerErrorKind::UnknownVariable(name.to_string())
        .into_message(span),
);

// After
ErrorKind::UnknownVariable { name: name.into() }
    .at(span)
    .label(span, Message::UnknownVariableLabel)
    .emit(diagnostics);
```

### Phase 4: Remove Old Types

**Files**: `compiler/src/reporting.rs`, `compiler/src/reporting/format.rs`

1. Remove old `MessageKind`, `CompilerErrorKind`, `ParseError` enums
2. Remove old formatting functions
3. Rename if needed (e.g., if `ErrorKind` should be `CompilerErrorKind`)

### Phase 5: Add LSP Support (Future)

**Files**: New `compiler/src/reporting/lsp.rs` or separate crate

1. Add `span_to_range()` to `DiagnosticCache`
2. Implement `Diagnostic::to_lsp()`
3. Create LSP server crate that uses these conversions

## Testing Strategy

### Unit Tests

1. Each `Message` variant renders correctly
2. Each `ErrorKind` produces the correct `Message`
3. Builder chains work correctly
4. `Diagnostic::write()` produces expected ariadne output

### Snapshot Tests

Existing snapshot tests should continue to pass after migration, verifying error output hasn't regressed.

### Migration Verification

During migration, both old and new APIs produce identical output for the same errors.

## Alternatives Considered

### String-Based Messages

Use string keys instead of an enum for messages:

```rust
.label(span, "found-type", [("ty", actual.to_string())])
```

**Rejected because**:

- Loses compile-time exhaustiveness checking
- Easy to typo message keys
- Types become strings, losing structure

### Convenience Functions for Common Errors

Provide helper functions like `errors::type_error(expected, actual, span1, span2)`:

**Rejected because**:

- Parameter lists become opaque (`span1`, `span2` - which is which?)
- Builder pattern is self-documenting via `.label()` calls
- Less flexible for adding context-specific labels

### Keep Spans in ErrorKind

Keep secondary spans in the error enum, extract them during formatting:

**Rejected because**:

- Mixes semantic data with presentation
- Different call sites may want different labels for the same span
- Harder to add/remove labels based on context

## Benefits

1. **Simpler error enum** - Just semantic data, no spans embedded
2. **Self-documenting call sites** - Each `.label()` shows what the span represents
3. **Translation-ready** - All strings go through `Message::render()`
4. **LSP-ready** - `Diagnostic` struct maps directly to LSP types
5. **Flexible** - Can add context-specific labels at each call site
6. **Fail-fast** - `#[must_use]` on `DiagnosticBuilder` ensures forgotten `.emit()` calls produce compiler warnings
7. **Documentable** - Stable error codes (E0001, etc.) enable linking to detailed error explanations
