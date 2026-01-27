# Implementation Plan: Symbol Span Tracking for Diagnostics

## Overview

Add source location tracking to symbols (values) rather than IR instructions. Each `SymbolId` has an associated "definition span" - where its value was computed. This enables precise compile-time warnings during optimization passes, pointing directly at problematic subexpressions.

## Motivation

For a warning about `sqrt(-1)`, we want to point at `-1`, not the whole call. For division by zero in `let x = y - y; let z = 5 / x;`, we want to point at `y - y`, not `x` or the division.

Symbol-based tracking naturally answers "where did this value come from?" which is exactly what we need for good diagnostics.

## Design Decisions

- **Symbol-based spans**: Track spans per `SymbolId`, not per IR instruction
- **Definition spans**: Each symbol's span points to where its value was computed
- **Span aliasing**: When symbols are renamed (SSA, etc.), spans are preserved/aliased
- **Optional spans**: Some symbols (function arguments, phi nodes) may not have meaningful spans

## Core Data Structure

`SymbolSpans` is kept separate from `SymTab` because:

- `SymTab` is built during symbol resolution (before we know values)
- `SymbolSpans` is populated during lowering (when we process expressions)
- They have different lifetimes and are modified at different stages

```rust
/// Tracks where each symbol's value was defined.
/// Used for precise diagnostic pointing during optimization.
#[derive(Debug, Clone, Default)]
pub struct SymbolSpans {
    spans: HashMap<SymbolId, Span>,
}

impl SymbolSpans {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record where a symbol's value was defined
    pub fn define(&mut self, symbol: SymbolId, span: Span) {
        self.spans.insert(symbol, span);
    }

    /// Get the definition span for a symbol
    pub fn get(&self, symbol: SymbolId) -> Option<Span> {
        self.spans.get(&symbol).copied()
    }

    /// Alias a new symbol to an existing symbol's span.
    /// Used when renaming symbols (SSA conversion, etc.)
    pub fn alias(&mut self, new_symbol: SymbolId, existing_symbol: SymbolId) {
        if let Some(span) = self.spans.get(&existing_symbol).copied() {
            self.spans.insert(new_symbol, span);
        }
    }

    /// Merge spans from another SymbolSpans (used when merging function IR)
    pub fn extend(&mut self, other: &SymbolSpans) {
        self.spans.extend(other.spans.iter().map(|(k, v)| (*k, *v)));
    }
}
```

## What Gets a Span

### Symbols WITH definition spans

| Source                     | Span points to                   |
| -------------------------- | -------------------------------- |
| Integer literal `42`       | The literal `42`                 |
| Fix literal `1.5`          | The literal `1.5`                |
| Bool literal `true`        | The literal `true`               |
| Binary op `a + b`          | The entire expression `a + b`    |
| Unary op `-x`              | The entire expression `-x`       |
| Function call result       | The call expression `foo(x, y)`  |
| Variable reference `x`     | The identifier `x`               |
| Property access `self.foo` | The access expression `self.foo` |

### Symbols WITHOUT definition spans (or with special handling)

| Source              | Handling                                                        |
| ------------------- | --------------------------------------------------------------- |
| Function parameters | No span (value comes from caller, not defined in this function) |
| Phi node results    | No span (merge point, trace back to incoming values if needed)  |
| Globals             | Span of the initializer expression                              |
| Symbols from moves  | Gets span of the source reference (e.g., `x` in `let y = x`)    |

## Symbol Creation vs Span Assignment

Symbols are created in two places:

1. **SymTabVisitor** - creates symbols for declared variables, function parameters, globals, properties
2. **Lowering** - creates temporary symbols for intermediate expressions

`SymbolSpans` is populated during **lowering**, not during symtab visiting. This is because we want the span of the _value_, not the declaration:

```tapir
let x = 1 + 2;  // We want span of "1 + 2", not "x"
let y = x;      // We want span of "x" (the reference)
```

When lowering processes `let x = 1 + 2`:

1. The symbol for `x` already exists (created by SymTabVisitor)
2. Lowering evaluates `1 + 2` and assigns to `x`
3. At this point, we record: `symbol_spans.define(x_symbol, span_of_1_plus_2)`

For reassignments, the span is overwritten with the new value's span:

```tapir
let x = 1;      // x's span = "1"
x = 2 + 3;      // x's span = "2 + 3" (overwrites)
```

This is correct - we want the span of the _current_ value, not the original declaration.

For **function parameters**, there's no definition expression in the function body - the value comes from the caller. Options:

- Return `None` from `symbol_spans.get()` (simplest)
- Use the parameter declaration span as a fallback
- Track call-site spans (complex, cross-function)

**Recommendation**: Parameters have no span initially. If we need to warn about a parameter value, the warning can note "parameter `x`" without pointing to a specific location, or we add declaration spans later as a fallback.

## Lowering Changes

### BlockVisitor Updates

```rust
struct BlockVisitor {
    blocks: Vec<TapIrBlock>,
    current_block: Vec<TapIr>,
    symbol_spans: SymbolSpans,  // NEW: track spans during lowering
    // ... other fields
}

impl BlockVisitor {
    fn blocks_for_expression(
        &mut self,
        expr: &ast::Expression,
        target: SymbolId,
        symtab: &mut SymTab,
    ) {
        // Record where this symbol's value comes from
        self.symbol_spans.define(target, expr.span);

        match &expr.kind {
            ast::ExpressionKind::Integer(value) => {
                self.current_block.push(TapIr::Constant(target, Constant::Int(*value)));
                // Span already recorded above
            }

            ast::ExpressionKind::BinaryOperator { lhs, op, rhs } => {
                let lhs_symbol = symtab.new_temporary();
                let rhs_symbol = symtab.new_temporary();

                // Recursively lower - each gets its own span
                self.blocks_for_expression(lhs, lhs_symbol, symtab);
                self.blocks_for_expression(rhs, rhs_symbol, symtab);

                self.current_block.push(TapIr::BinOp {
                    target,
                    lhs: lhs_symbol,
                    op: *op,
                    rhs: rhs_symbol,
                });
                // target's span is the whole binary expression (recorded above)
            }

            ast::ExpressionKind::Identifier(name) => {
                let source = symtab.resolve(name);
                self.current_block.push(TapIr::Move { target, source });
                // target's span is the identifier reference
            }

            // ... other cases
        }
    }
}
```

### Returning SymbolSpans

```rust
pub fn create_ir(f: &ast::Function<'_>, symtab: &mut SymTab) -> (TapIrFunction, SymbolSpans) {
    let mut block_visitor = BlockVisitor::default();
    let blocks = block_visitor.create_blocks(&f.statements, symtab);

    // ... construct TapIrFunction ...

    (function, block_visitor.symbol_spans)
}
```

### Threading Through the Pipeline

`SymbolSpans` needs to flow through compilation alongside `TapIrFunction`:

```rust
// In compile.rs or similar orchestration code

// 1. Create IR (populates SymbolSpans)
let (mut ir, mut symbol_spans) = create_ir(&function, &mut symtab);

// 2. SSA conversion (aliases spans when renaming)
make_ssa(&mut ir, &mut symbol_spans);

// 3. Optimizations (use spans for warnings)
fold_constants(&mut ir, &mut symbol_spans, &symtab, &mut diagnostics);

// 4. Further passes...
// symbol_spans is passed to any pass that might emit warnings
```

Alternatively, bundle them together:

```rust
pub struct IrWithSpans {
    pub ir: TapIrFunction,
    pub symbol_spans: SymbolSpans,
}
```

## SSA and Symbol Renaming

When SSA conversion or other passes rename symbols, spans must be aliased.

### SSA Conversion Example

```rust
// Before SSA:
//   x = 5
//   x = x + 1    // x is reassigned
//   y = x

// After SSA:
//   x_0 = 5
//   x_1 = x_0 + 1
//   y = x_1

// Spans:
//   x_0 → span of `5`
//   x_1 → span of `x + 1` (or `x_0 + 1` expression)
//   y   → span of `x` reference (which should alias to x_1's definition)
```

### Implementation

```rust
impl SsaConverter {
    fn rename_symbol(
        &mut self,
        old: SymbolId,
        new: SymbolId,
        symbol_spans: &mut SymbolSpans,
    ) {
        // When creating a new version of a symbol, alias its span
        symbol_spans.alias(new, old);
    }
}
```

### Phi Nodes

Phi nodes merge values from different control flow paths. Options:

1. **No span**: Phi results have no single definition site
2. **All incoming spans**: Track `Vec<Span>` for phi results
3. **Pick one**: Use the span from the "most likely" path (not recommended)

**Recommendation**: Start with option 1 (no span). If we need to warn about a phi result, we can trace back to the incoming values and report all their spans.

```rust
impl SymbolSpans {
    /// For phi nodes, we don't set a span. Warnings about phi results
    /// should trace back to the incoming values.
    pub fn define_phi(&mut self, _symbol: SymbolId) {
        // Intentionally don't insert - symbol will have no span
    }
}
```

## Optimization Pass Changes

Optimization passes receive `&mut SymbolSpans` and use it for warnings.

### Constant Folding Example

```rust
pub fn fold_constants(
    function: &mut TapIrFunction,
    symbol_spans: &mut SymbolSpans,
    symtab: &SymTab,
    diagnostics: &mut Diagnostics,
) {
    let mut constants: HashMap<SymbolId, Constant> = HashMap::new();

    // ... propagate constants ...

    for block in function.blocks_mut() {
        for instr in block.instrs_mut() {
            match instr {
                TapIr::BinOp { op: BinaryOperator::Div, rhs, .. } => {
                    if let Some(Constant::Int(0)) = constants.get(rhs) {
                        // Get the span of the RHS - where the zero came from
                        if let Some(span) = symbol_spans.get(*rhs) {
                            WarningKind::DivisionByZero
                                .at(span)
                                .emit(diagnostics);
                        }
                    }
                }

                TapIr::CallBuiltin { id, args, .. } => {
                    // Check for invalid constant arguments
                    if let Some(warning) = check_builtin_args(*id, args, &constants, symtab) {
                        // Get span of the problematic argument
                        let arg_idx = warning.argument_index;
                        if let Some(span) = symbol_spans.get(args[arg_idx]) {
                            warning.kind.at(span).emit(diagnostics);
                        }
                    }
                }

                _ => {}
            }
        }
    }
}
```

### Copy Propagation

When copy propagation replaces a symbol reference, the span should follow:

```rust
// Before: y = x; z = y + 1
// After:  y = x; z = x + 1  (y replaced with x)

// If we warn about z's computation, we trace back through x
// x's span points to where x was defined, which is correct
```

No special handling needed - we're not creating new symbols, just substituting.

### Dead Code Elimination

When removing dead code, we can optionally remove spans for dead symbols:

```rust
impl SymbolSpans {
    pub fn remove(&mut self, symbol: SymbolId) {
        self.spans.remove(&symbol);
    }

    pub fn retain(&mut self, live_symbols: &HashSet<SymbolId>) {
        self.spans.retain(|k, _| live_symbols.contains(k));
    }
}
```

## Warning Infrastructure

Same as before, but warnings reference symbol spans:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub enum WarningKind {
    /// Division by zero detected after constant propagation
    DivisionByZero,

    /// Builtin call with constant args will fail at runtime
    BuiltinWillFail {
        name: String,
        reason: String,
    },

    /// Integer overflow detected during constant folding
    IntegerOverflow,
}

impl WarningKind {
    pub fn code(&self) -> &'static str {
        match self {
            Self::DivisionByZero => "W0001",
            Self::BuiltinWillFail { .. } => "W0002",
            Self::IntegerOverflow => "W0003",
        }
    }

    pub fn at(self, span: Span) -> WarningBuilder {
        WarningBuilder::new(self, span)
    }
}
```

### Multi-Span Warnings

For richer diagnostics, we might want to show multiple locations:

```
warning: division by zero
  --> script.tapir:4:9
   |
 4 | let z = x / y;
   |         ^^^^^ in this division
   |
  --> script.tapir:2:9
   |
 2 | let y = a - a;
   |         ^^^^^ this evaluates to zero
```

This requires tracking both:

1. The instruction location (where the division is)
2. The symbol's definition span (where the zero came from)

For now, we focus on the symbol span (more precise). We can add instruction spans later if needed for "in this division" style labels.

## Example: Full Trace

Source:

```tapir
let a = 5;
let b = 5;
let x = a - b;      // Line 3: x defined here
let y = 10 / x;     // Line 4: division here
```

During lowering:

- `a` → symbol s1, span = "5" (line 1)
- `b` → symbol s2, span = "5" (line 2)
- `a - b` → symbol s3, span = "a - b" (line 3)
- `10` → symbol s4, span = "10" (line 4)
- `10 / x` → symbol s5, span = "10 / x" (line 4)

During constant folding:

- s1 = 5, s2 = 5
- s3 = s1 - s2 = 0
- Division s4 / s3 detected with s3 = 0
- Look up `symbol_spans.get(s3)` → span of "a - b" on line 3
- Emit warning pointing to line 3

Output:

```
warning: division by zero
  --> script.tapir:3:9
   |
 3 | let x = a - b;
   |         ^^^^^ this expression evaluates to zero
```

## Implementation Order

1. **Create SymbolSpans struct** with basic methods
2. **Update lowering** to populate SymbolSpans during IR creation
3. **Thread SymbolSpans** through compilation pipeline
4. **Update SSA conversion** to alias spans when renaming
5. **Add Severity enum** to diagnostics
6. **Add WarningKind enum** with initial variants
7. **Add Diagnostics::warn()** method
8. **Update constant folding** to emit warnings using symbol spans
9. **Add tests** for span tracking and warning emission

## Testing Strategy

### Span Tracking Tests

```rust
#[test]
fn test_binary_op_span() {
    let input = "let x = 1 + 2;";
    let (_, symbol_spans) = compile_to_ir(input);

    // Find the symbol for the binary op result
    let x_symbol = find_symbol_for_variable("x");
    let span = symbol_spans.get(x_symbol).unwrap();

    assert_eq!(span_text(span), "1 + 2");
}

#[test]
fn test_span_through_variable() {
    let input = r#"
        let a = -1;
        let b = a;  // b should trace back to -1
    "#;
    let (_, symbol_spans) = compile_to_ir(input);

    let b_symbol = find_symbol_for_variable("b");
    let span = symbol_spans.get(b_symbol).unwrap();

    // b's span points to the `a` reference, not `-1`
    // But if we trace through, we'd find a's definition
    assert_eq!(span_text(span), "a");
}
```

### Warning Tests

```rust
#[test]
fn test_division_by_zero_warning_points_to_source() {
    let input = r#"
        let x = 5 - 5;
        let y = 10 / x;
    "#;

    let diagnostics = compile(input);
    let warning = diagnostics.warnings().next().unwrap();

    // Warning should point to "5 - 5", not "x" or "10 / x"
    assert_eq!(span_text(warning.span), "5 - 5");
}

#[test]
fn test_sqrt_negative_warning() {
    let input = "let x = sqrt(-1.0);";

    let diagnostics = compile(input);
    let warning = diagnostics.warnings().next().unwrap();

    // Warning should point to "-1.0"
    assert_eq!(span_text(warning.span), "-1.0");
}
```

## Future Considerations

- **Multi-span diagnostics**: Show both "where the operation is" and "where the value came from"
- **Span chains**: For complex transformations, track a chain of spans showing how a value was derived
- **Source maps**: If we ever compile from another format, symbol spans provide the foundation for source maps
- **IDE integration**: "Go to definition" for values using symbol spans
