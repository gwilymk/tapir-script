# Implementation Plan: Immediate-Operand Arithmetic Instructions

## Overview

Patterns like `x = x + 1` are very common in tapir-script (frame counters, loop variables, animation offsets). Currently this compiles to 5 words of bytecode (`getprop` + `loadk`[2 words] + `add` + `setprop`). By adding immediate-operand instructions that encode a small constant directly in the instruction's `b` field, we eliminate the `LoadConstant` + register arithmetic pattern, saving both code size (2 words per occurrence) and one instruction dispatch.

```tapir
# These common patterns benefit from immediate instructions:
var i = 0;
loop {
    if i >= 100 { break; }
    x = x + 1;       # addi instead of loadk + add
    y = y + 0.5;      # fixaddi instead of loadk + fixadd (where fixadd = add for fix types)
    i = i + 1;
    wait;
}
```

## Design Decisions

### New Opcodes (8 total): Two Encoding Schemes

At the VM level, int and fix are both i32. The 8 new opcodes provide two ways to encode an immediate value in the `b` field, not two type-specific instruction sets. For `Add`/`Sub`, either encoding can be used for either type — the optimiser picks whichever one the value fits into.

**Direct encoding** (immediate used as-is, range 0–255):

- `AddI` — `target = a + imm`
- `SubI` — `target = a - imm`
- `MulI` — `target = a * imm`
- `DivI` — `target = a.div_euclid(imm)` (optimiser won't emit imm=0)

**Shifted encoding** (immediate shifted left by 4, effective range 0–4080 in steps of 16):

- `FixAddI` — `target = a + (imm << 4)`
- `FixSubI` — `target = a - (imm << 4)`
- `FixMulI` — `target = (a * imm) >> 4`
- `FixDivI` — `target = (a << 4) / imm`

The names use "Fix" for the shifted variants because the shift-by-4 originated from fix-point semantics, but the `Add`/`Sub` shifted opcodes work equally well for int values that happen to be multiples of 16.

### Cross-Type Encoding for Add/Sub

Since `AddI` just does `a + imm` and `FixAddI` does `a + (imm << 4)`, the optimiser can use **either** for **either** type:

| Value to add     | Type | Encoding | Opcode    | imm |
| ---------------- | ---- | -------- | --------- | --- |
| 1                | int  | direct   | `AddI`    | 1   |
| 200              | int  | direct   | `AddI`    | 200 |
| 256              | int  | shifted  | `FixAddI` | 16  |
| 4080             | int  | shifted  | `FixAddI` | 255 |
| 0.5 (raw 128)    | fix  | direct   | `AddI`    | 128 |
| 1.0 (raw 256)    | fix  | shifted  | `FixAddI` | 16  |
| 10.0 (raw 2560)  | fix  | shifted  | `FixAddI` | 160 |
| 0.015625 (raw 4) | fix  | direct   | `AddI`    | 4   |

The optimiser tries direct first (value fits in 0..=255), then shifted (value is a multiple of 16 and value/16 fits in 0..=255).

This means int add/sub can encode any value in `{0..=255} ∪ {16, 32, ..., 4080}`, and fix add/sub can encode any raw value in the same set.

### Mul/Div: Encoding is Semantically Fixed

Unlike `Add`/`Sub`, the `Mul`/`Div` opcodes have different semantics:

- `MulI`: `target = a * imm` (straight integer multiply)
- `FixMulI`: `target = (a * imm) >> 4` (fixed-point multiply)

These are NOT interchangeable. `MulI` is only for `BinaryOperator::Mul` (int), and `FixMulI` is only for `BinaryOperator::FixMul` (fix). The `BinaryOperator` already distinguishes these, so the emitter knows which to use without any extra type information.

### Why `<< 4` for Shifted Encoding

Fix numbers use 24.8 fixed-point (`Num<i32, 8>`) with 8 fractional bits. Storing a raw fix value directly in 8 bits gives range 0 to 255/256 (~0.996) — too small for common values like 1.0.

Shifting left by 4 means the immediate encodes multiples of 16 in raw representation:

- Range: 0.0 to (255×16)/256 = 15.9375
- Resolution: 1/16 = 0.0625
- Common values: 0.25 (imm=4), 0.5 (imm=8), 1.0 (imm=16), 2.0 (imm=32), 10.0 (imm=160)

But as shown above, this encoding is also useful for int values that are multiples of 16.

### VM Operations for Fix Mul/Div

The shifted mul/div need careful implementation:

```rust
// FixMulI: a is fix (24.8), imm represents a fix value with raw = imm << 4
// Full multiplication: (a * (imm << 4)) >> 8
// Simplified: (a * imm) >> 4
FixMulI:  target = (a * imm) >> 4

// FixDivI: a is fix (24.8), imm represents a fix value with raw = imm << 4
// Full division: (a << 8) / (imm << 4)
// Simplified: (a << 4) / imm
FixDivI:  target = (a << 4) / imm
```

### Negative Constant Normalisation

The optimisation pass also handles negative constants by swapping the operation:

- `x + (-N)` where N fits an encoding → `Sub` with that immediate
- `x - (-N)` where N fits an encoding → `Add` with that immediate

For example, `x + (-3)` becomes `SubI x, 3` and `x + (-256)` becomes `FixSubI x, 16`.

This ensures we don't need negative immediates, and the same direct-then-shifted encoding selection applies to the absolute value.

### Commutative Normalisation

For commutative ops (`Add`, `Mul`, `FixMul`), if the constant is on the LHS, the pass swaps to put it on the RHS:

- `1 + x` → `AddI x, 1`
- `2 * x` → `MulI x, 2`

Non-commutative ops with constant LHS are handled differently:

- **`Div`/`FixDiv`**: constant LHS cannot be converted (e.g., `10 / x` stays as `loadk + div`)
- **`Sub`**: constant LHS is rewritten as `neg` + `add immediate`:
  - `10 - x` → `neg tmp, x` + `AddI target, tmp, 10`
  - This saves 1 instruction word (2 words vs `loadk`[2] + `sub`[1] = 3 words)
  - The same encoding selection applies to the constant (direct first, then shifted)
  - This requires the pass to have `&mut SymTab` access to create the temporary

## IR Changes

### `Operand` Enum

**File:** `compiler/src/compile/ir.rs`

Add a new enum to represent either a register operand or an immediate value. The two immediate variants represent the two encoding schemes — they carry encoding information, not type information:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Symbol(SymbolId),
    Immediate(u8),         // value used directly (maps to AddI/SubI/MulI/DivI)
    ShiftedImmediate(u8),  // value << 4 at runtime (maps to FixAddI/FixSubI/FixMulI/FixDivI)
}
```

Change `BinOp`'s `rhs` field from `SymbolId` to `Operand`:

```rust
pub enum TapIr {
    // ...existing variants...

    BinOp {
        target: SymbolId,
        lhs: SymbolId,
        op: BinaryOperator,
        rhs: Operand,  // was: SymbolId
    },

    // ...remaining variants...
}
```

This avoids duplicating all `BinOp` matching logic across the compiler. All existing passes that match on `BinOp` get updated in-place.

### Symbol Iteration Updates

**File:** `compiler/src/compile/ir/symbol_iter.rs`

The `sources()` and `sources_mut()` iterators for `BinOp` must handle the `Operand` enum. When rhs is an immediate, it has no symbol to iterate:

```rust
// In SymbolIter::new_source
TapIr::BinOp { lhs, rhs, .. } => match rhs {
    Operand::Symbol(rhs) => Self::Two(Some(*lhs), Some(*rhs)),
    Operand::Immediate(_) | Operand::ShiftedImmediate(_) => Self::One(Some(*lhs)),
},

// In SymbolIterMut::new_source
TapIr::BinOp { lhs, rhs, .. } => match rhs {
    Operand::Symbol(rhs) => Self::Two(Some(lhs), Some(rhs)),
    Operand::Immediate(_) | Operand::ShiftedImmediate(_) => Self::One(Some(lhs)),
},
```

**File:** `compiler/src/compile/ir.rs`

Update the `symbols()` method on `TapIrBlock`:

```rust
TapIr::BinOp {
    target, lhs, rhs, ..
} => {
    symbols.extend([*target, *lhs]);
    if let Operand::Symbol(rhs) = rhs {
        symbols.insert(*rhs);
    }
}
```

### Pretty Print Updates

**File:** `compiler/src/compile/ir/pretty_print.rs`

Display immediate operands as literal values:

```rust
TapIr::BinOp {
    target,
    lhs,
    op,
    rhs,
} => {
    let rhs_str = match rhs {
        Operand::Symbol(s) => symtab.debug_name_for_symbol(*s),
        Operand::Immediate(imm) => format!("imm({imm})"),
        Operand::ShiftedImmediate(imm) => format!("simm({imm})"),
    };
    write!(
        output,
        "{} = {} {op} {}",
        symtab.debug_name_for_symbol(*target),
        symtab.debug_name_for_symbol(*lhs),
        rhs_str
    )
}
```

### Lowering

**File:** `compiler/src/compile/ir/lowering.rs`

Wrap existing `rhs` in `Operand::Symbol()` — no semantic change:

```rust
operator => {
    let rhs_target = symtab.new_temporary();

    self.blocks_for_expression(lhs, lhs_target, symtab);
    self.blocks_for_expression(rhs, rhs_target, symtab);

    self.current_block.push(TapIr::BinOp {
        target: target_symbol,
        lhs: lhs_target,
        rhs: Operand::Symbol(rhs_target),
        op: *operator,
    });
}
```

## Shared Helper: `constant_map()`

**File:** `compiler/src/compile/ir.rs`

Both `constant_folding` and `immediate_arithmetic` need to build a map from `SymbolId` to `Constant` by walking all blocks. Extract this as a method on `TapIrFunction`:

```rust
impl TapIrFunction {
    pub fn constant_map(&self) -> HashMap<SymbolId, Constant> {
        let mut constants = HashMap::new();
        for block in self.blocks() {
            for instr in block.instrs() {
                let TapIr::Constant(target, value) = *instr else {
                    continue;
                };
                if constants.insert(target, value).is_some() {
                    panic!("Should only be assigned once, because SSA");
                }
            }
        }
        constants
    }
}
```

Then both passes just call `f.constant_map()` instead of duplicating the walk. Update `constant_folding.rs` to use it too.

## Optimisation Pass

### New Pass: `immediate_arithmetic`

**File:** `compiler/src/compile/ir/optimisations/immediate_arithmetic.rs` (new)

This pass runs after constant folding (or as part of the optimisation loop). It scans for `BinOp` instructions where the RHS is a `Symbol` that was defined by a `Constant` instruction, and the constant fits the immediate range.

```rust
pub fn immediate_arithmetic(f: &mut TapIrFunction, symtab: &mut SymTab) -> OptimisationResult {
    let constants = f.constant_map();

    // For each BinOp with rhs: Operand::Symbol(s) where s maps to a constant:
    //   - Check if the op is Add/Sub/Mul/Div/FixMul/FixDiv
    //   - Extract the raw i32 value from the constant
    //   - Try to fit it into an encoding (direct first, then shifted)
    //   - If so, replace rhs with Operand::Immediate or Operand::ShiftedImmediate
    //
    // Also handle commutative normalisation:
    //   - If LHS is constant and op is commutative (Add/Mul/FixMul), swap lhs/rhs
    //     then try immediate conversion on the new rhs
    //
    // Also handle Sub with constant LHS:
    //   - `N - x` → insert `neg tmp, x` before, change to `Add target, tmp, imm(N)`
    //   - Requires symtab to create the temporary
    //
    // Also handle negative normalisation:
    //   - If constant is negative and op is Add, convert to Sub with |constant|
    //   - If constant is negative and op is Sub, convert to Add with |constant|
}
```

#### Encoding Selection

Given a constant value (i32 for int, raw i32 for fix), the pass tries encodings in order:

1. **Direct**: if `0 <= value <= 255` → `Operand::Immediate(value as u8)`
2. **Shifted**: if `value % 16 == 0` and `0 <= value / 16 <= 255` → `Operand::ShiftedImmediate((value / 16) as u8)`
3. **Neither**: leave as `Operand::Symbol` (uses `loadk` + register op)

This applies uniformly regardless of whether the constant is `Constant::Int` or `Constant::Fix` — we just extract the raw i32 value.

**For `Mul`/`Div`**: only direct encoding is valid (shifted mul/div have different semantics).
**For `FixMul`/`FixDiv`**: only shifted encoding is valid.
**For `Add`/`Sub`**: both encodings are valid, try direct first for maximum coverage.

#### Negative Constant Normalisation

For Add/Sub, negative constants are normalised by flipping the operation and taking the absolute value, then the encoding selection runs on the absolute value:

- `x + (-N)` → `x - N` (then try encoding N)
- `x - (-N)` → `x + N` (then try encoding N)

For example:

- `x + (-3)` → `SubI x, 3` (direct encoding)
- `x + (-256)` → `FixSubI x, 16` (shifted encoding, 256/16=16)

#### Constant-LHS Subtraction

When `Sub` has a constant LHS (e.g., `10 - x`), it can't be swapped like commutative ops. Instead, the pass rewrites it as `neg` + `add immediate`:

- `10 - x` → `neg tmp, x` then `AddI target, tmp, 10`
- `256 - x` → `neg tmp, x` then `FixAddI target, tmp, 16`

This replaces 3 instruction words (`loadk`[2] + `sub`[1]) with 2 (`neg`[1] + `addi`[1]). Same dispatch count but smaller code. The encoding selection for the constant follows the same direct-then-shifted logic.

The pass inserts the `UnaryOp::Neg` instruction before the `BinOp` and rewrites the `BinOp` in-place. It needs `&mut SymTab` to create the temporary for the negation result.

Does NOT apply to `Div`/`FixDiv` with constant LHS — `10 / x` has no equivalent rewrite.

### Wiring Up the Pass

**File:** `compiler/src/compile/ir/optimisations.rs`

Add to the `OPTIMISATIONS` list, after `constant_folding`:

```rust
mod immediate_arithmetic;

// In OPTIMISATIONS:
(
    "immediate_arithmetic",
    &(immediate_arithmetic::immediate_arithmetic
        as fn(&mut TapIrFunction, &mut SymTab) -> OptimisationResult),
),
```

### Constant Folding Updates

**File:** `compiler/src/compile/ir/optimisations/constant_folding.rs`

The constant folding pass accesses `BinOp.rhs` as a `SymbolId` to look up in the constants map. With `Operand`, it must handle both variants:

```rust
let TapIr::BinOp {
    target,
    lhs,
    op,
    rhs,
} = instr
else {
    continue;
};

let lhs_constant = constants.get(lhs).copied();
let rhs_constant = match rhs {
    Operand::Symbol(s) => constants.get(s).copied(),
    Operand::Immediate(_) | Operand::ShiftedImmediate(_) => None,  // Already an immediate, skip
};
```

The `rhs_span` lookup needs the same treatment — extract the `SymbolId` when it's a `Symbol`:

```rust
let rhs_span = match rhs {
    Operand::Symbol(s) => symbol_spans.get(*s),
    Operand::Immediate(_) | Operand::ShiftedImmediate(_) => None,
};
```

All the replacement patterns that create `TapIr::Move { source: *rhs }` or `TapIr::BinOp { rhs: *rhs }` must also handle `Operand`. The `take_rhs` pattern (`Move { target: t, source: *rhs }`) only makes sense when rhs is a `Symbol` — if it's `Immediate`, those patterns can't trigger (the immediate case means the constant was already converted).

The places in constant folding where new `BinOp` instructions are constructed (strength reduction, integer fix multiplication) should use `Operand::Symbol(temp)` for the newly created temporary:

```rust
*instr = TapIr::BinOp {
    target: t,
    lhs: *lhs,
    op: B::Shl,
    rhs: Operand::Symbol(temp),
};
```

## Bytecode Changes

### New Opcodes

**File:** `bytecode/src/lib.rs`

Add 8 new opcodes after the existing binop group:

```rust
#[repr(u8)]
pub enum Opcode {
    // ...existing opcodes through BitOr...

    // Immediate binops (b field is immediate value, not a register)
    AddI,
    SubI,
    MulI,
    DivI,
    FixAddI,
    FixSubI,
    FixMulI,
    FixDivI,

    // Unary ops
    Neg,
    Not,
    BitNot,
    // ...rest unchanged...
}
```

The `binop` range assertion on `Type1::binop()` doesn't include these — they use a separate constructor.

### New Constructor

```rust
impl Type1 {
    /// Immediate binary operation: target = a op imm
    /// The b field contains an immediate value, not a register index.
    pub const fn binop_imm(opcode: Opcode, target: u8, lhs: u8, imm: u8) -> Self {
        assert!(
            opcode as u8 >= Opcode::AddI as u8 && opcode as u8 <= Opcode::FixDivI as u8,
            "Invalid opcode for immediate binary operator",
        );

        Self::new3(opcode, target, lhs, imm)
    }
}
```

## VM Changes

**File:** `vm/src/state.rs`

Add 8 new dispatch arms. These read `a` from a register but use `b` as an immediate value:

```rust
macro_rules! binop_imm {
    ($a:ident, $imm:ident, $op:expr) => {{
        type1!(target, a, b);
        let $a = self.get_reg(a);
        let $imm = b as i32;
        self.set_reg(target, $op);
    }};
}

O::AddI => binop_imm!(a, imm, a + imm),
O::SubI => binop_imm!(a, imm, a - imm),
O::MulI => binop_imm!(a, imm, a * imm),
O::DivI => binop_imm!(a, imm, a.div_euclid(imm)),

O::FixAddI => binop_imm!(a, imm, a + (imm << 4)),
O::FixSubI => binop_imm!(a, imm, a - (imm << 4)),
O::FixMulI => binop_imm!(a, imm, (a * imm) >> 4),
O::FixDivI => binop_imm!(a, imm, (a << 4) / imm),
```

## Disassembler Changes

**File:** `compiler/src/compile/disassemble.rs`

Add display cases for the 8 new opcodes. These should show the immediate value distinctly from register references:

```rust
bytecode::Opcode::AddI => t1!("addi", 3),
bytecode::Opcode::SubI => t1!("subi", 3),
bytecode::Opcode::MulI => t1!("muli", 3),
bytecode::Opcode::DivI => t1!("divi", 3),
bytecode::Opcode::FixAddI => t1!("fixaddi", 3),
bytecode::Opcode::FixSubI => t1!("fixsubi", 3),
bytecode::Opcode::FixMulI => t1!("fixmuli", 3),
bytecode::Opcode::FixDivI => t1!("fixdivi", 3),
```

Note: The disassembler uses the same `t1!("...", 3)` macro which prints all three fields. The reader should understand that the third field (b) is an immediate for these opcodes, not a register. This matches how the existing disassembler works — it doesn't distinguish between register and non-register fields (e.g., `call builtin` also uses fields as non-register values).

## Compiler Bytecode Emission

**File:** `compiler/src/compile.rs`

Update the `BinOp` emission to match on `Operand` variants. The `Operand` carries the encoding choice; the `BinaryOperator` determines which family of opcodes:

```rust
TapIr::BinOp {
    target,
    lhs,
    op,
    rhs,
} => match rhs {
    Operand::Symbol(rhs) => {
        self.bytecode.binop(v(target), v(lhs), *op, v(rhs));
    }
    Operand::Immediate(imm) => {
        self.bytecode.binop_direct_imm(v(target), v(lhs), *op, *imm);
    }
    Operand::ShiftedImmediate(imm) => {
        self.bytecode.binop_shifted_imm(v(target), v(lhs), *op, *imm);
    }
},
```

Add two methods to `Bytecode`:

```rust
fn binop_direct_imm(&mut self, target: u8, lhs: u8, binop: BinaryOperator, imm: u8) {
    let opcode = match binop {
        BinaryOperator::Add => Opcode::AddI,
        BinaryOperator::Sub => Opcode::SubI,
        BinaryOperator::Mul => Opcode::MulI,
        BinaryOperator::Div => Opcode::DivI,
        _ => unreachable!("Invalid op for direct immediate"),
    };
    self.data.push(Type1::binop_imm(opcode, target, lhs, imm).encode());
}

fn binop_shifted_imm(&mut self, target: u8, lhs: u8, binop: BinaryOperator, imm: u8) {
    let opcode = match binop {
        BinaryOperator::Add => Opcode::FixAddI,
        BinaryOperator::Sub => Opcode::FixSubI,
        BinaryOperator::FixMul => Opcode::FixMulI,
        BinaryOperator::FixDiv => Opcode::FixDivI,
        _ => unreachable!("Invalid op for shifted immediate"),
    };
    self.data.push(Type1::binop_imm(opcode, target, lhs, imm).encode());
}
```

The mapping is clean: `Operand::Immediate` always maps to the direct opcodes (`AddI`/`SubI`/`MulI`/`DivI`), and `Operand::ShiftedImmediate` always maps to the shifted opcodes (`FixAddI`/`FixSubI`/`FixMulI`/`FixDivI`). No type information needed.

## Files to Modify

| File                                                            | Changes                                                                               |
| --------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| `bytecode/src/lib.rs`                                           | Add 8 opcodes, add `binop_imm` constructor                                            |
| `vm/src/state.rs`                                               | 8 new dispatch arms using `binop_imm!` macro                                          |
| `compiler/src/compile/disassemble.rs`                           | 8 new display cases                                                                   |
| `compiler/src/compile/ir.rs`                                    | Add `Operand` enum, change `BinOp.rhs` type, update `symbols()`, add `constant_map()` |
| `compiler/src/compile/ir/symbol_iter.rs`                        | Handle `Operand` in source iterators                                                  |
| `compiler/src/compile/ir/pretty_print.rs`                       | Display `Operand::Immediate`/`Operand::ShiftedImmediate` as literals                  |
| `compiler/src/compile/ir/optimisations/immediate_arithmetic.rs` | **New** — optimisation pass                                                           |
| `compiler/src/compile/ir/optimisations.rs`                      | Wire up new pass, add `mod immediate_arithmetic`                                      |
| `compiler/src/compile/ir/optimisations/constant_folding.rs`     | Handle `Operand` in existing `BinOp` folding, use `constant_map()`                    |
| `compiler/src/compile.rs`                                       | Emit immediate opcodes when `rhs` is `Immediate`/`ShiftedImmediate`                   |
| `compiler/src/compile/ir/lowering.rs`                           | Wrap existing `rhs` in `Operand::Symbol()`                                            |

## Implementation Order

### Phase 1: Bytecode + VM

1. **bytecode/src/lib.rs**: Add 8 opcodes after `BitOr`, add `Type1::binop_imm()` constructor
2. **vm/src/state.rs**: Add `binop_imm!` macro and 8 dispatch arms
3. **compiler/src/compile/disassemble.rs**: Add 8 display cases

**Compilable:** Yes (opcodes exist but nothing emits them yet)

### Phase 2: IR Changes

1. **compiler/src/compile/ir.rs**: Add `Operand` enum, change `BinOp.rhs` to `Operand`, update `symbols()`
2. **compiler/src/compile/ir/symbol_iter.rs**: Update `new_source` and `new_source_mut` for `BinOp`
3. **compiler/src/compile/ir/pretty_print.rs**: Display `Operand` variants
4. **compiler/src/compile/ir/lowering.rs**: Wrap `rhs` in `Operand::Symbol()`
5. **compiler/src/compile/ir/optimisations/constant_folding.rs**: Handle `Operand` in `BinOp` matching — extract symbol from `Operand::Symbol` for constant lookups, skip folding for `Immediate`/`ShiftedImmediate`

**Compilable:** Yes (all `BinOp` still uses `Operand::Symbol`, functionally identical)

### Phase 3: Bytecode Generation

1. **compiler/src/compile.rs**: Update `BinOp` emission to match on `Operand` variants, add `binop_imm()` method to `Bytecode`

**Compilable:** Yes (immediate path exists but is never reached since optimiser hasn't been added)

### Phase 4: Optimisation Pass

1. **compiler/src/compile/ir/optimisations/immediate_arithmetic.rs**: New pass implementing the immediate detection, commutative normalisation, constant-LHS subtraction, and negative normalisation
2. **compiler/src/compile/ir/optimisations.rs**: Add `mod immediate_arithmetic` and wire into `OPTIMISATIONS` list

**Compilable:** Yes — feature complete

### Phase 5: Testing

Test files and snapshot verification.

## Testing Strategy

### Compiler Snapshot Tests

Add test files in `compiler/src/compile/snapshot_tests/compiler/`:

**`immediate_int.tapir`:**

```tapir
# optimise
property x: int;
x = x + 1;
x = x + 255;
x = x - 10;
x = x * 3;
x = 10 - x;
```

Expected: `addi`, `addi`, `subi`, `muli` for the first four; `neg` + `addi` for `10 - x`. No `loadk` for any of these.

**`immediate_fix.tapir`:**

```tapir
# optimise
property x: fix;
x = x + 1.0;
x = x + 0.5;
x = x - 0.25;
```

Expected: `fixaddi` for 1.0 (raw 256, shifted encoding imm=16), `addi` for 0.5 (raw 128, direct encoding), `subi` for 0.25 (raw 64, direct encoding).

**`immediate_cross_encoding.tapir`:**

```tapir
# optimise
property x: int;
x = x + 256;
x = x + 4080;
x = x + 257;
```

Expected: `fixaddi` for 256 (shifted encoding, 256/16=16), `fixaddi` for 4080 (shifted, 4080/16=255), `loadk + add` for 257 (doesn't fit either encoding).

**`immediate_no_fit.tapir`:**

```tapir
# optimise
property x: int;
x = x + 4081;
x = x + 1000;
```

Expected: Still uses `loadk` + `add` (constants don't fit either encoding).

### IR Optimisation Snapshot Tests

Add test files in `compiler/src/compile/ir/snapshot_tests/optimisations/`:

**`immediate_arithmetic/basic.tapir`:**

```tapir
# constant_folding, immediate_arithmetic, dead_store_elimination
property x: int;
x = x + 1;
x = x - 5;
```

**`immediate_arithmetic/commutative.tapir`:**

```tapir
# constant_folding, immediate_arithmetic, dead_store_elimination
property x: int;
x = 1 + x;
x = 2 * x;
```

Expected: Constants normalised to RHS, then converted to immediates.

**`immediate_arithmetic/negative.tapir`:**

```tapir
# constant_folding, immediate_arithmetic, dead_store_elimination
property x: int;
x = x + (-1);
x = x - (-5);
```

Expected: Normalised to `x - 1` and `x + 5` with immediates.

**`immediate_arithmetic/const_lhs_sub.tapir`:**

```tapir
# constant_folding, immediate_arithmetic, dead_store_elimination
property x: int;
x = 10 - x;
x = 256 - x;
```

Expected: Each becomes `neg tmp, x` then `add target, tmp, imm`. 10 uses direct encoding, 256 uses shifted encoding.

**`immediate_arithmetic/fix.tapir`:**

```tapir
# constant_folding, immediate_arithmetic, dead_store_elimination
property x: fix;
x = x + 0.5;
x = x + 1.0;
```

Expected: `imm(128)` for 0.5 (raw 128 fits direct) and `simm(16)` for 1.0 (raw 256, shifted encoding 256/16=16).

### Runtime Verification

These can be tested via existing VM test infrastructure:

```tapir
# Verify int immediate arithmetic produces correct results
# x is a property so getprop produces an unknown value that can't be constant-folded
property x: int;
x = 10;
wait;
x = x + 5;      # addi: should be 15
wait;
x = x - 3;      # subi: should be 12
wait;
x = x * 2;      # muli: should be 24
# x should be 24
```

```tapir
# Verify fix immediate arithmetic produces correct results
property x: fix;
x = 1.0;
wait;
x = x + 0.5;    # addi (direct, raw 128): should be 1.5
wait;
x = x - 0.25;   # subi (direct, raw 64): should be 1.25
# x should be 1.25
```

### Edge Cases to Test

- `x + 0` — should be caught by constant folding as identity, NOT converted to immediate
- `x * 0` — should be caught by constant folding as zero, NOT converted to immediate
- `x * 1` — should be caught by constant folding as identity
- `x + 255` — largest direct immediate (int)
- `x + 256` — uses shifted encoding (256/16=16), NOT loadk
- `x + 257` — doesn't fit either encoding, falls back to `loadk + add`
- `x + 4080` — largest shifted immediate (4080/16=255)
- `x + 4081` — doesn't fit, falls back to `loadk + add`
- `x + 0.5` (fix, raw 128) — direct encoding, `addi` with imm=128
- `x + 1.0` (fix, raw 256) — shifted encoding, `fixaddi` with imm=16
- `x + 15.9375` (fix, raw 4080) — shifted encoding, `fixaddi` with imm=255
- `x + 16.0` (fix, raw 4096) — doesn't fit shifted encoding (4096/16=256>255), falls back to `loadk + add`
- `x + 0.015625` (fix, raw 4) — direct encoding, `addi` with imm=4
- Fix value where raw is not a multiple of 16 and > 255 — doesn't fit either encoding
- Division by immediate 0 — should never happen (constant folding warns first)

## Future Optimisation: Constant Reassociation

Expressions like `x + 10 + 10` currently compile to `addi(addi(x, 10), 10)` (2 instruction words). Ideally this would be `addi(x, 20)` (1 word). This requires a **reassociation** pass that rewrites `(x + c1) + c2` → `x + (c1 + c2)` by folding the two constants together. This is orthogonal to immediate arithmetic — it would also benefit non-immediate cases by eliminating a `loadk` — but immediates make it more visible since the result is so compact.

The same principle applies to other associative ops: `(x * 2) * 3` → `x * 6`, etc.

## Implementation Notes

This section will document differences between the plan and actual implementation.
