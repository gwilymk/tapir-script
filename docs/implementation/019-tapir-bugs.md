# Tapir-Script Bugs Found During Integration Testing

This document catalogs bugs discovered while creating comprehensive integration tests for the tapir-script compiler and VM.

---

## FIXED BUGS

### 1. Nested User-Defined Structs Crash the Compiler

**Status:** FIXED

**Original Description:** When a user-defined struct contains a field of another user-defined struct type, the compiler paniced with "Target should have matching expansion".

**Reproduction:**
```tapir
struct Point { x: int, y: int }
struct Rectangle { origin: Point, size: Point }

var r = Rectangle(Point(0, 0), Point(100, 50));
assert(r.origin.x == 0);
```

**Test file:** `bug1_nested_structs.tapir`

---

### 2. Global Struct Assignment Gets Optimized Away

**Status:** FIXED

**Original Description:** When assigning to a global variable of struct type, the optimizer incorrectly eliminated the assignment, leaving the global unmodified.

**Reproduction:**
```tapir
global g: int2;
g = int2(10, 20);
assert(g.x == 10);  # Previously failed - g.x was still 0
```

**Test file:** `bug2_global_struct_assignment.tapir`

---

### 3. Struct Constructors Are Not Constant Expressions

**Status:** FIXED

**Original Description:** Global variables could not be initialized with struct constructors because they were not treated as constant expressions.

**Reproduction:**
```tapir
global pos = int2(10, 20);  # Previously errored: "not a constant expression"
```

**Test file:** `bug3_global_struct_constant.tapir`

---

### 5. Euclidean Modulo vs Truncating Modulo

**Status:** FIXED

**Original Description:** The `%` operator (Euclidean modulo) was behaving identically to `%%` (truncating modulo) due to AST lowering collapsing both operators to the same IR operation.

**Root Cause:** AST conversion `(B::Mod, Type::Int) => *self = B::RealMod` was incorrectly converting Euclidean modulo to truncating modulo, and there was no separate `Mod` opcode in the bytecode.

**Fix:** Added proper `Mod` opcode and removed the incorrect AST conversion.

**Test file:** `modulo_operators.tapir`

---

### 7. Comparison Operators in Function If-Conditions (Potential Optimizer Bug)

**Status:** FIXED (or never was a bug)

**Original Description:** During testing, there were indications that `>`, `>=`, `==`, and `!=` operators in if-conditions inside functions may cause the optimizer to incorrectly eliminate code.

**Current Status:** Comprehensive testing shows all comparison operators work correctly in function if-conditions.

**Test file:** `bug7_comparison_optimizer.tapir`

---

### 8. Global Struct Field Assignment in Functions

**Status:** FIXED

**Original Description:** Individual field assignment to global structs inside functions didn't work. Whole struct assignment worked fine. This was NOT spawn-specific - it failed with regular function calls too.

**Root Cause:** The IR lowering phase didn't handle global struct field assignments. The type_table used a Vec which couldn't handle GlobalId-based symbols (which use large values with a bit flag), and the assignment code path had no special handling for global struct bases.

**Fix:** Changed type_table to HashMap and added global struct field assignment handling in `lowering.rs`, similar to how property base field assignments were already handled.

**Test files:** Tests merged into `globals_advanced.tapir` and `spawn_globals_interaction.tapir`

---

## OPEN BUGS

### 4. Floor Division Bug with Negative Divisors

**Status:** OPEN

**Severity:** Medium (incorrect behavior)

**Description:** The `/` operator is documented as floor division but does not correctly floor when the divisor is negative.

**Reproduction:**
```tapir
assert(7 / -4 == -2);  # Fails - returns -1 instead of -2
```

**Expected:** Floor division should round toward negative infinity: `7 / -4 = -1.75` floors to `-2`

**Actual:** Returns `-1` (truncation toward zero)

**Workaround:** Avoid negative divisors, or use `//` (truncating division) if that's the intended behavior.

**Test file:** `bug4_floor_division_negative.tapir` (failing assertions commented out)

---

### 6. sqrt() Returns Incorrect Values for Non-Perfect Squares

**Status:** OPEN

**Severity:** Medium (incorrect behavior)

**Description:** The `sqrt()` function returns incorrect results for values that aren't perfect squares.

**Reproduction:**
```tapir
var s = sqrt(2.0);
assert(s > 1.41 && s < 1.42);  # Fails - s is ~1.375
```

**Expected:** `sqrt(2.0) ≈ 1.414`

**Actual:** Returns `~1.375`

**Note:** Perfect squares work correctly: `sqrt(4.0) == 2.0`, `sqrt(9.0) == 3.0`

**Root Cause:** Likely due to limited precision in fixed-point arithmetic implementation.

**Test file:** `bug6_sqrt_nonperfect.tapir` (failing assertions commented out)

---

## Test Files

Bug-specific test files:
- `bug1_nested_structs.tapir` - Tests nested user-defined structs (FIXED)
- `bug2_global_struct_assignment.tapir` - Tests global struct assignment (FIXED)
- `bug3_global_struct_constant.tapir` - Tests struct constant initialization (FIXED)
- `bug4_floor_division_negative.tapir` - Tests floor division with negative divisors (OPEN)
- `bug6_sqrt_nonperfect.tapir` - Tests sqrt precision (OPEN)
- `bug7_comparison_optimizer.tapir` - Tests comparison operators in functions (FIXED)

Other test files:
- `division_operators.tapir` - Tests floor vs truncating division
- `modulo_operators.tapir` - Tests Euclidean vs truncating modulo (FIXED)
- `globals_advanced.tapir` - Includes global struct field assignment tests (bug #8 fix)
- `spawn_globals_interaction.tapir` - Includes spawned global struct field tests (bug #8 fix)

---

## Recommendations

1. **Priority 1:** Fix floor division with negative divisors - decide on semantics and fix implementation
2. **Priority 2:** Fix sqrt() for non-perfect squares - this affects any game using distance calculations
