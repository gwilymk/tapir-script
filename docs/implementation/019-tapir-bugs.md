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

### 8. Global Struct Field Assignment in Functions

**Status:** OPEN

**Severity:** Medium (silent incorrect behavior)

**Description:** Individual field assignment to global structs inside functions doesn't work. Whole struct assignment works fine. This is NOT spawn-specific - it fails with regular function calls too.

**What works:**
```tapir
global g: int2;

fn set_whole_struct(x: int, y: int) {
    g = int2(x, y);  # Whole struct assignment works
}

set_whole_struct(100, 200);
assert(g.x == 100);  # Passes
```

**What fails:**
```tapir
global g: int2;

fn set_struct_fields(x: int, y: int) {
    g.x = x;  # Individual field assignment fails
    g.y = y;
}

set_struct_fields(100, 200);
assert(g.x == 100);  # Fails - g.x is still 0
```

**Workaround:** Use whole struct assignment instead of individual field assignment:
```tapir
g = int2(x, y);  # Instead of g.x = x; g.y = y;
```

**Test files:**
- `bug8_spawn_globals.tapir` (tests with spawn)
- `bug8_test_nonspawn.tapir` (tests without spawn - same bug)

---

## Test Files

Bug-specific test files:
- `bug1_nested_structs.tapir` - Tests nested user-defined structs (FIXED)
- `bug2_global_struct_assignment.tapir` - Tests global struct assignment (FIXED)
- `bug3_global_struct_constant.tapir` - Tests struct constant initialization (FIXED)
- `bug4_floor_division_negative.tapir` - Tests floor division with negative divisors (OPEN)
- `bug6_sqrt_nonperfect.tapir` - Tests sqrt precision (OPEN)
- `bug7_comparison_optimizer.tapir` - Tests comparison operators in functions (FIXED)
- `bug8_spawn_globals.tapir` - Tests globals in spawned tasks (PARTIAL)

Other test files:
- `division_operators.tapir` - Tests floor vs truncating division
- `modulo_operators.tapir` - Tests Euclidean vs truncating modulo (FIXED)
- `task_handles.tapir` - Uses globals in spawned tasks

---

## Recommendations

1. **Priority 1:** Fix floor division with negative divisors - decide on semantics and fix implementation
2. **Priority 2:** Fix sqrt() for non-perfect squares - this affects any game using distance calculations
3. **Priority 3:** Fix global struct field assignment in functions - this limits struct usability
