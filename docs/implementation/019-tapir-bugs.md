# Tapir-Script Bugs Found During Integration Testing

This document catalogs bugs discovered while creating comprehensive integration tests for the tapir-script compiler and VM.

## 1. Nested User-Defined Structs Crash the Compiler

**Severity:** High (compiler crash)

**Description:** When a user-defined struct contains a field of another user-defined struct type, the compiler panics with "Target should have matching expansion".

**Reproduction:**
```tapir
struct Point { x: int, y: int }
struct Rectangle { origin: Point, size: Point }

var r = Rectangle(Point(0, 0), Point(100, 50));
assert(r.origin.x == 0);
```

**Error:**
```
thread 'test::assert_tests' panicked at compiler/src/compile/ir/lowering.rs:1025:22:
Target should have matching expansion
```

**Workaround:** Use prelude structs (int2, fix2) instead, or avoid nesting user-defined structs.

---

## 2. Global Struct Assignment Gets Optimized Away

**Severity:** High (silent incorrect behavior)

**Description:** When assigning to a global variable of struct type, the optimizer incorrectly eliminates the assignment, leaving the global unmodified.

**Reproduction:**
```tapir
global g: int2;
g = int2(10, 20);
assert(g.x == 10);  # Fails - g.x is still 0
```

**Workaround:** Avoid global struct variables, or use separate int globals for each field.

---

## 3. Struct Constructors Are Not Constant Expressions

**Severity:** Medium (compile error)

**Description:** Global variables cannot be initialized with struct constructors because they are not treated as constant expressions.

**Reproduction:**
```tapir
global pos = int2(10, 20);  # Error: not a constant expression
```

**Workaround:** Use type-only declaration and assign in code:
```tapir
global pos: int2;
pos = int2(10, 20);
```

---

## 4. Floor Division Bug with Negative Divisors

**Severity:** Medium (incorrect behavior)

**Description:** The `/` operator is documented as floor division but does not correctly floor when the divisor is negative.

**Reproduction:**
```tapir
assert(7 / -4 == -2);  # Fails - returns -1 instead of -2
```

**Expected:** Floor division should round toward negative infinity: `7 / -4 = -1.75` floors to `-2`

**Actual:** Returns `-1` (truncation toward zero)

**Workaround:** Avoid negative divisors, or use `//` (truncating division) if that's the intended behavior.

---

## 5. Euclidean Modulo Behaves Like Truncating Modulo

**Severity:** Medium (incorrect behavior)

**Description:** The `%` operator is documented as Euclidean modulo but actually behaves identically to `%%` (truncating modulo).

**Reproduction:**
```tapir
# Euclidean modulo should always return non-negative
assert(-7 % 4 == 1);   # Fails - returns -3 instead of 1
assert(-7 %% 4 == -3); # Passes - truncating modulo
```

**Expected:** Euclidean modulo: `-7 % 4 = 1` (always non-negative result)

**Actual:** Both `%` and `%%` return `-3`

**Workaround:** Add divisor when result is negative: `((a % b) + b) % b`

---

## 6. sqrt() Returns Incorrect Values for Non-Perfect Squares

**Severity:** Medium (incorrect behavior)

**Description:** The `sqrt()` function returns incorrect results for values that aren't perfect squares.

**Reproduction:**
```tapir
var s = sqrt(2.0);
assert(s > 1.41 && s < 1.42);  # Fails - s is 1.375
```

**Expected:** `sqrt(2.0) ≈ 1.414`

**Actual:** Returns `1.375`

**Note:** Perfect squares work correctly: `sqrt(4.0) == 2.0`, `sqrt(9.0) == 3.0`

---

## 7. Comparison Operators in Function If-Conditions (Potential Optimizer Bug)

**Severity:** High (silent incorrect behavior) - *Needs verification*

**Description:** During testing, there were indications that `>`, `>=`, `==`, and `!=` operators in if-conditions inside functions may cause the optimizer to incorrectly eliminate code. Only `<` and `<=` appeared to work reliably.

**Reproduction:** (intermittent, needs isolation)
```tapir
fn check_greater(a: int, b: int) -> bool {
    if a > b {
        return true;
    }
    return false;
}
# Function body may get optimized away incorrectly
```

**Status:** This bug was observed during testing but needs further isolation to confirm. The tests were rewritten to use `<` and `<=` as a workaround.

---

## 8. Global Variable Assignment in Spawned Tasks

**Severity:** High (silent incorrect behavior)

**Description:** Global variable assignments inside spawned task functions may be optimized away or not take effect correctly.

**Reproduction:**
```tapir
global result = 0;

fn set_result(v: int) {
    result = v;
}

spawn set_result(42);
wait;
assert(result == 42);  # May fail
```

**Status:** Needs further investigation. Simplified spawn tests work, but complex interactions with globals fail.

---

## Test Files Affected

The following test files were simplified or modified to work around these bugs:

- `structs.tapir` - Removed nested struct tests
- `struct_methods.tapir` - Removed methods returning different struct types
- `struct_assignment.tapir` - Uses only prelude structs
- `globals_advanced.tapir` - Removed global struct tests
- `spawn_complex_args.tapir` - Removed global struct argument tests
- `spawn_globals_interaction.tapir` - Heavily simplified
- `division_operators.tapir` - Removed negative divisor tests
- `modulo_operators.tapir` - Removed negative number tests
- `task_handles.tapir` - Left failing intentionally to verify fix later

---

## Recommendations

1. **Priority 1:** Fix the nested struct compiler crash - this prevents users from using a common language pattern
2. **Priority 2:** Fix the global struct assignment optimization - silent incorrect behavior is dangerous
3. **Priority 3:** Fix sqrt() for non-perfect squares
4. **Priority 4:** Decide on modulo/division semantics and either fix implementation or update documentation
5. **Priority 5:** Investigate and fix the comparison operator optimizer bug
