# Bytecode Reference

Tapir-script compiles to bytecode that runs on a register-based virtual machine (similar to Lua).

## Instruction Format

All instructions are exactly 32 bits. The first 8 bits are always the opcode. There are three instruction formats:

### Type 1 (most instructions)

| Bits 31-24 | Bits 23-16 | Bits 15-8 | Bits 7-0 |
| ---------- | ---------- | --------- | -------- |
| b          | a          | target    | opcode   |

### Type 2 (immediate value instructions)

| Bits 31-16     | Bits 15-8 | Bits 7-0 |
| -------------- | --------- | -------- |
| imm (signed 16-bit) | reg  | opcode   |

### Type 3 (jumps)

| Bits 31-8        | Bits 7-0 |
| ---------------- | -------- |
| value (24-bit)   | opcode   |

## Registers

The VM uses a register-based model with a growable stack. Registers are addressed relative to a stack offset that changes during function calls.

- `r0` is special during calls (stores return address + stack offset)
- Arguments are passed in `r1`, `r2`, `r3`, etc.
- Return values are placed in `r1`, `r2`, etc.

## Opcodes

### Data Movement

| Opcode         | Format | Description                                                   |
| -------------- | ------ | ------------------------------------------------------------- |
| `Mov`          | Type 1 | `r[target] = r[a]`                                            |
| `LoadConstant` | Type 1 | `r[target] = bytecode[pc++]` (next word is a 32-bit constant) |
| `LoadI`        | Type 2 | `r[reg] = imm` (sign-extended 16-bit immediate)               |

### Arithmetic (Type 1: `r[target] = r[a] op r[b]`)

| Opcode    | Operation                                        |
| --------- | ------------------------------------------------ |
| `Add`     | `a + b`                                          |
| `Sub`     | `a - b`                                          |
| `Mul`     | `a * b`                                          |
| `Div`     | `a.div_euclid(b)` (Euclidean division)           |
| `Mod`     | `a.rem_euclid(b)` (Euclidean modulo)             |
| `RealDiv` | `a / b` (truncating division)                    |
| `RealMod` | `a % b` (truncating modulo)                      |
| `FixMul`  | Fixed-point multiply                             |
| `FixDiv`  | Fixed-point divide                               |

### Bitwise (Type 1: `r[target] = r[a] op r[b]`)

| Opcode   | Operation                            |
| -------- | ------------------------------------ |
| `Shl`    | `a << (b & 31)` (left shift)         |
| `Shr`    | `a >> (b & 31)` (right shift signed) |
| `BitAnd` | `a & b`                              |
| `BitOr`  | `a \| b`                             |

Note: Shift amounts are masked to 0-31 to match Rust's behavior.

### Immediate Arithmetic (Type 1: `r[target] = r[a] op b`)

These use the Type 1 format but the `b` field is an unsigned 8-bit immediate value (0–255) rather than a register index.

| Opcode   | Operation                                               |
| -------- | ------------------------------------------------------- |
| `AddI`   | `a + b`                                                 |
| `SubI`   | `a - b`                                                 |
| `MulI`   | `a * b`                                                 |
| `DivI`   | `a.div_euclid(b)` (Euclidean division)                  |
| `FixAddI`| `a + (b << 4)` (fixed-point add with integer immediate) |
| `FixSubI`| `a - (b << 4)` (fixed-point sub with integer immediate) |
| `FixMulI`| `(a * b) >> 4` (fixed-point multiply)                   |
| `FixDivI`| `(a << 4) / b` (fixed-point divide)                     |
| `ShlI`   | `a << (b & 31)` (left shift)                            |
| `ShrI`   | `a >> (b & 31)` (right shift signed)                    |

### Comparison (Type 1: `r[target] = (r[a] op r[b]) ? 1 : 0`)

| Opcode | Operation |
| ------ | --------- |
| `EqEq` | `a == b`  |
| `NeEq` | `a != b`  |
| `Gt`   | `a > b`   |
| `GtEq` | `a >= b`  |
| `Lt`   | `a < b`   |
| `LtEq` | `a <= b`  |

### Properties

| Opcode     | Format | Description                             |
| ---------- | ------ | --------------------------------------- |
| `GetProp`  | Type 1 | `r[target] = property[a]`               |
| `SetProp`  | Type 1 | `property[a] = r[target]`               |
| `SetPropI` | Type 2 | `property[reg] = imm` (16-bit immediate)|

### Globals

| Opcode       | Format | Description                               |
| ------------ | ------ | ----------------------------------------- |
| `GetGlobal`  | Type 1 | `r[target] = global[a]`                   |
| `SetGlobal`  | Type 1 | `global[a] = r[target]`                   |
| `SetGlobalI` | Type 2 | `global[reg] = imm` (16-bit immediate)    |

Global variables are script-defined mutable values with constant initializers. They are stored separately from the bytecode and initialized when the VM is created. Unlike properties, globals are internal to the script and not accessible from Rust.

### Built-ins (Type 1)

| Opcode       | Description               |
| ------------ | ------------------------- |
| `GetBuiltin` | `r[target] = builtin[a]`  |

Built-in variables are runtime-provided values accessed by index:

| Index | Name    | Description                                                                 |
| ----- | ------- | --------------------------------------------------------------------------- |
| 0     | `frame` | Global frame counter, starts at 0 and increments at the end of each `run()` |

### Control Flow

| Opcode   | Format | Description                                            |
| -------- | ------ | ------------------------------------------------------ |
| `Jump`   | Type 3 | `pc = value`                                           |
| `JumpIf` | Type 1 | If `r[target] == 0`, skip next instruction (`pc += 1`) |
| `Wait`   | Type 1 | Yield execution, return `Waiting`                      |
| `Ret`    | Type 1 | Return from function (see calling convention)          |

### Function Calls (Type 1)

| Opcode        | Description                                                                      |
| ------------- | -------------------------------------------------------------------------------- |
| `Call`        | Internal function call, `target` = first arg register                            |
| `ExternCall`  | External (Rust) function call, `target` = extern ID, `a` = first arg register    |
| `Spawn`       | Start concurrent thread, stores task ID in `target`, `a` = first arg, `b` = args |
| `Trigger`     | Fire event to Rust, `target` = trigger ID, `a` = first arg register              |
| `CallBuiltin` | Call builtin function, `target` = result, `a` = builtin ID, `b` = first arg      |

## Calling Convention

### Internal Calls (`Call` + `Jump`)

To call a function at address `ADDR` with 3 arguments:

1. Copy arguments to consecutive registers starting at `r[N+1]` (e.g., `r8`, `r9`, `r10`)
2. Execute `Call N` (where N is the first arg register minus 1, e.g., 7)
3. Next instruction must be `Jump ADDR`

After `Call`:

- `r0 = (N << 24) | return_address` (return address is PC after the Call instruction)
- Stack offset increases by N
- Arguments are now accessible as `r1`, `r2`, `r3`

### Returning (`Ret`)

1. Place return values in `r1`, `r2`, etc.
2. Execute `Ret`

The `Ret` instruction:

- Reads `r0` to get stack offset and return address
- Restores stack offset
- Jumps to return address + 1 (skipping the original `Jump`)

If `r0 == -1` (0xFFFFFFFF), the thread terminates.

### Spawn

Creates a new execution thread and returns a task ID:

- `target` = register to store the task ID (non-zero integer)
- `a` = first argument register
- `b` = number of arguments
- Next word (32-bit) = function PC

Execution:
1. Read function PC from the next bytecode word
2. Assign a unique task ID (starts at 1, increments monotonically)
3. Store task ID in `r[target]`
4. Copy arguments to new thread's stack
5. New thread starts at function PC, original continues at PC+1

Task IDs can be used with `task.cancel()` (builtin -2) to stop the spawned function.

### External Calls (`ExternCall`)

- `target` = external function ID (index into extern fn declarations)
- `a` = first argument register
- Passes the stack and first arg index to the Rust implementation

## Thread Model

The VM maintains multiple execution states (threads). Each `run()` call:

1. Executes each thread until it hits `Wait`, `Ret` (with r0=-1), or spawns
2. Threads that `Wait` are kept for the next `run()`
3. Threads that finish are removed
4. Spawned threads are added to the list

## Example: Simple Function Call

```
# fn add(a: int, b: int) -> int { return a + b; }
# var x = add(3, 5);

0: LoadI r1, 3          # r1 = 3
1: LoadI r2, 5          # r2 = 5
2: Call 0               # prepare call, r0 = return info
3: Jump 5               # jump to function
4: Mov r1, r1           # (return value already in r1)
...
5: Add r1, r1, r2       # function body: r1 = r1 + r2
6: Ret                  # return
```
