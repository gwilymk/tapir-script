# Bytecode Reference

Tapir-script compiles to bytecode that runs on a register-based virtual machine (similar to Lua).

## Instruction Format

All instructions are exactly 32 bits. The first 8 bits are always the opcode. There are two instruction formats:

### Type 1 (most instructions)

| Bits 31-24 | Bits 23-16 | Bits 15-8 | Bits 7-0 |
| ---------- | ---------- | --------- | -------- |
| opcode     | target     | a         | b        |

### Type 3 (jumps)

| Bits 31-24 | Bits 23-0      |
| ---------- | -------------- |
| opcode     | value (24-bit) |

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

### Arithmetic (Type 1: `r[target] = r[a] op r[b]`)

| Opcode    | Operation                  |
| --------- | -------------------------- |
| `Add`     | `a + b`                    |
| `Sub`     | `a - b`                    |
| `Mul`     | `a * b`                    |
| `RealDiv` | `a / b` (integer division) |
| `RealMod` | `a.rem_euclid(b)`          |
| `FixMul`  | Fixed-point multiply       |
| `FixDiv`  | Fixed-point divide         |

### Comparison (Type 1: `r[target] = (r[a] op r[b]) ? 1 : 0`)

| Opcode | Operation |
| ------ | --------- |
| `EqEq` | `a == b`  |
| `NeEq` | `a != b`  |
| `Gt`   | `a > b`   |
| `GtEq` | `a >= b`  |
| `Lt`   | `a < b`   |
| `LtEq` | `a <= b`  |

### Properties (Type 1)

| Opcode    | Description               |
| --------- | ------------------------- |
| `GetProp` | `r[target] = property[a]` |
| `SetProp` | `property[a] = r[target]` |

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

| Opcode       | Description                                                                   |
| ------------ | ----------------------------------------------------------------------------- |
| `Call`       | Internal function call, `target` = first arg register                         |
| `ExternCall` | External (Rust) function call, `target` = extern ID, `a` = first arg register |
| `Spawn`      | Start concurrent thread, `target` = first arg register, `a` = num args        |
| `Trigger`    | Fire event to Rust, `target` = trigger ID, `a` = first arg register           |

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

Similar to `Call`, but:

- Creates a new execution thread
- Copies arguments to the new thread's stack
- Both threads continue executing (original skips the `Jump`, new thread follows it)

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

0: LoadConstant r1      # r1 = 3
1: 3
2: LoadConstant r2      # r2 = 5
3: 5
4: Call 0               # prepare call, r0 = return info
5: Jump 7               # jump to function
6: Mov r1, r1           # (return value already in r1)
...
7: Add r1, r1, r2       # function body: r1 = r1 + r2
8: Ret                  # return
```
