# Bytecode concept attempt 2

Register based VM (similar to lua)

Bytecode instructions are exactly 32-bits in size. 3 variants. Always starts with a 8 bit opcode.

The VM ensures that there is always enough stack

## Type 1:

| 8        | 8      | 8       | 8   | meaning                                      |
| -------- | ------ | ------- | --- | -------------------------------------------- |
| opcode   | target | a       | b   |                                              |
|          |        |         |     |                                              |
| binop    | r0     | r1      | r2  | r0 = r1 binop r2                             |
|          |        |         |     |                                              |
| get_prop | r0     | prop_id |     | r0 = prop(prop_id)                           |
| set_prop | r0     | prop_id |     | prop(prop_id) = r0                           |
|          |        |         |     |                                              |
| call     | arg0   |         |     | prepares for a function call                 |
| spawn    | arg0   |         |     | same as `call` but starts a new thread       |
| trigger  | id     | arg0    |     | fires trigger with given id and starting arg |
|          |        |         |     |                                              |
| jump_if  | target |         |     | skips the next instruction if target != 0    |
| ret      |        |         |     | returns (see function call below)            |

...

## Type 2:

| 8      | 8      | 16    |                         |
| ------ | ------ | ----- | ----------------------- |
| opcode | target | value | meaning                 |
|        |        |       |                         |
| load_k | r0     | value | r0 = sign_extend(value) |

## Type 3:

| 8      | 24     |                                |
| ------ | ------ | ------------------------------ |
| opcode | value  | meaning                        |
|        |        |                                |
| jump   | target | unconditionally jump to target |

# Function calls

Function calls work by putting at some register offset all the arguments in order.
Suppose registers from 7 are free and you want to call a function located at 12345 with 3 arguments.

Firstly, copy argument 1 to _r8_, argument 2 to r9, argument 3 to r10.
Then use the `call 7` instruction. The following instruction must be `jump 12345`.

After the `call` instruction, `r0` is set to `the argument to call << 24 | return address` where the return address is 1 past the `call` instruction.
`r1` is the first argument, `r2` is the second argument and `r3` is the third argument.

When ready to return, copy the first return value to `r1`, the second to `r2` etc, then use the `ret` instruction.
This will look at the value of `r0`, and push the stack back the correct amount to return the registers to how they were before the call and jump to the return address.

If `r0` is u32::MAX, then this thread terminates.
