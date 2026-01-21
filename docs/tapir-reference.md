# Tapir Script Language Reference

Tapir-script is a scripting language for defining game animations asynchronously. Scripts run frame-by-frame using `wait` to yield control, making it easy to express animations, cutscenes, and state machines that span multiple frames.

```tapir
# Animate a character walking right over 10 frames
var i = 0;
loop {
    if i >= 10 {
        break;
    }
    position = position + 1;
    i = i + 1;
    wait;  # yield until next frame
}
```

## Comments

```tapir
# This is a comment (extends to end of line)
```

## Types

| Type   | Description                              |
| ------ | ---------------------------------------- |
| `int`  | 32-bit signed integer                    |
| `fix`  | 24.8 fixed-point number (via agb_fixnum) |
| `bool` | Boolean (`true` or `false`)              |

## Literals

```tapir
42       # int
-17      # negative int
3.14     # fix (must have decimal point)
true     # bool
false    # bool
```

## Variables

```tapir
var x = 5;              # declaration with initialization
var a, b = 1, 2;        # multiple declaration
x = 10;                 # assignment
a, b = b, a;            # swap
```

Variables are block-scoped. Type is inferred from the initializer.

## Global Variables

Global variables are declared at the top level with constant initializers:

```tapir
global counter = 0;
global max_health = 100;
global speed = 1.5;
global is_active = true;
```

Globals differ from local variables:

- Accessible from all functions and top-level code
- Persist across `wait` calls and function boundaries
- Can be shadowed by local variables with the same name
- Initialized once when the script is created

```tapir
global counter = 0;

fn increment() {
    counter = counter + 1;  # modifies global
}

fn use_local() {
    var counter = 99;       # shadows global
    counter = counter + 1;  # modifies local (100)
}

increment();                # counter is now 1
use_local();                # global counter unchanged
increment();                # counter is now 2
```

Globals are useful for state shared between spawned functions:

```tapir
global shared_counter = 0;

fn worker() {
    loop {
        shared_counter = shared_counter + 1;
        wait;
    }
}

spawn worker();
spawn worker();
# Both workers increment the same counter
```

Unlike properties, globals are internal to the script and not accessible from Rust.

## Operators

### Arithmetic (work on `int` and `fix`)

| Op   | Description                   |
| ---- | ----------------------------- |
| `+`  | Addition                      |
| `-`  | Subtraction                   |
| `*`  | Multiplication (fix-aware)    |
| `/`  | Division (truncating for int) |
| `%`  | Modulo (truncating for int)   |
| `//` | Real division                 |
| `%%` | Real modulo                   |

### Comparison (work on `int` and `fix`, return `bool`)

`==`, `!=`, `<`, `<=`, `>`, `>=`

Comparison operators are non-associative: `a < b < c` is invalid.

### Logical (work on `bool`, return `bool`)

`&&` (and), `||` (or)

Short-circuit evaluation: `&&` and `||` only evaluate the right operand if necessary.

### Special

`then` - Evaluates left side, discards result, returns right side:

```tapir
var x = foo() then 42;  # calls foo(), returns 42
```

### Precedence (lowest to highest)

1. `then`
2. `||`
3. `&&`
4. Comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`)
5. Addition (`+`, `-`)
6. Multiplication (`*`, `/`, `%`, `//`, `%%`)

## Control Flow

### If/Else

```tapir
if condition {
    # ...
}

if condition {
    # ...
} else {
    # ...
}
```

Condition must be `bool`.

### Loop

```tapir
loop {
    if done {
        break;
    }
    continue;  # skip to next iteration
}
```

`break` and `continue` must be inside a loop.

### Blocks

```tapir
{
    var scoped = 1;  # only visible in this block
}
```

## Functions

```tapir
fn add(a: int, b: int) -> int {
    return a + b;
}

fn swap(a: int, b: int) -> (int, int) {
    return b, a;
}

fn side_effect() {
    # no return type means void
}
```

### Calling Functions

```tapir
var result = add(1, 2);
var x, y = swap(1, 2);      # unpack multiple returns
side_effect();              # call as statement
```

## External Functions

Declare functions implemented in Rust:

```tapir
extern fn get_random() -> int;
extern fn set_value(x: int);
extern fn compute(a: int, b: fix) -> fix;
```

Called the same way as regular functions.

## Properties

Properties are variables shared between the script and Rust host. They must be declared at the top level:

```tapir
property health: int;
property position: fix;
property alive: bool;
```

Properties persist across frames and can be read/written from both script and Rust:

```tapir
property counter: int;

counter = counter + 1;      # set property
var x = counter;            # read property
```

Each declared property must have a corresponding field in the Rust `#[derive(TapirScript)]` struct with a compatible type.

## Event Handlers

Event handlers are functions that can be triggered externally:

```tapir
event fn on_collision(damage: int) {
    health = health - damage;
}
```

Event handlers cannot have return values and cannot be called directly from script.

## Concurrency

### Wait

Yields execution until the next frame. This is the core primitive for frame-based animation:

```tapir
# Move through 3 positions over 3 frames
position = 0;
wait;
position = 1;
wait;
position = 2;
```

Each `wait` returns control to the game loop. Call `script.run()` each frame to advance.

### Spawn

Starts a function as a concurrent animation thread. Multiple spawned functions run in parallel, all advancing on each `run()` call:

```tapir
fn fade_in() {
    var alpha = 0;
    loop {
        if alpha >= 255 { break; }
        opacity = alpha;
        alpha = alpha + 5;
        wait;
    }
}

fn slide_in() {
    var x = -100;
    loop {
        if x >= 0 { break; }
        position_x = x;
        x = x + 10;
        wait;
    }
}

# Both animations run simultaneously
spawn fade_in();
spawn slide_in();
```

### Trigger

Fires a named trigger that can be handled by the Rust host:

```tapir
trigger PlayerDied;
trigger DamageDealt(50);
trigger Position(x, y);
```

## Top-Level Code

Code outside functions runs when the script starts:

```tapir
# This runs on script start
counter = 0;
wait;
counter = 1;

fn helper() -> int {
    return counter;
}
```

## Identifiers

Must start with letter or underscore, followed by letters, digits, or underscores:

```
valid_name
_private
counter2
```

## Reserved Words

```
break continue else event extern false fn global if int fix bool
loop property return spawn then trigger true var wait
```

## Rust Integration

### Basic Setup

Use the `#[derive(TapirScript)]` macro on a struct to create a script host. The struct fields must match the property declarations in the .tapir file:

```tapir
# script.tapir
property health: int;
property position: fix;
property alive: bool;

# ... script code ...
```

```rust
use tapir_script::TapirScript;

#[derive(TapirScript)]
#[tapir("path/to/script.tapir")]
struct MyScript {
    health: i32,      // matches property health: int
    position: Fix,    // matches property position: fix
    alive: bool,      // matches property alive: bool
}
```

### Running Scripts

Call `run()` once per frame in your game loop. Each call executes until the script hits `wait` or completes:

```rust
let mut script = MyScript { health: 100, position: num!(0.0), alive: true }.script();

// In your game loop:
loop {
    // Advance script by one frame
    script.run();

    // Read animated values
    render_at(script.properties.position);

    // Check if animation is complete
    if !script.will_calling_run_do_anything() {
        break;
    }
}
```

### Rust-Only Fields

Fields without a corresponding property declaration in the .tapir file are simply not accessible from the script:

```rust
#[derive(TapirScript)]
#[tapir("script.tapir")]  // declares: property health: int;
struct MyScript {
    health: i32,          // accessible from script
    internal_state: i32,  // not declared in .tapir, not accessible
}
```

This allows you to have Rust-only fields for internal state that the script cannot see or modify.

### External Functions

Declare in script:

```tapir
property result: int;

extern fn add_numbers(a: int, b: int) -> int;
extern fn side_effect(value: int);

result = add_numbers(3, 5);
side_effect(result);
```

Implement as methods on the struct:

```rust
#[derive(TapirScript)]
#[tapir("script.tapir")]
struct MyScript {
    result: i32,
}

impl MyScript {
    fn add_numbers(&mut self, a: i32, b: i32) -> i32 {
        a + b
    }

    fn side_effect(&mut self, value: i32) {
        println!("Side effect: {}", value);
    }
}
```

Method names must match the extern function names (snake_case). Methods take `&mut self` and can access/modify struct fields.

### Event Handlers

Define in script:

```tapir
event fn damage(num_flashes: int) {
    var i = 0;
    loop {
        if i >= num_flashes { break; }
        visible = false;
        wait;
        wait;
        visible = true;
        wait;
        wait;
        i = i + 1;
    }
}
```

Call from Rust (method is auto-generated with `on_` prefix):

```rust
let mut script = MyScript { visible: true }.script();
script.on_damage(3);  // Start flashing animation
// Animation runs over subsequent run() calls
```

### Triggers

Define a trigger enum and associate it with the script:

```rust
#[derive(TapirScript)]
#[tapir("script.tapir", trigger_type = GameEvent)]
struct MyScript;

#[derive(Debug, PartialEq)]
enum GameEvent {
    PlayerDied,
    DamageDealt(i32),
    Position(i32, i32),
}
```

Script triggers fire events:

```tapir
trigger PlayerDied;
trigger DamageDealt(50);
trigger Position(x, y);
```

Collect triggers from `run()`:

```rust
let mut script = MyScript.script();
let events: Vec<GameEvent> = script.run();

for event in events {
    match event {
        GameEvent::PlayerDied => game_over(),
        GameEvent::DamageDealt(dmg) => apply_damage(dmg),
        GameEvent::Position(x, y) => update_pos(x, y),
    }
}
```

Enum variant names must match trigger names. Variant fields correspond to trigger arguments.

### Type Mappings

| Script | Rust                                   |
| ------ | -------------------------------------- |
| `int`  | `i32`                                  |
| `fix`  | `tapir_script::Fix` (24.8 fixed-point) |
| `bool` | `bool`                                 |

Use `agb_fixnum::num!()` macro for fix literals: `num!(3.5)`
