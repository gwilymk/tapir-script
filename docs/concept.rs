

#[tapir_script(
    "my_object.tapir",
    trigger EventKind,
)]
struct MyObjectProperties {
    x: fix,
    y: fix,
    counter: int,

    current_sprite: int,

    is_taking_damage: bool,

    #[tapir(skip)]
    something_else: Vec<MyObject>,
}

static POTENTIAL_RUST_CALLS = &[
    |properties: &mut MyObjectProperties, stack: &mut Stack| {
        let b = stack.pop();
        let a = stack.pop();

        let (ret0, ret1) = crate::my_cool_rust_fn(a, b);
        
        stack.push(ret0);
        stack.push(Fix::from_raw(ret1));
    }
];

static POTENTIAL_TRIGGER_EVENTS = [
    |stack: &mut Stack| {
        EventKind::Bar(stack.pop())
    }
];

impl ScriptGetSet for MyObjectProperties {
    fn get(&self, index: usize) -> i32 {
        match index {
            0 => self.x.to_raw(),
            1 => self.y.to_raw(),
            2 => self.counter,
            3 => self.current_sprite,
            4 => self.is_taking_damage as i32,
            _ => unreachable!(),
        }
    }

    fn set(&mut self, index: usize, value: i32) {
        match index {
            0 => {
                self.x = Fix::from_raw(value);
            }
            ...

            4 => {
                self.is_taking_damage = value != 0;
            }
        }
    }
}

----

/// use 'utils.tapir';
/// 
/// extern fn Self::my_cool_rust_fn(self, counter: int, frame: int) -> (int, fix);
/// 
/// x = 3.5;
/// y = 4.2;
/// 
/// spawn change_sprite();
/// 
/// wait;
/// 
/// var counter = 0;
/// 
/// loop {
///     wait;
///
///     x += 1.;
///     counter += 1;
///
///     if counter > 6 {
///         continue;
///     }
/// 
///     var alpha, beta = my_cool_rust_fn(counter, frame);
///     if beta > 0. || alpha <= 3 {
///         break;
///     }
/// }
/// 
/// trigger Bar(counter);
/// trigger Bar(3);
/// 
/// fn flash() {
///     var counter = 20;
///     is_taking_damage = true;
/// 
///     loop {
///         if counter == 0 {
///             break;
///         }
/// 
///         current_sprite = (frame / 4) % 2 + 2;
///         wait;
///     }
/// 
///     is_taking_damage = false;
/// }
/// 
/// fn change_sprite() {
///     loop {
///         if !is_taking_damage {
///             current_sprite = (frame / 4) % 2;
///         }
/// 
///         wait;
///     }
/// }
/// 
/// fn my_function(argument: fix) -> (fix, fix) {
///     var my_var = 3;
///     return argument + 22, my_var;
/// }
/// 
/// on event take_damage(amount: fix) {
///     if amount < 0 {
///         panic
///     }
/// 
///     if amount > 3.5 {
///         flash();
///     }
/// }
----

struct MyObjectPropertiesScript {
    pub properties: MyObjectProperties,
    states: Vec<State>,
}

struct State {
    pc: usize,
    stack: Vec<i32>,
    stack_frames: Vec<(usize, usize)>
}

enum EventKind {
    Bar(i32, i32),
}

impl MyObjectPropertiesScript {
    pub fn new() -> Self {
        Self {          
            pc: 0,
            stack: Vec::new(),
        }
    }

    pub fn run(&mut self) -> SmolVec<EventKind> {
        for state in self.states.iter().rev() {
            state.do_stuff(&mut self.properties);
        }
    }

    pub fn is_done(&self) -> bool {
        self.states.is_empty()
    }

    pub on_take_damage(&mut self, amount: Num<i32, 8>) {
        self.states.push(State {
            pc: 3847398,
            stack: vec![ScriptAccessible::get(&amount)],
        });
    }
}


----


fn render() {

}