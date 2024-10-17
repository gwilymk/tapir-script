#![no_std]

pub use tapir_script_macros::TapirScript;
pub use vm::TapirScript;

pub trait TapirProperty {
    fn to_i32(&self) -> i32;
    fn set_from_i32(&mut self, value: i32);
}

impl TapirProperty for i32 {
    fn to_i32(&self) -> i32 {
        *self
    }

    fn set_from_i32(&mut self, value: i32) {
        *self = value;
    }
}

impl TapirProperty for bool {
    fn to_i32(&self) -> i32 {
        (*self).into()
    }

    fn set_from_i32(&mut self, value: i32) {
        *self = value != 0;
    }
}
