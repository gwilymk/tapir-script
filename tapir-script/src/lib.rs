#![deny(clippy::all)]
#![no_std]

extern crate alloc;

pub use tapir_script_macros::TapirScript;
pub use vm::{Script, TapirScript};

pub type Fix = agb_fixnum::Num<i32, 8>;

pub use alloc::vec::Vec;

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

impl TapirProperty for Fix {
    fn to_i32(&self) -> i32 {
        self.to_raw()
    }

    fn set_from_i32(&mut self, value: i32) {
        *self = Fix::from_raw(value);
    }
}

#[cfg(test)]
mod test {
    use crate::TapirProperty;

    extern crate std;

    #[test]
    fn tapir_property_for_bool() {
        let mut test = false;

        TapirProperty::set_from_i32(&mut test, 1);

        assert!(test);
        assert_eq!(TapirProperty::to_i32(&test), 1);

        TapirProperty::set_from_i32(&mut test, 0);

        assert!(!test);
        assert_eq!(TapirProperty::to_i32(&test), 0);
    }
}
