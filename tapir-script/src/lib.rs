#![deny(clippy::all)]
#![no_std]

extern crate alloc;

pub use tapir_script_macros::{ConvertBetweenTapir, TapirScript};
pub use vm::{Script, TapirScript};

pub type Fix = agb_fixnum::Num<i32, 8>;
use agb_fixnum::Vector2D;

#[doc(hidden)]
pub mod __private {
    pub use alloc::vec::Vec;
}

impl ConvertBetweenTapir for Vector2D<i32> {
    fn write_to_tapir(&self, target: &mut [i32]) -> usize {
        target[0] = self.x;
        target[1] = self.y;
        2
    }

    fn read_from_tapir(values: &[i32]) -> (Self, usize) {
        (
            Self {
                x: values[0],
                y: values[1],
            },
            2,
        )
    }
}

impl ConvertBetweenTapir for Vector2D<Fix> {
    fn write_to_tapir(&self, target: &mut [i32]) -> usize {
        target[0] = self.x.to_raw();
        target[1] = self.y.to_raw();
        2
    }

    fn read_from_tapir(values: &[i32]) -> (Self, usize) {
        (
            Self {
                x: Fix::from_raw(values[0]),
                y: Fix::from_raw(values[1]),
            },
            2,
        )
    }
}

/// Trait for converting Rust types to/from tapir's i32 stack representation.
///
/// This trait handles both scalar types (i32, Fix, bool) and composite types (structs).
/// For structs, derive this trait using `#[derive(ConvertBetweenTapir)]`.
pub trait ConvertBetweenTapir: Sized {
    /// Writes self into target slice, assuming target has enough space.
    /// Returns the number of i32 slots used.
    fn write_to_tapir(&self, target: &mut [i32]) -> usize;

    /// Reads Self from the values slice.
    /// Returns Self and the number of i32 slots consumed.
    fn read_from_tapir(values: &[i32]) -> (Self, usize);
}

impl ConvertBetweenTapir for i32 {
    fn write_to_tapir(&self, target: &mut [i32]) -> usize {
        target[0] = *self;
        1
    }

    fn read_from_tapir(values: &[i32]) -> (Self, usize) {
        (values[0], 1)
    }
}

impl ConvertBetweenTapir for bool {
    fn write_to_tapir(&self, target: &mut [i32]) -> usize {
        target[0] = (*self).into();
        1
    }

    fn read_from_tapir(values: &[i32]) -> (Self, usize) {
        (values[0] != 0, 1)
    }
}

impl ConvertBetweenTapir for Fix {
    fn write_to_tapir(&self, target: &mut [i32]) -> usize {
        target[0] = self.to_raw();
        1
    }

    fn read_from_tapir(values: &[i32]) -> (Self, usize) {
        (Fix::from_raw(values[0]), 1)
    }
}

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
