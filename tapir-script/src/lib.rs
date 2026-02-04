#![deny(clippy::all)]
#![no_std]

extern crate alloc;

pub use tapir_script_macros::{ConvertBetweenTapir, TapirScript};
pub use vm::{NoEventType, Script, TapirScript};

pub type Fix = agb_fixnum::Num<i32, 8>;
use agb_fixnum::Vector2D;

#[doc(hidden)]
pub mod __private {
    pub use alloc::vec::Vec;
}

impl ConvertBetweenTapir for Vector2D<i32> {
    const SIZE: usize = 2;

    fn write_to_tapir(&self, target: &mut [i32]) {
        target[0] = self.x;
        target[1] = self.y;
    }

    fn read_from_tapir(values: &[i32]) -> Self {
        Self {
            x: values[0],
            y: values[1],
        }
    }
}

impl ConvertBetweenTapir for Vector2D<Fix> {
    const SIZE: usize = 2;

    fn write_to_tapir(&self, target: &mut [i32]) {
        target[0] = self.x.to_raw();
        target[1] = self.y.to_raw();
    }

    fn read_from_tapir(values: &[i32]) -> Self {
        Self {
            x: Fix::from_raw(values[0]),
            y: Fix::from_raw(values[1]),
        }
    }
}

/// Trait for converting Rust types to/from tapir's i32 stack representation.
///
/// This trait handles both scalar types (i32, Fix, bool) and composite types (structs).
/// For structs, derive this trait using `#[derive(ConvertBetweenTapir)]`.
pub trait ConvertBetweenTapir: Sized {
    /// Number of i32 slots this type occupies (compile-time constant).
    const SIZE: usize;

    /// Writes self into target slice. Target must have at least SIZE elements.
    fn write_to_tapir(&self, target: &mut [i32]);

    /// Reads Self from the values slice. Values must have at least SIZE elements.
    fn read_from_tapir(values: &[i32]) -> Self;

    /// Convenience method: resize vec and write. Uses SIZE for exact allocation.
    fn write_to_tapir_vec(&self, target: &mut alloc::vec::Vec<i32>) {
        let start = target.len();
        target.resize(start + Self::SIZE, 0);
        self.write_to_tapir(&mut target[start..]);
    }
}

impl ConvertBetweenTapir for i32 {
    const SIZE: usize = 1;

    fn write_to_tapir(&self, target: &mut [i32]) {
        target[0] = *self;
    }

    fn read_from_tapir(values: &[i32]) -> Self {
        values[0]
    }
}

impl ConvertBetweenTapir for bool {
    const SIZE: usize = 1;

    fn write_to_tapir(&self, target: &mut [i32]) {
        target[0] = (*self).into();
    }

    fn read_from_tapir(values: &[i32]) -> Self {
        values[0] != 0
    }
}

impl ConvertBetweenTapir for Fix {
    const SIZE: usize = 1;

    fn write_to_tapir(&self, target: &mut [i32]) {
        target[0] = self.to_raw();
    }

    fn read_from_tapir(values: &[i32]) -> Self {
        Fix::from_raw(values[0])
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
