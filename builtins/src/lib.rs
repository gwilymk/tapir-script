#![no_std]

use agb_fixnum::Num;

pub type Fix = Num<i32, 8>;

/// Error that can occur when executing a builtin function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinError {
    /// sqrt was called with a negative argument
    SqrtNegative,
}

/// Context for impure builtin functions that need runtime state.
pub struct Context {
    /// Current frame number
    pub frame: i32,
}

/// Execute a pure builtin function (ID >= 0).
/// Returns None if the builtin ID is unknown.
/// Returns Some(Err(...)) if the builtin fails (e.g., sqrt of negative).
pub fn execute_pure(builtin_id: i16, args: &[i32]) -> Option<Result<i32, BuiltinError>> {
    Some(match builtin_id {
        0 => Ok(sin(Fix::from_raw(args[0])).to_raw()),
        1 => Ok(cos(Fix::from_raw(args[0])).to_raw()),
        2 => sqrt(Fix::from_raw(args[0])).map(|v| v.to_raw()),
        3 => Ok(floor(Fix::from_raw(args[0]))),
        4 => Ok(ceil(Fix::from_raw(args[0]))),
        5 => Ok(round(Fix::from_raw(args[0]))),
        _ => return None,
    })
}

/// Execute an impure builtin function (ID < 0).
/// Returns None if the builtin ID is unknown.
pub fn execute_impure(builtin_id: i16, _args: &[i32], ctx: &Context) -> Option<i32> {
    match builtin_id {
        -1 => Some(ctx.frame),
        _ => None,
    }
}

/// sin(x) - returns the sine of x where x is in the range 0..1
/// (0 = 0 radians, 0.5 = π radians, 1 = 2π radians / full rotation)
fn sin(x: Fix) -> Fix {
    x.sin()
}

/// cos(x) - returns the cosine of x where x is in the range 0..1
/// (0 = 0 radians, 0.5 = π radians, 1 = 2π radians / full rotation)
fn cos(x: Fix) -> Fix {
    x.cos()
}

/// sqrt(x) - returns the square root of x, or error if negative
fn sqrt(x: Fix) -> Result<Fix, BuiltinError> {
    if x < Fix::new(0) {
        Err(BuiltinError::SqrtNegative)
    } else {
        Ok(x.sqrt())
    }
}

/// floor(x) - returns the largest integer less than or equal to x
fn floor(x: Fix) -> i32 {
    x.floor()
}

/// ceil(x) - returns the smallest integer greater than or equal to x
fn ceil(x: Fix) -> i32 {
    -(-x).floor()
}

/// round(x) - returns the nearest integer to x
fn round(x: Fix) -> i32 {
    x.round()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sin_zero() {
        let result = execute_pure(0, &[Fix::new(0).to_raw()]);
        assert!(result.is_some());
        let value = result.unwrap().unwrap();
        // sin(0) should be 0
        assert_eq!(value, 0);
    }

    #[test]
    fn test_cos_zero() {
        let result = execute_pure(1, &[Fix::new(0).to_raw()]);
        assert!(result.is_some());
        let value = Fix::from_raw(result.unwrap().unwrap());
        // cos(0) should be 1
        assert_eq!(value, Fix::new(1));
    }

    #[test]
    fn test_sqrt_four() {
        let result = execute_pure(2, &[Fix::new(4).to_raw()]);
        assert!(result.is_some());
        let value = Fix::from_raw(result.unwrap().unwrap());
        // sqrt(4) should be 2
        assert_eq!(value, Fix::new(2));
    }

    #[test]
    fn test_sqrt_negative() {
        let result = execute_pure(2, &[Fix::new(-1).to_raw()]);
        assert!(result.is_some());
        assert_eq!(result.unwrap(), Err(BuiltinError::SqrtNegative));
    }

    #[test]
    fn test_floor() {
        // floor(2.5) should be 2
        let x = Fix::new(2) + Fix::new(1) / 2;
        let result = execute_pure(3, &[x.to_raw()]);
        assert_eq!(result.unwrap().unwrap(), 2);
    }

    #[test]
    fn test_ceil() {
        // ceil(2.5) should be 3
        let x = Fix::new(2) + Fix::new(1) / 2;
        let result = execute_pure(4, &[x.to_raw()]);
        assert_eq!(result.unwrap().unwrap(), 3);
    }

    #[test]
    fn test_round() {
        // round(2.5) should be 3
        let x = Fix::new(2) + Fix::new(1) / 2;
        let result = execute_pure(5, &[x.to_raw()]);
        assert_eq!(result.unwrap().unwrap(), 3);
    }

    #[test]
    fn test_frame() {
        let ctx = Context { frame: 42 };
        let result = execute_impure(-1, &[], &ctx);
        assert_eq!(result, Some(42));
    }

    #[test]
    fn test_unknown_pure() {
        let result = execute_pure(999, &[]);
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_impure() {
        let ctx = Context { frame: 0 };
        let result = execute_impure(-999, &[], &ctx);
        assert!(result.is_none());
    }
}
