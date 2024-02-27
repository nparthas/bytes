use std::convert::TryInto;
use std::fmt;
use std::ops::{Add, Not, Shr};

use crate::expression::{ErrorLocation, SolverError};

#[derive(Debug, Default)]
pub enum SolverType {
    #[default]
    U64,
    I64,
    U8,
    I8,
}

impl SolverType {
    pub fn is_signed(&self) -> bool {
        unimplemented!();
    }

    pub fn bits(&self) -> usize {
        unimplemented!();
    }

    pub fn from_str_radix(&self, src: &str, radix: u32, loc: usize) -> Result<SolverInt, SolverError> {
        let handle_err = |e: std::num::ParseIntError| -> SolverError {
            assert!(e.kind() == &std::num::IntErrorKind::PosOverflow);
            SolverError::ParseTooLargeNumError(ErrorLocation {
                error: "Provided integer too large",
                loc,
                hint: None,
            })
        };

        match &self {
            SolverType::U64 => match u64::from_str_radix(src, radix) {
                Err(e) => Err(handle_err(e)),
                Ok(r) => Ok(SolverInt::U64(r)),
            },
            SolverType::I64 => match i64::from_str_radix(src, radix) {
                Err(e) => Err(handle_err(e)),
                Ok(r) => Ok(SolverInt::I64(r)),
            },
        }
    }
}

impl fmt::Display for SolverType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let signed = if self.is_signed() { "signed" } else { "unsigend" };

        write!(f, "{}-bit {} integer", self.bits(), signed)
    }
}

#[derive(Debug, PartialEq)]
pub enum SolverInt {
    U64(u64),
    I64(i64),
    U8(u8),
    I8(i8),
}

impl SolverInt {
    pub fn log2(&self) -> Self {
        unimplemented!();
        /*
        if 0 == self {
            return 0;
        }

        Self::Output::try_from(Self::BITS).unwrap() - Self::Output::try_from(self.leading_zeros()).unwrap() - 1
        */
    }

    pub fn count_ones(&self) -> Self {
        unimplemented!();
    }

    pub fn one(&self) -> Self {
        match *self {
            SolverInt::U64(_) => SolverInt::U64(1),
            SolverInt::I64(_) => SolverInt::I64(1),
            SolverInt::U8(_) => SolverInt::U8(1),
            SolverInt::I8(_) => SolverInt::I8(1),
        }
    }

    pub fn zero(&self) -> Self {
        match *self {
            SolverInt::U64(_) => SolverInt::U64(0),
            SolverInt::I64(_) => SolverInt::I64(0),
            SolverInt::U8(_) => SolverInt::U8(0),
            SolverInt::I8(_) => SolverInt::I8(0),
        }
    }

    pub fn is_one(&self) -> bool {
        match *self {
            SolverInt::U64(x) => x == 1,
            SolverInt::I64(x) => x == 1,
            SolverInt::U8(x) => x == 1,
            SolverInt::I8(x) => x == 1,
        }
    }

    pub fn is_zero(&self) -> bool {
        match *self {
            SolverInt::U64(x) => x == 0,
            SolverInt::I64(x) => x == 0,
            SolverInt::U8(x) => x == 0,
            SolverInt::I8(x) => x == 0,
        }
    }

    pub fn as_usize(&self) -> usize {
        match *self  {
            SolverInt::U64(x) => x as usize,
            SolverInt::I64(x) => x as usize,
            SolverInt::U8(x) => x as usize,
            SolverInt::I8(x) => x as usize,
        }
    }
}

impl fmt::Display for SolverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SolverInt::U64(x) => write!(f, "{}u64", x),
        }
    }
}

impl fmt::Binary for SolverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SolverInt::U64(x) => fmt::Binary::fmt(&x, f),
            SolverInt::I64(x) => fmt::Binary::fmt(&x, f),
            SolverInt::U8(x) => fmt::Binary::fmt(&x, f),
            SolverInt::I8(x) => fmt::Binary::fmt(&x, f),
        }
    }
}

impl fmt::LowerHex for SolverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SolverInt::U64(x) => fmt::LowerHex::fmt(&x, f),
            SolverInt::I64(x) => fmt::LowerHex::fmt(&x, f),
            SolverInt::U8(x) => fmt::LowerHex::fmt(&x, f),
            SolverInt::I8(x) => fmt::LowerHex::fmt(&x, f),
        }
    }
}

/*
macro_rules! obvious_impl {
    (impl $trait_: ident for $type_: ident { fn $method: ident }) => {
        impl $trait_<$type_> for $type_ {
            type Output = $type_;

            fn $method(self, $type_(b): $type_) -> $type_ {
                let $type_(a) = self;
                $type_(a.$method(&b))
            }
        }
    };
}
*/

// #![feature(trace_macros)]
// trace_macros!(true);

// $type_ only works for `SolverInt` but it looks nicer to have in the macro
macro_rules! forward_binary_impl {
    (impl $trait_: ident for $type_: ident { fn $method: ident }) => {
        impl $trait_<$type_> for $type_ {
            type Output = $type_;

            fn $method(self, rhs: $type_) -> $type_ {
                match (&self, rhs) {
                    ($type_::U64(r), $type_::U64(l)) => $type_::U64(r.$method(l)),
                    ($type_::I64(r), $type_::I64(l)) => $type_::I64(r.$method(l)),
                    ($type_::U8(r), $type_::U8(l)) => $type_::U8(r.$method(l)),
                    ($type_::I8(r), $type_::I8(l)) => $type_::I8(r.$method(l)),
                    _ => panic!("mismatched types {}, {}", self, rhs),
                }
            }
        }
    };
}

// $type_ only works for `SolverInt` but it looks nicer to have in the macro
macro_rules! forward_unary_impl {
    (impl $trait_: ident for $type_: ident { fn $method: ident }) => {
        impl $trait_ for $type_ {
            type Output = $type_;

            fn $method(self) -> $type_ {
                match (&self) {
                    $type_::U64(r) => $type_::U64(r.$method().into()),
                    $type_::I64(r) => $type_::I64(r.$method().into()),
                    $type_::U8(r) => $type_::U8(r.$method().try_into().unwrap()),
                    $type_::I8(r) => $type_::I8(r.$method().try_into().unwrap()),
                }
            }
        }
    };
}

pub trait ILog2 {
    type Output;

    fn ilog2(self) -> Self::Output;
}

forward_binary_impl! { impl Add for SolverInt { fn add } }
forward_binary_impl! { impl Shr for SolverInt { fn shr } }
forward_unary_impl! { impl Not for SolverInt { fn not } }
forward_unary_impl! { impl ILog2 for SolverInt { fn ilog2 } }