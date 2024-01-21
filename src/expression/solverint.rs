use std::fmt;
use std::ops::{Add, Not, Shr};


#[derive(Debug, Default)]
pub enum SolverType {
    #[default]
    U64,
}

impl SolverType {

    pub fn is_signed(self) -> bool {
        unimplemented!();
    }

    pub fn bits(self) -> usize {
        unimplemented!();
    }
}

impl fmt::Display for SolverType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let signed = if self.is_signed() {
            "signed"
        } else {
            "unsigend"
        };

        write!(f, "{}-bit {} integer", self.bits(), signed)
    }
}

#[derive(Debug, PartialEq)]
pub enum SolverInt {
    U64(u64),
}

impl SolverInt {
    pub fn log2(self) -> Self {
        unimplemented!();
        /*
        if 0 == self {
            return 0;
        }

        Self::Output::try_from(Self::BITS).unwrap() - Self::Output::try_from(self.leading_zeros()).unwrap() - 1
        */
    }

    pub fn is_signed(self) -> bool {
        unimplemented!();
    }
}

impl fmt::Display for SolverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            SolverInt::U64(x) => write!(f, "{}u64", x),
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
                match (self, rhs) {
                    ($type_::U64(r), $type_::U64(l)) => $type_::U64(r.$method(l)),
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
                match (self) {
                    $type_::U64(r) => $type_::U64(r.$method()),
                }
            }
        }
    };
}


forward_binary_impl! { impl Add for SolverInt { fn add } }
forward_binary_impl! { impl Shr for SolverInt { fn shr } }
forward_unary_impl! { impl Not for SolverInt { fn not } }

// TODO:: convert to macro and implement all required operators

// impl Add for SolverInt {
//     type Output = Self;
//
//     fn add(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (SolverInt::U64(r), SolverInt::U64(l)) => SolverInt::U64(r.add(l)),
//             _ => panic!("mismatched types {}, {}", self, rhs),
//         }
//     }
// }
