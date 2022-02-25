use std::fmt;
use std::mem;

use crate::expression::solver::*;

type FormatInt = usize;

macro_rules! static_assert {
    ($condition:expr) => {
        const _: &() = &[()][1 - ($condition) as usize];
    };
}

#[derive(Debug)]
pub enum FormatError {
    InvalidFormatString(&'static str),
    ParseBitsError(&'static str),
    ParseIntError(&'static str),
    BitFormatError(&'static str),
}

impl From<std::num::ParseIntError> for FormatError {
    fn from(_: std::num::ParseIntError) -> Self {
        FormatError::ParseIntError("Could not parse num from fmt string")
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormatError::InvalidFormatString(e) => write!(f, "{}", e),
            FormatError::ParseBitsError(e) => write!(f, "{}", e),
            FormatError::ParseIntError(e) => write!(f, "{}", e),
            FormatError::BitFormatError(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Bits {
    Two,
    Four,
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
    Free,
}

impl Bits {
    fn from_num(num: FormatInt) -> Result<Self, FormatError> {
        match num {
            0 => Ok(Bits::Free),
            2 => Ok(Bits::Two),
            4 => Ok(Bits::Four),
            8 => Ok(Bits::Eight),
            16 => Ok(Bits::Sixteen),
            32 => Ok(Bits::ThirtyTwo),
            64 => Ok(Bits::SixtyFour),
            _ => Err(FormatError::ParseBitsError(
                "Invalid with specifier to binary, must be unspecified/0 or in [2,4,8,16]",
            )),
        }
    }

    fn to_num(self) -> FormatInt {
        match self {
            Bits::Two => 2,
            Bits::Four => 4,
            Bits::Eight => 8,
            Bits::Sixteen => 16,
            Bits::ThirtyTwo => 32,
            Bits::SixtyFour => 64,
            Bits::Free => 0,
        }
    }
}

impl fmt::Display for Bits {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_num())
    }
}

#[derive(Debug, PartialEq)]
pub enum PrintStyle {
    Decimal,
    Binary(Bits),
    Hex(Bits),
    Pretty,
}

impl fmt::Display for PrintStyle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrintStyle::Decimal => write!(f, "Decimal")?,
            PrintStyle::Binary(bits) => {
                write!(f, "Binary")?;
                if Bits::Free != *bits {
                    write!(f, ", {}-bit", bits)?;
                }
            }
            PrintStyle::Hex(bits) => {
                write!(f, "Hex")?;
                if Bits::Free != *bits {
                    write!(f, ", {}-bit", bits)?;
                }
            }
            PrintStyle::Pretty => {
                write!(f, "Pretty")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Formatter {
    print_style: PrintStyle,
}

#[derive(Debug, PartialEq)]
pub enum FormatResult {
    Ok(String),
    OverWidth(String, Bits),
}

impl Formatter {
    pub fn new() -> Self {
        Formatter::default()
    }

    const fn hex_bounds() -> (Bits, Bits) {
        (Bits::Eight, Bits::SixtyFour)
    }

    const fn bin_bounds() -> (Bits, Bits) {
        (Bits::Two, Bits::Sixteen)
    }

    // must be in the form /\d+[d,b,x] and match up with valid digit combination
    pub fn update_fmt(&mut self, fmt_string: &str) -> Result<(), FormatError> {
        let mut itr = fmt_string.chars().peekable();

        if let Some('/') = itr.next() {
            let mut len = String::with_capacity(5);
            while let Some(c) = itr.peek() {
                if !c.is_numeric() {
                    break;
                }

                len.push(itr.next().unwrap());
            }

            match itr.next() {
                Some('d') => {
                    if !len.is_empty() {
                        return Err(FormatError::InvalidFormatString(
                            "Length argument not available with decimal format",
                        ));
                    }

                    self.print_style = PrintStyle::Decimal;
                    return Ok(());
                }
                Some('b') => {
                    let num = if "" == len.as_str() {
                        0
                    } else {
                        FormatInt::from_str_radix(len.as_str(), 10)?
                    };

                    let bits = Bits::from_num(num)?;
                    if bits != Bits::Free && bits > Formatter::bin_bounds().1 {
                        return Err(FormatError::BitFormatError("Bit format must be <= 16-bit"));
                    }

                    self.print_style = PrintStyle::Binary(bits);
                    return Ok(());
                }
                Some('x') => {
                    let num = if "" == len.as_str() {
                        0
                    } else {
                        FormatInt::from_str_radix(len.as_str(), 10)?
                    };

                    let bits = Bits::from_num(num)?;
                    if bits < Formatter::hex_bounds().0 {
                        return Err(FormatError::BitFormatError("Bit format must be >= 8-bit"));
                    }

                    self.print_style = PrintStyle::Hex(bits);
                    return Ok(());
                }
                Some('p') => {
                    self.print_style = PrintStyle::Pretty;
                    return Ok(());
                }

                _ => {
                    return Err(FormatError::InvalidFormatString(
                        "Expected one of [d,b,x,p] format specifiers",
                    ))
                }
            }
        }

        Err(FormatError::InvalidFormatString("Expected fmt string to start with /"))
    }

    pub fn get_fmt(&self) -> &PrintStyle {
        &self.print_style
    }

    pub fn format(&self, value: SolverInt) -> FormatResult {
        match &self.print_style {
            PrintStyle::Decimal => FormatResult::Ok(format!("{}", value)),
            PrintStyle::Binary(bits) => {
                let num_bits = bits.to_num();
                let res = format!("{:#width$b}", value, width = num_bits + 2);
                if Bits::Free != *bits && mem::size_of::<FormatInt>() * 8 != num_bits && value >> num_bits != 0 {
                    return FormatResult::OverWidth(res, *bits);
                }

                FormatResult::Ok(res)
            }
            PrintStyle::Hex(bits) => {
                let num_bits = bits.to_num();
                let res = format!("{:#0width$x}", value, width = num_bits / 4 + 2);
                println!("value:{} bits:{}", value, num_bits);
                if Bits::Free != *bits && mem::size_of::<FormatInt>() * 8 != num_bits && value >> num_bits != 0 {
                    return FormatResult::OverWidth(res, *bits);
                }

                FormatResult::Ok(res)
            }
            PrintStyle::Pretty => {
                static_assert!(mem::size_of::<SolverInt>() == 8);
                let mut res = String::with_capacity(100);

                let bits = {
                    let mut b = 2;
                    while b != 64 && 0 != value >> b {
                        b <<= 1;
                    }

                    b
                };

                let hex_bits = if bits < Formatter::hex_bounds().0.to_num() {
                    Formatter::hex_bounds().0.to_num()
                } else {
                    bits
                };

                // no clue how format strings are actually supposed to be used for this
                res.push_str(
                    format!(
                        "{:<21}{:<20}{}\n",
                        "Decimal",
                        format!("{}-bit hex", hex_bits).as_str(),
                        format!("{}-bit binary", bits).as_str()
                    )
                    .as_str(),
                );

                res.push_str(format!("{:<21}", value).as_str());
                res.push_str(format!("{:<20}", format!("{:#0width$x}", value, width = hex_bits / 4 + 2)).as_str());
                res.push_str(format!("{:#0width$b}", value, width = bits).as_str());

                FormatResult::Ok(res)
            }
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Formatter {
            print_style: PrintStyle::Decimal,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    #[derive(Debug)]
    struct TestError {
        reason: String,
    }

    impl From<FormatError> for TestError {
        fn from(e: FormatError) -> Self {
            TestError {
                reason: format!("{:?}", e),
            }
        }
    }

    macro_rules! expect_error {
        ($formatter:expr, $fmt_string:literal, $err:path) => {
            let res = $formatter.update_fmt($fmt_string);
            if let Err($err(_)) = res {
            } else {
                return Err(TestError {
                    reason: format!(
                        "(line: {}) Expected [{}] to return [{}], got [{:?}]",
                        line!(),
                        $fmt_string,
                        stringify!($err),
                        res,
                    ),
                });
            }
        };
    }

    fn ensure_width(formatter: &mut Formatter, format: &'static str, expected_width: FormatInt) -> Result<(), TestError> {
        formatter.update_fmt(format)?;
        match formatter.format(0) {
            FormatResult::Ok(res) => {
                assert_eq!(res.chars().count(), expected_width);
                Ok(())
            }
            FormatResult::OverWidth(_, _) => Err(TestError {
                reason: format!("Should not be getting OverWidth for {}", format),
            }),
        }
    }

    #[test]
    fn test_format_configurations() -> Result<(), TestError> {
        let mut formatter = Formatter::new();

        // binary
        formatter.update_fmt("/b")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Binary(Bits::Free));

        formatter.update_fmt("/2b")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Binary(Bits::from_num(2)?));

        formatter.update_fmt("/4b")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Binary(Bits::from_num(4)?));

        formatter.update_fmt("/8b")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Binary(Bits::from_num(8)?));

        formatter.update_fmt("/16b")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Binary(Bits::from_num(16)?));

        // hex
        formatter.update_fmt("/x")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Hex(Bits::Free));

        formatter.update_fmt("/8x")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Hex(Bits::from_num(8)?));

        formatter.update_fmt("/16x")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Hex(Bits::from_num(16)?));

        formatter.update_fmt("/32x")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Hex(Bits::from_num(32)?));

        formatter.update_fmt("/64x")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Hex(Bits::from_num(64)?));

        // decimal
        formatter.update_fmt("/d")?;
        assert_eq!(*formatter.get_fmt(), PrintStyle::Decimal);

        Ok(())
    }

    #[test]
    fn test_invalid_fmts() -> Result<(), TestError> {
        let mut formatter = Formatter::new();

        // binary
        expect_error!(formatter, "/3b", FormatError::ParseBitsError);
        expect_error!(formatter, "/32b", FormatError::BitFormatError);

        // hex
        expect_error!(formatter, "/2x", FormatError::BitFormatError);
        expect_error!(formatter, "/4x", FormatError::BitFormatError);
        expect_error!(formatter, "/128x", FormatError::ParseBitsError);

        // decimal
        expect_error!(formatter, "/128d", FormatError::InvalidFormatString);

        Ok(())
    }

    #[test]
    fn test_over_width() -> Result<(), TestError> {
        let mut formatter = Formatter::new();

        // binary
        formatter.update_fmt("/4b")?;
        assert_eq!(formatter.format(0b1111), FormatResult::Ok(format!("{:#b}", 0b1111)));

        assert_eq!(
            formatter.format(0b1111 + 1),
            FormatResult::OverWidth(format!("{:#b}", 0b1111 + 1), Bits::from_num(4)?)
        );

        formatter.update_fmt("/8b")?;
        assert_eq!(formatter.format(0b11111111), FormatResult::Ok(format!("{:#b}", 0b11111111)));

        assert_eq!(
            formatter.format(0b11111111 + 1),
            FormatResult::OverWidth(format!("{:#b}", 0b11111111 + 1), Bits::from_num(8)?)
        );

        // hex
        formatter.update_fmt("/16x")?;
        assert_eq!(formatter.format(0xFFFF), FormatResult::Ok(format!("{:#x}", 0xFFFF)));

        assert_eq!(
            formatter.format(0xFFFF + 1),
            FormatResult::OverWidth(format!("{:#x}", 0xFFFF + 1), Bits::from_num(16)?)
        );

        formatter.update_fmt("/32x")?;
        assert_eq!(
            formatter.format(0xFFFF_FFFF),
            FormatResult::Ok(format!("{:#x}", 0xFFFF_FFFFusize))
        );

        assert_eq!(
            formatter.format(0xFFFF_FFFF + 1),
            FormatResult::OverWidth(format!("{:#x}", 0xFFFF_FFFFisize + 1), Bits::from_num(32)?)
        );

        Ok(())
    }

    #[test]
    fn test_prints_right_width() -> Result<(), TestError> {
        let mut formatter = Formatter::new();

        // binary
        let format = "/2b";
        ensure_width(&mut formatter, format, 4)?;

        let format = "/4b";
        ensure_width(&mut formatter, format, 6)?;

        let format = "/8b";
        ensure_width(&mut formatter, format, 10)?;

        let format = "/16b";
        ensure_width(&mut formatter, format, 18)?;

        // hex
        let format = "/8x";
        ensure_width(&mut formatter, format, 4)?;

        let format = "/16x";
        ensure_width(&mut formatter, format, 6)?;

        let format = "/32x";
        ensure_width(&mut formatter, format, 10)?;

        let format = "/64x";
        ensure_width(&mut formatter, format, 18)?;

        Ok(())
    }
}
