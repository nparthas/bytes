use std::convert::TryFrom;
use std::fmt;
use std::iter;
use std::mem;
use std::num::{ParseIntError, TryFromIntError};
use std::str;

// TODO:: implement Display eventually
// TODO:: fix closing brackets terminating expression
// TODO:: equals implementation
// TODO:: brackets next to each other
// TODO:: shift ops >>/<<
// TODO:: floating point support
// TODO:: identifiers -- add to TokenType and keep value as str,

pub type SolverInt = isize;

macro_rules! ok_some {
    ($x:expr) => {
        Ok(Some($x))
    };
}

macro_rules! unsfe {
    ($x:expr) => {
        unsafe { $x }
    };
}

#[derive(Debug)]
pub enum SolverError {
    ParseNumError(String),
    ParseUnsignedPowError(TryFromIntError),
    InvalidExpressionError(String),
    UnbalancedBracketError(String),
    NotImplementedError,
}

impl From<TryFromIntError> for SolverError {
    fn from(error: std::num::TryFromIntError) -> Self {
        SolverError::ParseUnsignedPowError(error)
    }
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Op,
    Num,
}

union TokenElement {
    op: OpToken,
    num: SolverInt,
}

pub struct Token {
    token_type: TokenType,
    element: TokenElement,
}

impl Token {
    fn from_op(op: OpToken) -> Self {
        Token {
            token_type: TokenType::Op,
            element: TokenElement { op },
        }
    }

    fn from_num(num: SolverInt) -> Self {
        Token {
            token_type: TokenType::Num,
            element: TokenElement { num },
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Token")
            .field("token_type", &self.token_type)
            .field(
                "element",
                if self.token_type == TokenType::Num {
                    unsfe!(&self.element.num)
                } else {
                    unsfe!(&self.element.op)
                },
            )
            .finish()
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum OpToken {
    _Equal, // unused
    Plus,
    Minus, // this can be binary or unary
    Mul,
    Div,
    Mod,
    Exp,
    Open,
    Close,
}

impl OpToken {
    fn precedence(&self) -> i32 {
        match &self {
            OpToken::_Equal => 1,
            OpToken::Plus | OpToken::Minus => 2,
            OpToken::Mul | OpToken::Div | OpToken::Mod => 3,
            OpToken::Exp => 5,
            OpToken::Open | OpToken::Close => 6,
        }
    }

    fn is_binary(&self) -> bool {
        match &self {
            OpToken::_Equal | OpToken::Open | OpToken::Close => false,
            OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Exp => true,
        }
    }

    fn prec_bump(&self) -> i32 {
        if self.is_right_assoc() {
            0
        } else {
            1
        }
    }

    fn is_right_assoc(&self) -> bool {
        match &self {
            OpToken::_Equal | OpToken::Exp => true,
            OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Open
            | OpToken::Close => false,
        }
    }

    const PRECEDENCE_NO_PREC: i32 = 0;
    const PRECEDENCE_NEG_TOK: i32 = 4;
}

struct TokenFeed<'a> {
    _src: String,
    _peek: Option<Result<Option<Token>, SolverError>>,
    itr: iter::Peekable<iter::Enumerate<str::Chars<'a>>>,
}

// our return type isn't what Iterator is so we don't implement
// using transmute instead of manually holding the index since its simpler
impl<'a> TokenFeed<'a> {
    fn new(line: String) -> TokenFeed<'a> {
        let itr = unsfe!(mem::transmute(line.chars().enumerate().peekable()));
        TokenFeed {
            _src: line,
            _peek: None,
            itr,
        }
    }

    fn next(&mut self) -> Result<Option<Token>, SolverError> {
        // if we've peeked a value before return it and empty the cache
        if self._peek.is_some() {
            return mem::replace(&mut self._peek, None).unwrap();
        }

        while let Some(c) = self.itr.next() {
            match c.1 {
                '(' => return ok_some!(Token::from_op(OpToken::Open)),
                ')' => return ok_some!(Token::from_op(OpToken::Close)),
                '+' => return ok_some!(Token::from_op(OpToken::Plus)),
                '-' => return ok_some!(Token::from_op(OpToken::Minus)),
                '*' => return ok_some!(Token::from_op(OpToken::Mul)),
                '/' => return ok_some!(Token::from_op(OpToken::Div)),
                '^' => return ok_some!(Token::from_op(OpToken::Exp)),
                '%' => return ok_some!(Token::from_op(OpToken::Mod)),
                '0'..='9' | 'a'..='f' => match TokenFeed::extract_number(c.1, &mut self.itr) {
                    Ok(num) => return ok_some!(Token::from_num(num)),
                    Err(_) => {
                        return Err(SolverError::ParseNumError(format!(
                            "Invalid char trying to parse num '{}' near col [{}]",
                            c.1,
                            c.0 + 1
                        )))
                    }
                },
                c if c.is_whitespace() => continue,
                _ => {
                    return Err(SolverError::ParseNumError(format!(
                        "Invalid char in expression '{}' near at col [{}]",
                        c.1,
                        c.0 + 1
                    )))
                }
            }
        }

        Ok(None)
    }

    fn peek(&mut self) -> &Result<Option<Token>, SolverError> {
        if self._peek.is_some() {
            return self._peek.as_ref().unwrap();
        }

        self._peek = Some(self.next());
        self._peek.as_ref().unwrap()
    }

    fn cur_col(&mut self) -> Option<usize> {
        if let Some(val) = self.itr.peek() {
            return Some(val.0);
        }

        None
    }

    fn extract_number(
        c: char,
        itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>,
    ) -> Result<SolverInt, ParseIntError> {
        // probably the best way to do this since rust isn't ASCII
        let mut s = String::with_capacity(18);
        if c != '0' {
            s.push(c);
        }
        // handle 0b and 0x prefixes
        let mut radix = 10;
        if let Some(c) = itr.peek() {
            match c.1 {
                'b' | 'B' => {
                    radix = 2;
                    itr.next();
                }
                'x' | 'X' => {
                    radix = 16;
                    itr.next();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => s.push(itr.next().unwrap().1),
                _ => {
                    return Ok(SolverInt::from_str_radix(s.as_str(), radix)?);
                }
            }
        }
        while let Some(c) = itr.peek() {
            match c.1 {
                '0'..='9' | 'a'..='f' | 'A'..='F' => s.push(itr.next().unwrap().1),
                _ => break,
            }
        }
        Ok(SolverInt::from_str_radix(s.as_str(), radix)?)
    }
}

fn should_loop(precedence: i32, feed: &mut TokenFeed) -> Result<bool, SolverError> {
    let next = match feed.peek() {
        Ok(Some(x)) => x,
        Ok(None) => return Ok(false), // we are done parsing
        Err(_) => return Err(feed.next().err().unwrap()),
    };

    let op = unsfe!(next.element.op);
    Ok(next.token_type == TokenType::Op && op.is_binary() && op.precedence() >= precedence)
}

// do expression until a token with a lower precedence is found
fn do_expression(precedence: i32, feed: &mut TokenFeed) -> Result<SolverInt, SolverError> {
    let cur = match feed.next()? {
        Some(x) => x,
        None => {
            return Err(SolverError::InvalidExpressionError(
                "Expected a token at the start of an expression".to_string(),
            ))
        }
    };

    let mut num1 = match cur.token_type {
        TokenType::Num => unsfe!(cur.element.num),
        TokenType::Op => match unsfe!(cur.element.op) {
            OpToken::Minus => -do_expression(OpToken::PRECEDENCE_NEG_TOK, feed)?,
            OpToken::Open => {
                let res = do_expression(OpToken::PRECEDENCE_NO_PREC, feed)?;
                if let Some(next) = feed.next()? {
                    if !(next.token_type == TokenType::Op && unsfe!(next.element.op) == OpToken::Close) {
                        return Err(SolverError::UnbalancedBracketError(format!(
                            "Expected a closing bracket near col [{}]",
                            feed.cur_col().expect("We should be able to peek a token here")
                        )));
                    }
                } else {
                    return Err(SolverError::UnbalancedBracketError(
                        "Expected a closing bracket".to_string(),
                    ));
                }

                res
            }
            _ => {
                return Err(SolverError::InvalidExpressionError(format!(
                    "Unexpected token near {}",
                    if let Some(col) = feed.cur_col() {
                        format!("col [{}]", col)
                    } else {
                        "the end".to_string()
                    }
                )));
            }
        },
    };

    while should_loop(precedence, feed)? {
        let next = feed.next()?.unwrap();
        let op = unsfe!(next.element.op);

        let num2 = do_expression(op.precedence() + op.prec_bump(), feed)?;

        match op {
            OpToken::Plus => num1 += num2,
            OpToken::Minus => num1 -= num2,
            OpToken::Mul => num1 *= num2,
            OpToken::Div => num1 /= num2,
            OpToken::Mod => num1 %= num2,
            OpToken::Exp => num1 = num1.pow(u32::try_from(num2)?),
            OpToken::_Equal => return Err(SolverError::NotImplementedError),
            _ => {
                // I don't think it's possible to hit here, in case the condition is found
                panic!("hit here... save the equation and write a test");
                // return Err(SolverError::InvalidExpressionError(
                //     "Invalid op token here".to_string(),
                // ))
            }
        }
    }

    Ok(num1)
}

pub fn solve(expr: &str) -> Result<SolverInt, SolverError> {
    let mut tokenfeed = TokenFeed::new(expr.to_string());
    do_expression(0, &mut tokenfeed)
}

#[cfg(test)]
mod tests {
    use super::*;

    const OPS: [OpToken; 9] = [
        OpToken::_Equal,
        OpToken::Plus,
        OpToken::Minus,
        OpToken::Mul,
        OpToken::Div,
        OpToken::Mod,
        OpToken::Exp,
        OpToken::Open,
        OpToken::Close,
    ];

    #[derive(Debug)]
    struct TestError {
        reason: String,
    }

    impl From<SolverError> for TestError {
        fn from(e: SolverError) -> Self {
            TestError {
                reason: format!("{:?}", e),
            }
        }
    }

    fn call_eq(expr: &str, expected: SolverInt) -> Result<(), TestError> {
        let actual = solve(expr);
        match actual {
            Ok(actual) => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(TestError {
                        reason: format!("Calling solver on [{}] A:{} E:{}", expr, actual, expected),
                    })
                }
            }
            Err(e) => Err(TestError {
                reason: format!("Calling solver on [{}] returned [{:?}]", expr, e),
            }),
        }
    }

    macro_rules! expect_error {
        ($equation:expr, $err:path) => {
            let res = solve($equation);
            if let Err($err(_)) = res {
            } else {
                return Err(TestError {
                    reason: format!(
                        "(line: {}) Expected [{}] to return [{}], got [{:?}]",
                        line!(),
                        $equation,
                        stringify!($err),
                        res,
                    ),
                });
            }
        };
    }

    #[test]
    fn test_basic_ops() -> Result<(), TestError> {
        for op in OPS.iter() {
            match op {
                OpToken::_Equal => assert!("not implemented" != ""),
                OpToken::Plus => call_eq("5 + 5", 10)?,
                OpToken::Minus => call_eq("-5 -2", -7)?,
                OpToken::Mul => call_eq("12*15", 180)?,
                OpToken::Div => call_eq("12/3", 4)?,
                OpToken::Mod => call_eq("8%5", 3)?,
                OpToken::Exp => call_eq("9^3", 729)?,
                OpToken::Open => call_eq("(5+1)*2", 12)?,
                OpToken::Close => call_eq("(3-7)/-2", 2)?,
            }
        }

        Ok(())
    }

    #[test]
    fn test_exp_right_assoc() -> Result<(), TestError> {
        call_eq("2^3^2", 512)?;
        Ok(())
    }

    #[test]
    fn test_0_padding() -> Result<(), TestError> {
        call_eq("00000100 * 110011", 11001100)?;
        Ok(())
    }

    #[test]
    fn test_hex_and_bin() -> Result<(), TestError> {
        call_eq("0x55 - 0b1101101", -24)?;
        call_eq("0x055 - 0b0000001101101", -24)?;
        Ok(())
    }

    #[test]
    fn test_bedmas() -> Result<(), TestError> {
        let inputs = [
            ("(5+2)*2", 14),
            ("(3-1)*10", 20),
            ("(2*3)^2", 36),
            ("(1-2)*5", -5),
            ("3*(8/2) + 1", 13),
            ("-5 * (-3+1)/2", 5),
            ("((2+1)*4)^2", 144),
            ("36/(1+2)", 12),
            ("5*2 +1", 11),
            ("5*5 - 1 ^10 * 55000 / 100 ", -525),
            ("6 + (16 - 4)/(2^2 + 2) - 2", 6),
            ("(4 + 8)/(2 + 1) - (3 - 1) + 2", 4),
        ];

        for input in inputs.iter() {
            call_eq(input.0, input.1)?;
        }

        Ok(())
    }

    #[test]
    fn test_parse_errors() -> Result<(), TestError> {
        expect_error!("2 - 2asf", SolverError::ParseNumError);
        expect_error!("(3+1) + ) + 2", SolverError::InvalidExpressionError);
        expect_error!("((3+1)", SolverError::UnbalancedBracketError);
        expect_error!("(3+1 1*2", SolverError::UnbalancedBracketError);
        expect_error!("(6+1", SolverError::UnbalancedBracketError);
        expect_error!("6+1)", SolverError::InvalidExpressionError);
        expect_error!("5 *2 - a$sdf$", SolverError::ParseNumError);
        expect_error!("2^-1", SolverError::ParseUnsignedPowError);
        expect_error!("", SolverError::InvalidExpressionError);
        expect_error!("3//2", SolverError::InvalidExpressionError);
        expect_error!("3 2", SolverError::InvalidExpressionError);
        Ok(())
    }
}
