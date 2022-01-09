use std::convert::TryFrom;
use std::fmt;
use std::iter;
use std::mem;
use std::num::{ParseIntError, TryFromIntError};
use std::str;

pub type SolverInt = i32;

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

// TODO:: identifiers -- add to TokenType and keep value as str,

#[derive(Debug)]
pub enum SolverError {
    ParseIntError(ParseIntError),
    ParseNumError(ParseNumError),
    ParseUnsignedPowError(TryFromIntError),
    InvalidExpressionError(String),
    UnbalancedBracketError,
    NotImplementedError,
}

#[derive(Debug, Clone)]
pub struct ParseNumError {
    why: String,
}

impl From<ParseIntError> for SolverError {
    fn from(error: std::num::ParseIntError) -> Self {
        SolverError::ParseIntError(error)
    }
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
    fn from_op(op: OpToken) -> Token {
        Token {
            token_type: TokenType::Op,
            element: TokenElement { op },
        }
    }

    fn from_num(num: SolverInt) -> Token {
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
    // Invalid,
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
    itr: iter::Peekable<str::Chars<'a>>,
}

// our return type isn't what Iterator is so we don't implement
// using transmute instead of manually holding the index since its simpler
impl<'a> TokenFeed<'a> {
    fn new(line: String) -> TokenFeed<'a> {
        let itr = unsfe!(mem::transmute(line.chars().peekable()));
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
            match c {
                '(' => return ok_some!(Token::from_op(OpToken::Open)),
                ')' => return ok_some!(Token::from_op(OpToken::Close)),
                '+' => return ok_some!(Token::from_op(OpToken::Plus)),
                '-' => return ok_some!(Token::from_op(OpToken::Minus)),
                '*' => return ok_some!(Token::from_op(OpToken::Mul)),
                '/' => return ok_some!(Token::from_op(OpToken::Div)),
                '^' => return ok_some!(Token::from_op(OpToken::Exp)),
                '%' => return ok_some!(Token::from_op(OpToken::Mod)),
                '0'..='9' | 'a'..='f' => {
                    let num = TokenFeed::extract_number(c, &mut self.itr)?;
                    return ok_some!(Token::from_num(num));
                }
                c if c.is_whitespace() => continue,
                _ => {
                    return Err(SolverError::ParseNumError(ParseNumError {
                        why: format!("Invalid char in expression '{}'", c),
                    }))
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

    fn extract_number(
        c: char,
        itr: &mut iter::Peekable<str::Chars>,
    ) -> Result<SolverInt, SolverError> {
        // probably the best way to do this since rust isn't ASCII
        let mut s = String::with_capacity(20);
        if c != '0' {
            s.push(c);
        }
        // handle 0b and 0x prefixes
        let mut radix = 10;
        if let Some(c) = itr.peek() {
            match c {
                'b' | 'B' => {
                    radix = 2;
                    itr.next();
                }
                'x' | 'X' => {
                    radix = 16;
                    itr.next();
                }
                '1'..='9' | 'a'..='f' | 'A'..='F' => s.push(itr.next().unwrap()),
                _ => {
                    return Ok(SolverInt::from_str_radix(s.as_str(), radix)?);
                }
            }
        }
        while let Some(c) = itr.peek() {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => s.push(itr.next().unwrap()),
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
                "Expected a token here".to_string(),
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
                    if next.token_type != TokenType::Op && unsfe!(cur.element.op) != OpToken::Close
                    {
                        return Err(SolverError::UnbalancedBracketError);
                    }
                } else {
                    return Err(SolverError::InvalidExpressionError(
                        "Expected a closing bracket".to_string(),
                    ));
                }

                res
            }
            _ => {
                return Err(SolverError::InvalidExpressionError(
                    "Unexpected token here".to_string(),
                ))
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
                return Err(SolverError::InvalidExpressionError(
                    "Invalid op token here".to_string(),
                ))
            }
        }
    }

    Ok(num1)
}

pub fn solve(expr: &str) -> Result<SolverInt, SolverError> {
    let mut tokenfeed = TokenFeed::new(expr.to_string());
    do_expression(0, &mut tokenfeed)
}
