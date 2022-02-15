use std::collections;
use std::convert::TryFrom;
use std::fmt;
use std::iter;
use std::mem;
use std::num::TryFromIntError;
use std::str;

// TODO:: move exp to xor
// TODO:: log_2 + exp
// TODO:: shift ops >>/<<
// TODO:: logical ops
// TODO:: brackets next to each other
// TODO:: floating point support
// TODO:: handle 0b66 better
// TODO:: large num support
// TODO:: sqrt

pub type SolverInt = isize;

pub const FEED_OFFSET_END: usize = usize::MAX;
const FEED_OFFSET_BEHIND: usize = 1;
const FEED_OFFSET_ON: usize = 2;
const FEED_OFFSET_VAR_ON: usize = 3;

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

macro_rules! extract_on_radix {
    ($($pat: pat)|+, $s:expr, $itr:expr) => {
        while let Some(c) = $itr.peek() {
            match c.1 {
                $($pat)|+ => $s.push($itr.next().unwrap().1),
                _ => {
                    break;
                }
            }
        }
    };
}

// we are greedy on error reporting, just return the first error
// we see in the expression even if there are more errors or
// "better" errors
#[derive(Debug)]
pub enum SolverError {
    ParseUnsignedPowError(TryFromIntError),
    InvalidExpressionError(ErrorLocation),
    UnbalancedBracketError(ErrorLocation),
    UnsetVariableError(ErrorLocation),
    NotImplementedError,
}

#[derive(Debug)]
pub struct ErrorLocation {
    error: &'static str,
    loc: usize, // store as 0-indexed
    hint: Option<String>,
}

impl ErrorLocation {
    fn new(error: &'static str, loc: usize, hint: Option<String>) -> Self {
        ErrorLocation { error, loc, hint }
    }

    pub fn get_loc(&self) -> usize {
        self.loc
    }
}

impl fmt::Display for ErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(c) = &self.hint {
            if self.loc != usize::MAX {
                write!(f, "{} '{}' near col [{}]", self.error, c, self.loc + 1)
            } else {
                write!(f, "{} '{}' near the end of the expression", self.error, c)
            }
        } else if self.loc != usize::MAX {
            write!(f, "{} near col [{}]", self.error, self.loc + 1)
        } else {
            write!(f, "{} near the end of the expression", self.error,)
        }
    }
}

impl From<TryFromIntError> for SolverError {
    fn from(error: std::num::TryFromIntError) -> Self {
        SolverError::ParseUnsignedPowError(error)
    }
}

impl fmt::Display for SolverError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            SolverError::ParseUnsignedPowError(_) => write!(f, "Negative powers are not supported"),
            SolverError::InvalidExpressionError(e) => e.fmt(f),
            SolverError::UnbalancedBracketError(e) => e.fmt(f),
            SolverError::UnsetVariableError(e) => e.fmt(f),
            SolverError::NotImplementedError => write!(f, "Not implemented"),
        }
    }
}

#[derive(Debug)]
pub struct Variables {
    vars: collections::HashMap<Identifier, SolverInt>,
    res_ident: Identifier,
}

impl Variables {
    pub fn new() -> Self {
        Variables::default()
    }

    pub fn update(&mut self, var: Identifier, value: SolverInt) {
        self.vars.insert(var, value);
    }

    pub fn get(&self, var: &Identifier) -> Option<&SolverInt> {
        self.vars.get(var)
    }

    pub fn result_identifier(&self) -> Identifier {
        self.res_ident.clone()
    }
}

impl Default for Variables {
    fn default() -> Self {
        Variables {
            vars: collections::HashMap::new(),
            res_ident: Identifier("_".to_string()),
        }
    }
}

impl fmt::Display for Variables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(&v) = self.vars.get(&self.result_identifier()) {
            writeln!(f, "{:>6} = {}", self.result_identifier(), v)?;
        }

        for (k, v) in &self.vars {
            if *k == self.result_identifier() {
                continue;
            }

            writeln!(f, "{:>6} = {}", k, v)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
enum Token {
    Op(OpToken),
    Num(SolverInt),
    Var(Identifier),
}

impl Token {
    fn as_op(&self) -> OpToken {
        match self {
            Token::Op(op) => *op,
            _ => unreachable!(),
        }
    }

    fn as_var(&self) -> Identifier {
        match self {
            Token::Var(ident) => ident.clone(),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Num(num) => write!(f, "{}", num),
            Token::Op(op) => write!(f, "{}", op),
            Token::Var(ident) => write!(f, "{}", ident),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum OpToken {
    Equal,
    Plus,
    Minus, // this can be binary or unary
    Mul,
    Div,
    Mod,
    Exp,
    Open,
    Close,
    BitAnd,
    BitOr,
}

impl OpToken {
    fn precedence(&self) -> i32 {
        match &self {
            OpToken::BitOr => 1,
            OpToken::BitAnd => 2,
            OpToken::Equal => 3,
            OpToken::Plus | OpToken::Minus => 4,
            OpToken::Mul | OpToken::Div | OpToken::Mod => 6,
            OpToken::Exp => 7,
            OpToken::Open | OpToken::Close => 8,
        }
    }

    fn is_binary(&self) -> bool {
        match &self {
            OpToken::Open | OpToken::Close => false,
            OpToken::Equal
            | OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Exp
            | OpToken::BitAnd
            | OpToken::BitOr => true,
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
            OpToken::Equal | OpToken::Exp => true,
            OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Open
            | OpToken::Close
            | OpToken::BitAnd
            | OpToken::BitOr => false,
        }
    }

    const PRECEDENCE_NO_PREC: i32 = 0;
    const PRECEDENCE_NEG_TOK: i32 = 5;
}

impl fmt::Display for OpToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            OpToken::Equal => write!(f, "+"),
            OpToken::Exp => write!(f, "^"),
            OpToken::Plus => write!(f, "+"),
            OpToken::Minus => write!(f, "-"),
            OpToken::Mul => write!(f, "*"),
            OpToken::Div => write!(f, "/"),
            OpToken::Mod => write!(f, "%"),
            OpToken::Open => write!(f, "("),
            OpToken::Close => write!(f, ")"),
            OpToken::BitAnd => write!(f, "&"),
            OpToken::BitOr => write!(f, "|"),
        }
    }
}

struct TokenFeed<'a> {
    _src: String,
    _peek: Option<Result<Option<Token>, SolverError>>,
    itr: iter::Peekable<iter::Enumerate<str::Chars<'a>>>,
}

// our return type isn't what Iterator is so we don't implement
// using transmute instead of manually holding the index since its simpler
impl<'a> TokenFeed<'a> {
    fn new(line: String) -> Self {
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
                w if w.is_whitespace() => continue,
                '(' => return ok_some!(Token::Op(OpToken::Open)),
                ')' => return ok_some!(Token::Op(OpToken::Close)),
                '+' => return ok_some!(Token::Op(OpToken::Plus)),
                '-' => return ok_some!(Token::Op(OpToken::Minus)),
                '*' => return ok_some!(Token::Op(OpToken::Mul)),
                '/' => return ok_some!(Token::Op(OpToken::Div)),
                '^' => return ok_some!(Token::Op(OpToken::Exp)),
                '%' => return ok_some!(Token::Op(OpToken::Mod)),
                '=' => return ok_some!(Token::Op(OpToken::Equal)),
                '&' => return ok_some!(Token::Op(OpToken::BitAnd)),
                '|' => return ok_some!(Token::Op(OpToken::BitOr)),
                '0'..='9' => return ok_some!(Token::Num(TokenFeed::extract_number(c.1, &mut self.itr))),
                w if w.is_alphabetic() || '_' == w => {
                    return ok_some!(Token::Var(TokenFeed::extract_identifer(c.1, &mut self.itr)?));
                }
                _ => {
                    return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                        "Invalid char in expression",
                        c.0,
                        Some(c.1.to_string()),
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

    // lazy implementation to get the current location in the code
    // requires manually offsetting sometimes to get the right location
    fn cur_col(&mut self) -> Option<usize> {
        if let Some(val) = self.itr.peek() {
            return Some(val.0);
        }

        None
    }

    // first character must be valid
    // push any other invalid errors to be caught by other parts of the program
    fn extract_number(c: char, itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>) -> SolverInt {
        // probably the best way to do this since rust isn't ASCII
        let mut s = String::with_capacity(18);
        s.push(c);

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
                '0'..='9' => s.push(itr.next().unwrap().1),
                _ => {
                    return SolverInt::from_str_radix(s.as_str(), radix).unwrap();
                }
            }
        }

        match radix {
            16 => extract_on_radix!('0'..='9' | 'a'..='f' | 'A'..='F', s, itr),
            10 => extract_on_radix!('0'..='9', s, itr),
            2 => extract_on_radix!('0'..='1', s, itr),
            _ => panic!("Unsupported radix"),
        }

        SolverInt::from_str_radix(s.as_str(), radix).unwrap()
    }

    fn extract_identifer(c: char, itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>) -> Result<Identifier, SolverError> {
        let mut s = String::with_capacity(20);
        s.push(c);

        while let Some(c) = itr.peek() {
            if c.1.is_whitespace() || !c.1.is_alphabetic() {
                break;
            }

            s.push(itr.next().unwrap().1);
        }

        Ok(Identifier(s))
    }
}

fn should_loop(precedence: i32, feed: &mut TokenFeed) -> Result<bool, SolverError> {
    let next = match feed.peek() {
        Ok(Some(x)) => x,
        Ok(None) => return Ok(false), // we are done parsing
        Err(_) => return Err(feed.next().err().unwrap()),
    };

    match next {
        Token::Op(op) if op.is_binary() && op.precedence() >= precedence => Ok(true),
        _ => Ok(false),
    }
}

// do expression until a token with a lower precedence is found
fn do_expression(precedence: i32, feed: &mut TokenFeed, vars: &mut Variables) -> Result<SolverInt, SolverError> {
    let cur = match feed.next()? {
        Some(x) => x,
        None => {
            return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                "Expected a token",
                FEED_OFFSET_END,
                None,
            )));
        }
    };

    // need to keep track if we need an = following an unset identifier
    let mut unset_ident = false;

    let mut num1 = match &cur {
        Token::Num(num) => *num,
        Token::Op(op) => match op {
            OpToken::Minus => -do_expression(OpToken::PRECEDENCE_NEG_TOK, feed, vars)?,
            OpToken::Open => {
                let res = do_expression(OpToken::PRECEDENCE_NO_PREC + 1, feed, vars)?;
                if let Some(next) = feed.next()? {
                    // we need a closing token here
                    match next {
                        Token::Op(op) if OpToken::Close == op => (),
                        _ => {
                            return Err(SolverError::UnbalancedBracketError(ErrorLocation::new(
                                "Expected a closing bracket",
                                feed.cur_col().expect("We should be able to peek a token here")
                                    - next.to_string().len()
                                    - FEED_OFFSET_BEHIND,
                                Some(")".to_string()),
                            )))
                        }
                    }
                } else {
                    return Err(SolverError::UnbalancedBracketError(ErrorLocation::new(
                        "Expected a closing bracket",
                        FEED_OFFSET_END,
                        Some(")".to_string()),
                    )));
                }

                res
            }
            _ => {
                return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                    "Unexpected token",
                    match feed.cur_col() {
                        Some(i) => i - FEED_OFFSET_ON,
                        None => usize::MAX,
                    },
                    None,
                )));
            }
        },
        Token::Var(ref ident) => {
            if let Some(num) = vars.get(&ident) {
                *num
            } else {
                unset_ident = true;
                0xDEADBEEF
            }
        }
    };

    while should_loop(precedence, feed)? {
        let cur_col = feed.cur_col();
        let next = feed.next()?.unwrap();
        let op = next.as_op();

        let num2 = do_expression(op.precedence() + op.prec_bump(), feed, vars)?;

        // special case this one for nice error printing
        if unset_ident && OpToken::Equal != op {
            return Err(SolverError::UnsetVariableError(ErrorLocation::new(
                "= must immediately follow an unset variable:",
                cur_col.expect("We should be able to peek a token here") - next.to_string().len(),
                Some(cur.as_var().to_string()),
            )));
        }

        match op {
            OpToken::Plus => num1 += num2,
            OpToken::Minus => num1 -= num2,
            OpToken::Mul => num1 *= num2,
            OpToken::Div => num1 /= num2,
            OpToken::Mod => num1 %= num2,
            OpToken::Exp => num1 = num1.pow(u32::try_from(num2)?),
            OpToken::Equal => {
                num1 = num2;
                vars.update(cur.as_var(), num1);
                unset_ident = false;
            }
            OpToken::BitAnd => num1 &= num2,
            OpToken::BitOr => num1 |= num2,
            _ => {
                unreachable!("missed handling on op... save the equation and write a test");
            }
        }
    }

    // if we are finished with this level of the expression tree all
    // variables at this point must be resolved
    if unset_ident {
        return Err(SolverError::UnsetVariableError(ErrorLocation::new(
            "Unset variable in expression:",
            match feed.cur_col() {
                Some(i) => i - FEED_OFFSET_VAR_ON,
                None => usize::MAX,
            },
            Some(cur.as_var().to_string()),
        )));
    }

    Ok(num1)
}

pub fn solve<S: Into<String>>(expr: S, vars: Option<&mut Variables>) -> Result<SolverInt, SolverError> {
    let mut tokenfeed = TokenFeed::new(expr.into());
    let mut vars = vars;

    let res = match vars {
        Some(ref mut variables) => do_expression(OpToken::PRECEDENCE_NO_PREC, &mut tokenfeed, variables),
        None => do_expression(OpToken::PRECEDENCE_NO_PREC, &mut tokenfeed, &mut Variables::new()),
    };

    if let Ok(num) = res {
        if let Ok(None) = tokenfeed.next() {
            if let Some(v) = vars {
                v.update(v.result_identifier(), num);
            }

            Ok(num)
        } else {
            Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                "Unused tokens",
                FEED_OFFSET_END,
                None,
            )))
        }
    } else {
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const OPS: [OpToken; 11] = [
        OpToken::Equal,
        OpToken::Plus,
        OpToken::Minus,
        OpToken::Mul,
        OpToken::Div,
        OpToken::Mod,
        OpToken::Exp,
        OpToken::Open,
        OpToken::Close,
        OpToken::BitAnd,
        OpToken::BitOr,
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

    fn call_eq<S: Into<String>>(expr: S, expected: SolverInt, vars: Option<&mut Variables>) -> Result<(), TestError> {
        let expr = expr.into();
        println!("running [{}]", expr);
        let actual = solve(&expr, vars);
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
            let res = solve($equation, None);
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
        // make sure to add the enum variant to the array
        for op in OPS.iter() {
            match op {
                OpToken::Equal => call_eq("a = 5", 5, None)?,
                OpToken::Plus => call_eq("5 + 5", 10, None)?,
                OpToken::Minus => call_eq("-5 -2", -7, None)?,
                OpToken::Mul => call_eq("12*15", 180, None)?,
                OpToken::Div => call_eq("12/3", 4, None)?,
                OpToken::Mod => call_eq("8%5", 3, None)?,
                OpToken::Exp => call_eq("9^3", 729, None)?,
                OpToken::Open => call_eq("(5+1)*2", 12, None)?,
                OpToken::Close => call_eq("(3-7)/-2", 2, None)?,
                OpToken::BitAnd => call_eq("0xFF & 0xA1", 0xA1, None)?,
                OpToken::BitOr => call_eq("0b1100 | 0b0011", 0b1111, None)?,
            }
        }

        Ok(())
    }

    #[test]
    fn test_exp_right_assoc() -> Result<(), TestError> {
        call_eq("2^3^2", 512, None)?;
        Ok(())
    }

    #[test]
    fn test_0_padding() -> Result<(), TestError> {
        call_eq("00000100 * 110011", 11001100, None)?;
        call_eq("0 + 5", 5, None)?;
        call_eq("0 * -1", 0, None)?;
        Ok(())
    }

    #[test]
    fn test_hex_and_bin() -> Result<(), TestError> {
        let inputs = [
            ("0x55 - 0b1101101", -24),
            ("0x055 - 0b0000001101101", -24),
            ("0x16 - 0x0A1", -139),
            ("0B101 - 0b010", 3),
            ("0X1E - 0xFF", -225),
        ];

        for input in inputs.iter() {
            call_eq(input.0, input.1, None)?;
        }

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
            ("-5 ^ 3 * -4 + 7 - -10", 517),
        ];

        for input in inputs.iter() {
            call_eq(input.0, input.1, None)?;
        }

        Ok(())
    }

    #[test]
    fn test_parse_errors() -> Result<(), TestError> {
        expect_error!("2 - 2a$f", SolverError::InvalidExpressionError);
        expect_error!("(3+1) + ) + 2", SolverError::InvalidExpressionError);
        expect_error!("((3+1)", SolverError::UnbalancedBracketError);
        expect_error!("(3+1 1*2", SolverError::UnbalancedBracketError);
        expect_error!("5 *2 - a$sdf$", SolverError::InvalidExpressionError);
        expect_error!("2^-1", SolverError::ParseUnsignedPowError);
        expect_error!("", SolverError::InvalidExpressionError);
        expect_error!("3++2", SolverError::InvalidExpressionError);
        expect_error!("(6+1", SolverError::UnbalancedBracketError);
        expect_error!("3 2", SolverError::InvalidExpressionError);
        expect_error!("6+1)", SolverError::InvalidExpressionError);
        expect_error!("15 + a", SolverError::UnsetVariableError);
        expect_error!("a", SolverError::UnsetVariableError);

        Ok(())
    }

    #[test]
    fn test_res_var_is_updated() -> Result<(), TestError> {
        let mut vars = Variables::new();
        let res_ident = vars.result_identifier();

        call_eq("5*5", 25, Some(&mut vars))?;
        assert_eq!(25, *vars.get(&res_ident).unwrap());

        call_eq("3-2", 1, Some(&mut vars))?;
        assert_eq!(1, *vars.get(&res_ident).unwrap());

        Ok(())
    }

    #[test]
    fn test_variables() -> Result<(), TestError> {
        let mut vars = Variables::new();
        let res_ident = vars.result_identifier();

        let a = "a".to_string();
        let b = "b".to_string();
        let c = "c".to_string();
        let d = "d".to_string();

        call_eq(format!("{a}=15", a = a), 15, Some(&mut vars))?;
        assert_eq!(vars.get(&Identifier(a.clone())).unwrap(), vars.get(&res_ident).unwrap());

        call_eq(format!("{b} = {a} * _", b = b, a = a), 225, Some(&mut vars))?;
        assert_eq!(vars.get(&Identifier(b.clone())).unwrap(), vars.get(&res_ident).unwrap());

        call_eq(format!("{c} = {b}/({a}^3/_) * 10", c = c, b = b, a = a), 150, Some(&mut vars))?;
        assert_eq!(*vars.get(&Identifier(c)).unwrap(), 150);

        call_eq(format!("{d}=4", d = d), 4, Some(&mut vars))?;
        call_eq(format!("{d}/{d}+5", d = d), 6, Some(&mut vars))?;

        Ok(())
    }
}
