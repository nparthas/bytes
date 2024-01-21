use std::collections;
use std::convert::TryFrom;
use std::fmt;
use std::iter;
use std::mem;
use std::num::TryFromIntError;
use std::str;

use crate::expression::solverint::{SolverInt, SolverType};

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
    ParseTooLargeNumError(ErrorLocation),
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
            SolverError::ParseTooLargeNumError(e) => e.fmt(f),
            SolverError::InvalidExpressionError(e) => e.fmt(f),
            SolverError::UnbalancedBracketError(e) => e.fmt(f),
            SolverError::UnsetVariableError(e) => e.fmt(f),
            SolverError::NotImplementedError => write!(f, "Not implemented"),
        }
    }
}

#[derive(Debug)]
pub struct Variables {
    solver_type: SolverType,
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

    pub fn solver_type(&self) -> SolverType {
        self.solver_type
    }

    pub fn clear(&mut self) {
        self.vars.clear();
    }

    pub fn print<F>(&self, f: F)
    where
        F: Fn(SolverInt) -> String,
    {
        if let Some(&v) = self.vars.get(&self.result_identifier()) {
            println!("{:>6} = {}", self.result_identifier(), f(v));
        }

        for (k, v) in &self.vars {
            if *k == self.result_identifier() {
                continue;
            }

            println!("{:>6} = {}", k, f(*v));
        }
    }
}

impl Default for Variables {
    fn default() -> Self {
        Variables {
            vars: collections::HashMap::new(),
            res_ident: Identifier("_".to_string()),
            solver_type: Default::default(),
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
enum Func {
    Log2,
    Popcnt,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Func::Log2 => write!(f, "log2($x)"),
            Func::Popcnt => write!(f, "popcnt($x)"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum OpToken {
    Assignment,
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
    BitXor,
    BitNot,
    BitShiftRight,
    BitShiftLeft,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Greater,
    GreaterEq,
    Lesser,
    LesserEq,
    Equal,
    NotEqual,
    TernaryQuestion,
    TernaryColon,
    Function(Func),
}

impl OpToken {
    const PRECEDENCE_NO_PREC: i32 = 0;
    const PRECEDENCE_NEG_TOK: i32 = 13;
    const OPS: [OpToken; 28] = [
        OpToken::Assignment,
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
        OpToken::BitXor,
        OpToken::BitNot,
        OpToken::BitShiftRight,
        OpToken::BitShiftLeft,
        OpToken::LogicalAnd,
        OpToken::LogicalOr,
        OpToken::LogicalNot,
        OpToken::Greater,
        OpToken::GreaterEq,
        OpToken::Lesser,
        OpToken::LesserEq,
        OpToken::Equal,
        OpToken::NotEqual,
        OpToken::TernaryQuestion,
        OpToken::TernaryColon,
        OpToken::Function(Func::Log2), // we only have log2 for now, but we can expand the parser if there are more options, will need a `,` token
        OpToken::Function(Func::Popcnt), // we only have log2 for now, but we can expand the parser if there are more options, will need a `,` token
    ];

    fn precedence(&self) -> i32 {
        match &self {
            // PRECEDENCE_NO_PREC goes here (0)
            OpToken::Assignment => 1,
            OpToken::TernaryQuestion => 2,
            OpToken::TernaryColon => 3,
            OpToken::LogicalOr => 4,
            OpToken::LogicalAnd => 5,
            OpToken::BitOr => 6,
            OpToken::BitXor => 7,
            OpToken::BitAnd => 8,
            OpToken::Equal | OpToken::NotEqual => 9,
            OpToken::Greater | OpToken::GreaterEq | OpToken::Lesser | OpToken::LesserEq => 10,
            OpToken::BitShiftRight | OpToken::BitShiftLeft => 11,
            OpToken::Plus | OpToken::Minus => 12,
            // PRECEDENCE_NEG_TOK goes here (13)
            OpToken::Mul | OpToken::Div | OpToken::Mod => 14,
            OpToken::BitNot => 15,
            OpToken::LogicalNot => 16,
            OpToken::Exp => 17,
            OpToken::Open | OpToken::Close => 18,
            OpToken::Function(_) => 19,
        }
    }

    fn is_binary(&self) -> bool {
        match &self {
            OpToken::Open | OpToken::Close | OpToken::BitNot | OpToken::LogicalNot | OpToken::Function(_) => false,
            OpToken::Assignment
            | OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Exp
            | OpToken::BitAnd
            | OpToken::BitOr
            | OpToken::BitXor
            | OpToken::BitShiftRight
            | OpToken::BitShiftLeft
            | OpToken::LogicalAnd
            | OpToken::LogicalOr
            | OpToken::Greater
            | OpToken::GreaterEq
            | OpToken::Lesser
            | OpToken::LesserEq
            | OpToken::Equal
            | OpToken::NotEqual
            | OpToken::TernaryQuestion
            | OpToken::TernaryColon => true,
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
            OpToken::Assignment | OpToken::Exp => true,
            OpToken::Plus
            | OpToken::Minus
            | OpToken::Mul
            | OpToken::Div
            | OpToken::Mod
            | OpToken::Open
            | OpToken::Close
            | OpToken::BitAnd
            | OpToken::BitOr
            | OpToken::BitXor
            | OpToken::BitNot
            | OpToken::BitShiftRight
            | OpToken::BitShiftLeft
            | OpToken::LogicalAnd
            | OpToken::LogicalOr
            | OpToken::LogicalNot
            | OpToken::Greater
            | OpToken::GreaterEq
            | OpToken::Lesser
            | OpToken::LesserEq
            | OpToken::Equal
            | OpToken::NotEqual
            | OpToken::TernaryQuestion
            | OpToken::TernaryColon
            | OpToken::Function(_) => false,
        }
    }
}

impl fmt::Display for OpToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            match &self {
                OpToken::Assignment => write!(f, "{:20}=", "Assignment:"),
                OpToken::Exp => write!(f, "{:20}#", "Exponent:"),
                OpToken::Plus => write!(f, "{:20}+", "Plus:"),
                OpToken::Minus => write!(f, "{:20}-", "Subtract:"),
                OpToken::Mul => write!(f, "{:20}*", "Multiply:"),
                OpToken::Div => write!(f, "{:20}/", "(Integer) Divide:"),
                OpToken::Mod => write!(f, "{:20}%", "Modulo:"),
                OpToken::Open => write!(f, "{:20}(", "Open Bracket:"),
                OpToken::Close => write!(f, "{:20})", "Close Bracket:"),
                OpToken::BitAnd => write!(f, "{:20}&", "Bitwise And:"),
                OpToken::BitOr => write!(f, "{:20}|", "Bitwise Or:"),
                OpToken::BitXor => write!(f, "{:20}^", "Bitwise Xor:"),
                OpToken::BitNot => write!(f, "{:20}~", "Bitwise Not:"),
                OpToken::BitShiftRight => write!(f, "{:20}>>", "Bit Shift Right:"),
                OpToken::BitShiftLeft => write!(f, "{:20}<<", "Bit Shift Left:"),
                OpToken::LogicalAnd => write!(f, "{:20}&&", "Logical And:"),
                OpToken::LogicalOr => write!(f, "{:20}||", "Logical Or:"),
                OpToken::LogicalNot => write!(f, "{:20}!", "Logical Not:"),
                OpToken::Greater => write!(f, "{:20}>", "Greater:"),
                OpToken::GreaterEq => write!(f, "{:20}>=", "Greater Eq:"),
                OpToken::Lesser => write!(f, "{:20}<", "Lesser:"),
                OpToken::LesserEq => write!(f, "{:20}<=", "Lesser Eq:"),
                OpToken::Equal => write!(f, "{:20}==", "Equal:"),
                OpToken::NotEqual => write!(f, "{:20}!=", "Not Equal:"),
                OpToken::TernaryQuestion => write!(f, "{:20}()?", "Ternary Condition:"),
                OpToken::TernaryColon => write!(f, "{:20}:", "Ternary Options:"),
                OpToken::Function(func) => write!(f, "{:20}{}", "Function", func),
            }
        } else {
            match &self {
                OpToken::Assignment => write!(f, "="),
                OpToken::Exp => write!(f, "#"),
                OpToken::Plus => write!(f, "+"),
                OpToken::Minus => write!(f, "-"),
                OpToken::Mul => write!(f, "*"),
                OpToken::Div => write!(f, "/"),
                OpToken::Mod => write!(f, "%"),
                OpToken::Open => write!(f, "("),
                OpToken::Close => write!(f, ")"),
                OpToken::BitAnd => write!(f, "&"),
                OpToken::BitOr => write!(f, "|"),
                OpToken::BitXor => write!(f, "^"),
                OpToken::BitNot => write!(f, "~"),
                OpToken::BitShiftRight => write!(f, ">>"),
                OpToken::BitShiftLeft => write!(f, "<<"),
                OpToken::LogicalAnd => write!(f, "&&"),
                OpToken::LogicalOr => write!(f, "||"),
                OpToken::LogicalNot => write!(f, "!"),
                OpToken::Greater => write!(f, ">"),
                OpToken::GreaterEq => write!(f, ">="),
                OpToken::Lesser => write!(f, "<"),
                OpToken::LesserEq => write!(f, "<="),
                OpToken::Equal => write!(f, "=="),
                OpToken::NotEqual => write!(f, "!="),
                OpToken::TernaryQuestion => write!(f, "()?"),
                OpToken::TernaryColon => write!(f, ":"),
                OpToken::Function(_) => write!(f, "$func(...)"),
            }
        }
    }
}

pub fn print_ops() {
    for op in &OpToken::OPS {
        println!("{:#}", op);
    }
}

#[derive(Debug)]
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
            return self._peek.take().unwrap();
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
                '#' => return ok_some!(Token::Op(OpToken::Exp)),
                '%' => return ok_some!(Token::Op(OpToken::Mod)),
                '=' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("==", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::Equal));
                    }

                    return ok_some!(Token::Op(OpToken::Assignment));
                }
                '&' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("&&", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::LogicalAnd));
                    }

                    return ok_some!(Token::Op(OpToken::BitAnd));
                }
                '|' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("||", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::LogicalOr));
                    }

                    return ok_some!(Token::Op(OpToken::BitOr));
                }
                '^' => return ok_some!(Token::Op(OpToken::BitXor)),
                '~' => return ok_some!(Token::Op(OpToken::BitNot)),
                '!' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("!=", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::NotEqual));
                    }
                    return ok_some!(Token::Op(OpToken::LogicalNot));
                }
                '>' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok(">>", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::BitShiftRight));
                    }

                    if let Ok(()) = TokenFeed::match_multichar_tok(">=", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::GreaterEq));
                    }

                    return ok_some!(Token::Op(OpToken::Greater));
                }
                '<' => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("<<", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::BitShiftLeft));
                    }

                    if let Ok(()) = TokenFeed::match_multichar_tok("<=", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::LesserEq));
                    }

                    return ok_some!(Token::Op(OpToken::Lesser));
                }
                '?' => {
                    return ok_some!(Token::Op(OpToken::TernaryQuestion));
                }
                ':' => {
                    return ok_some!(Token::Op(OpToken::TernaryColon));
                }
                '0'..='9' => return ok_some!(Token::Num(TokenFeed::extract_number(c.1, &mut self.itr)?)),
                w if w.is_alphabetic() || '_' == w => {
                    if let Ok(()) = TokenFeed::match_multichar_tok("log2", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::Function(Func::Log2)));
                    }
                    if let Ok(()) = TokenFeed::match_multichar_tok("popcnt", c.0, c.1, &mut self.itr) {
                        return ok_some!(Token::Op(OpToken::Function(Func::Popcnt)));
                    }

                    return ok_some!(Token::Var(TokenFeed::extract_identifer(c.1, &mut self.itr)));
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
    fn extract_number(c: char, itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>) -> Result<SolverInt, SolverError> {
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
                    return Ok(SolverInt::from_str_radix(s.as_str(), radix).unwrap());
                }
            }
        }

        match radix {
            16 => extract_on_radix!('0'..='9' | 'a'..='f' | 'A'..='F', s, itr),
            10 => extract_on_radix!('0'..='9', s, itr),
            2 => extract_on_radix!('0'..='1', s, itr),
            _ => unreachable!("Unsupported radix"),
        }

        if (radix == 2 || radix == 16) && s.len() == 1 {
            let loc = if let Some(i) = itr.peek() { i.0 } else { FEED_OFFSET_END };
            let error = if radix == 2 {
                "Empty binary number"
            } else {
                "Empty hex number"
            };

            return Err(SolverError::InvalidExpressionError(ErrorLocation { error, loc, hint: None }));
        }

        match SolverInt::from_str_radix(s.as_str(), radix) {
            Ok(res) => Ok(res),
            Err(e) => {
                assert!(e.kind() == &std::num::IntErrorKind::PosOverflow);
                let loc = if let Some(i) = itr.peek() { i.0 } else { FEED_OFFSET_END };

                Err(SolverError::ParseTooLargeNumError(ErrorLocation {
                    error: "Provided integer too large",
                    loc,
                    hint: None,
                }))
            }
        }
    }

    fn extract_identifer(c: char, itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>) -> Identifier {
        let mut s = String::with_capacity(20);
        s.push(c);

        while let Some(c) = itr.peek() {
            if c.1.is_whitespace() || (!c.1.is_alphabetic() && '_' != c.1) {
                break;
            }

            s.push(itr.next().unwrap().1);
        }

        Identifier(s)
    }

    // expects the full token match in tok, even if c is already matched to tok[0]
    fn match_multichar_tok(
        tok: &'static str,
        cur_pos: usize,
        c: char,
        itr: &mut iter::Peekable<iter::Enumerate<str::Chars>>,
    ) -> Result<(), SolverError> {
        let mut tok_itr = tok.chars();
        let mut pos = cur_pos;

        // sanity in case this isn't called in TokenFeed::next
        match tok_itr.next() {
            Some(t) => {
                if t != c {
                    return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                        "Invalid char in expression",
                        pos,
                        Some(c.to_string()),
                    )));
                }
            }
            None => {
                return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                    "Invalid token, did you mean:",
                    pos,
                    Some(tok.to_string()),
                )));
            }
        }

        pos += 1;

        for t in tok_itr {
            if itr.peek().is_none() || t != itr.peek().unwrap().1 {
                return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                    "Invalid token, did you mean:",
                    pos,
                    Some(tok.to_string()),
                )));
            }

            itr.next();
            pos += 1;
        }

        Ok(())
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
            OpToken::Minus => do_expression(OpToken::PRECEDENCE_NEG_TOK, feed, vars)?.twosc(),
            OpToken::BitNot => !do_expression(OpToken::BitNot.precedence(), feed, vars)?,
            OpToken::LogicalNot => {
                if do_expression(OpToken::LogicalNot.precedence(), feed, vars)? != 0 {
                    0
                } else {
                    1
                }
            }
            OpToken::Open | OpToken::Function(_) => {
                // if we have a function just skip that token for now
                if let OpToken::Function(_) = op {
                    feed.next()?;
                }

                let mut res = do_expression(OpToken::PRECEDENCE_NO_PREC + 1, feed, vars)?;
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
                    // this is on the only place we can start a ternary expression
                    if let Ok(Some(Token::Op(OpToken::TernaryQuestion))) = feed.peek() {
                        feed.next().unwrap();
                        let n1 = do_expression(OpToken::TernaryColon.precedence() + 1, feed, vars)?;
                        if let Some(Token::Op(OpToken::TernaryColon)) = feed.next()? {
                            let n2 = do_expression(OpToken::TernaryColon.precedence() + 1, feed, vars)?;
                            if res != 0 {
                                res = n1
                            } else {
                                res = n2
                            }
                        } else {
                            return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                                "Expected : after ?",
                                FEED_OFFSET_END,
                                Some(":".to_string()),
                            )));
                        }
                    }
                } else {
                    return Err(SolverError::UnbalancedBracketError(ErrorLocation::new(
                        "Expected a closing bracket",
                        FEED_OFFSET_END,
                        Some(")".to_string()),
                    )));
                }

                if let OpToken::Function(func) = op {
                    match func {
                        Func::Log2 => res = TwosC::log2(res),
                        Func::Popcnt => res = res.count_ones().try_into().unwrap(),
                    }
                }

                res
            }
            op => {
                return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                    "Unexpected token",
                    match feed.cur_col() {
                        Some(i) => i.saturating_sub(FEED_OFFSET_ON),
                        None => usize::MAX,
                    },
                    Some(op.to_string()),
                )));
            }
        },
        Token::Var(ref ident) => {
            if let Some(num) = vars.get(ident) {
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
        if unset_ident && OpToken::Assignment != op {
            return Err(SolverError::UnsetVariableError(ErrorLocation::new(
                "= must immediately follow an unset variable:",
                cur_col.expect("We should be able to peek a token here") - next.to_string().len(),
                Some(cur.to_string()),
            )));
        }

        match op {
            OpToken::Plus => num1 = num1.wrapping_add(num2),
            OpToken::Minus => num1 = num1.wrapping_sub(num2),
            OpToken::Mul => num1 = num1.wrapping_mul(num2),
            OpToken::Div => num1 = num1.div(num2), // intentional
            OpToken::Mod => num1 = num1.wrapping_rem(num2),
            OpToken::Exp => num1 = num1.wrapping_pow(u32::try_from(num2)?),
            OpToken::Assignment => {
                // check that cur/num1 is actually a variable
                if let Token::Var(_) = cur {
                    num1 = num2;
                    vars.update(cur.as_var(), num1);
                    unset_ident = false;
                } else {
                    return Err(SolverError::InvalidExpressionError(ErrorLocation::new(
                        "cannot assign to a literal:",
                        cur_col.expect("We should be able to peek a token here") - next.to_string().len(),
                        Some(cur.to_string()),
                    )));
                }
            }
            OpToken::BitAnd => num1 &= num2,
            OpToken::BitOr => num1 |= num2,
            OpToken::BitXor => num1 ^= num2,
            OpToken::BitShiftRight => num1 >>= num2,
            OpToken::BitShiftLeft => num1 <<= num2,
            OpToken::LogicalAnd => num1 = if num1 != 0 && num2 != 0 { 1 } else { 0 },
            OpToken::LogicalOr => num1 = if num1 != 0 || num2 != 0 { 1 } else { 0 },
            OpToken::Greater => num1 = if num1 > num2 { 1 } else { 0 },
            OpToken::GreaterEq => num1 = if num1 >= num2 { 1 } else { 0 },
            OpToken::Lesser => num1 = if num1 < num2 { 1 } else { 0 },
            OpToken::LesserEq => num1 = if num1 <= num2 { 1 } else { 0 },
            OpToken::Equal => num1 = if num1 == num2 { 1 } else { 0 },
            OpToken::NotEqual => num1 = if num1 != num2 { 1 } else { 0 },

            _ => {
                unreachable!("missed handling on op [`{}`]... save the equation and write a test", op);
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

pub fn solve<S: Into<String>>(expr: S, vars: &mut Variables) -> Result<SolverInt, SolverError> {
    let mut tokenfeed = TokenFeed::new(expr.into());
    let mut vars = vars;

    let res = do_expression(OpToken::PRECEDENCE_NO_PREC, &mut tokenfeed, vars);

    if let Ok(num) = res {
        if let Ok(None) = tokenfeed.next() {
            vars.update(vars.result_identifier(), num);
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

    #[allow(dead_code)]
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
        let actual = match vars {
            Some(ref mut variables) => solve(&expr, variables),
            None => solve(&expr, &mut Variables::new()),
        };

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
            let res = solve($equation, &mut Variables::new());
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
        for op in OpToken::OPS.iter() {
            match op {
                OpToken::Assignment => call_eq("a = 5", 5, None)?,
                OpToken::Plus => call_eq("5 + 5", 10, None)?,
                OpToken::Minus => call_eq("-5 -2", 7.twosc(), None)?,
                OpToken::Mul => call_eq("12*15", 180, None)?,
                OpToken::Div => call_eq("12/3", 4, None)?,
                OpToken::Mod => call_eq("8%5", 3, None)?,
                OpToken::Exp => call_eq("9#3", 729, None)?,
                OpToken::Open => call_eq("(5+1)*2", 12, None)?,
                OpToken::Close => call_eq("(3-7)/-2", 2, None)?,
                OpToken::BitAnd => call_eq("0xFF & 0xA1", 0xA1, None)?,
                OpToken::BitOr => call_eq("0b1100 | 0b0011", 0b1111, None)?,
                OpToken::BitXor => call_eq("0b1100 ^ 0b1010", 0b0110, None)?,
                OpToken::BitNot => call_eq("~0", 1.twosc(), None)?,
                OpToken::BitShiftRight => call_eq("0b101010 >> 2", 0b1010, None)?,
                OpToken::BitShiftLeft => call_eq("0b11 << 8", 0b1100000000, None)?,
                OpToken::LogicalAnd => call_eq("1 && 0", 0, None)?,
                OpToken::LogicalOr => call_eq("0 || 1", 1, None)?,
                OpToken::LogicalNot => call_eq("!0", 1, None)?,
                OpToken::Greater => call_eq("5 > 5", 0, None)?,
                OpToken::GreaterEq => call_eq("5 >= 5", 1, None)?,
                OpToken::Lesser => call_eq("2 < 2", 0, None)?,
                OpToken::LesserEq => call_eq("2 <= 2", 1, None)?,
                OpToken::Equal => call_eq("0b1111 == 0xf", 1, None)?,
                OpToken::NotEqual => call_eq("1 != !1", 1, None)?,
                OpToken::TernaryQuestion => call_eq("(5)? (0)? 1 : 2 : (3)? 4 : 5", 2, None)?,
                OpToken::TernaryColon => call_eq("(15)? 1 : 2", 1, None)?,
                OpToken::Function(Func::Log2) => call_eq("log2(16)", 4, None)?,
                OpToken::Function(Func::Popcnt) => call_eq("popcnt(15)", 4, None)?,
            }
        }

        Ok(())
    }

    #[test]
    fn test_div() -> Result<(), TestError> {
        // test the 4 cases of signs in division

        call_eq("4/2", 2, None)?;
        call_eq("-27/7", 3.twosc(), None)?;
        call_eq("72/-8", 9.twosc(), None)?;
        call_eq("-729/-9", 81, None)?;

        Ok(())
    }

    #[test]
    fn test_exp_right_assoc() -> Result<(), TestError> {
        call_eq("2#3#2", 512, None)?;
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
            ("0x55 - 0b1101101", 24.twosc()),
            ("0x055 - 0b0000001101101", 24.twosc()),
            ("0x16 - 0x0A1", 139.twosc()),
            ("0B101 - 0b010", 3),
            ("0X1E - 0xFF", 225.twosc()),
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
            ("(2*3)#2", 36),
            ("(1-2)*5", 5.twosc()),
            ("3*(8/2) + 1", 13),
            ("-5 * (-3+1)/2", 5),
            ("((2+1)*4)#2", 144),
            ("36/(1+2)", 12),
            ("5*2 +1", 11),
            ("5*5 - 1 #10 * 55000 / 100 ", 525.twosc()),
            ("6 + (16 - 4)/(2#2 + 2) - 2", 6),
            ("(4 + 8)/(2 + 1) - (3 - 1) + 2", 4),
            ("-5 # 3 * -4 + 7 - -10", 517),
            ("0b1001 | 0b0110 ^  0b0010 << 1 & ~0b1001", 0b1011),
            ("1 || 1 && !1", 1),
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
        expect_error!("2#-1", SolverError::ParseUnsignedPowError);
        expect_error!("", SolverError::InvalidExpressionError);
        expect_error!("3++2", SolverError::InvalidExpressionError);
        expect_error!("(6+1", SolverError::UnbalancedBracketError);
        expect_error!("3 2", SolverError::InvalidExpressionError);
        expect_error!("6+1)", SolverError::InvalidExpressionError);
        expect_error!("15 + a", SolverError::UnsetVariableError);
        expect_error!("a", SolverError::UnsetVariableError);
        expect_error!("1 = 2", SolverError::InvalidExpressionError);
        expect_error!("* = 1", SolverError::InvalidExpressionError);
        expect_error!("0x + 10", SolverError::InvalidExpressionError);
        expect_error!("0b", SolverError::InvalidExpressionError);
        expect_error!("(1)? a = 2 : 3", SolverError::UnsetVariableError);

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

        let a = "a_a".to_string();
        let b = "b".to_string();
        let c = "c".to_string();
        let d = "d".to_string();
        let e = "d".to_string();

        call_eq(format!("{a}=15", a = a), 15, Some(&mut vars))?;
        assert_eq!(vars.get(&Identifier(a.clone())).unwrap(), vars.get(&res_ident).unwrap());

        call_eq(format!("{b} = {a} * _", b = b, a = a), 225, Some(&mut vars))?;
        assert_eq!(vars.get(&Identifier(b.clone())).unwrap(), vars.get(&res_ident).unwrap());

        call_eq(format!("{c} = {b}/({a}#3/_) * 10", c = c, b = b, a = a), 150, Some(&mut vars))?;
        assert_eq!(*vars.get(&Identifier(c)).unwrap(), 150);

        call_eq(format!("{d}=4", d = d), 4, Some(&mut vars))?;
        call_eq(format!("{d}/{d}+5", d = d), 6, Some(&mut vars))?;

        call_eq(
            format!("{e}=0b1001 | 0b0110 ^  0b0100 & ~0b1001", e = e),
            0b1011,
            Some(&mut vars),
        )?;
        assert_eq!(*vars.get(&Identifier(e)).unwrap(), 0b1011);

        Ok(())
    }

    #[test]
    fn test_wrapping() -> Result<(), TestError> {
        let inputs = [
            ("13#10", 137858491849),
            ("0xFFFFFFFFFFFFFFFF", 0xFFFFFFFFFFFFFFFF),
            ("0xFFFFFFFFFFFFFFFF + 1", 0),
            ("(1-2)*5", 5.twosc()),
            ("0xFFFFFFFFFFFFFFFF + 2", 1),
            ("~0", !0),
        ];

        for input in inputs.iter() {
            call_eq(input.0, input.1, None)?;
        }

        expect_error!("0xFFFFFFFFFFFFFFFFF", SolverError::ParseTooLargeNumError);
        Ok(())
    }
}
