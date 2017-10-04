use std::str::FromStr;

use regex::{Regex, RegexSet};
use num::FromPrimitive;
use num::rational::Ratio;

use op::{OpCode, CompOp};


/// Regex rules for matching tokens and the functions to create them
const DEFAULT_TOKEN_RULES: [(&'static str, &'static Fn(&str) -> Token); 25] = [
    (r"\(", &|_| Token::LParen),
    (r"\)", &|_| Token::RParen),
    (r"\[", &|_| Token::LBracket),
    (r"\]", &|_| Token::RBracket),
    (r"\{", &|_| Token::LBrace),
    (r"\}", &|_| Token::RBrace),

    (r"->", &|_| Token::RightArrow),

    (r"=", &|_| Token::Equals),
    (r"!=", &|_| Token::Comp(CompOp::Nequals)),
    (r"<", &|_| Token::Comp(CompOp::Lt)),
    (r"<=", &|_| Token::Comp(CompOp::Lte)),
    (r">", &|_| Token::Comp(CompOp::Gt)),
    (r">=", &|_| Token::Comp(CompOp::Gte)),

    (r"\+", &|_| Token::Op(OpCode::Add)),
    (r"\*", &|_| Token::Op(OpCode::Mul)),
    (r"/", &|_| Token::Op(OpCode::Div)),
    (r"\^", &|_| Token::Op(OpCode::Pow)),

    (r"\.", &|_| Token::Dot),
    (r",", &|_| Token::Comma),
    (r":", &|_| Token::Colon),
    (r"\|", &|_| Token::Alternate),
    (r"-", &|_| Token::Minus),

    // match numerals
    (r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?",
        &|s| Token::Num(Ratio::from_f32(f32::from_str(s).unwrap()).unwrap())
    ),

    // match strings
    (r#""(.*?[^\\])?""#, &|s| Token::Str(s[1..s.len()-1].to_owned())),
    
    // match identifiers and keywords
    (r"[\pL_][\pL\pN_]*", &|s| match s {
        "not" => Token::Not,
        "and" => Token::Op(OpCode::And),
        "or" => Token::Op(OpCode::Or),
        "xor" => Token::Op(OpCode::Xor),
        "is" => Token::Is,
        "in" => Token::In,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "loop" => Token::Loop,
        "while" => Token::While,
        "for" => Token::For,
        "try" => Token::Try,
        "catch" => Token::Catch,
        "throw" => Token::Throw,
        "fn" => Token::Fn,
        "none" => Token::None,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Id(s.to_owned()),
    }),
];

lazy_static! {
    static ref WS_STRIP_REGEX: Regex = Regex::from_str(r"^\s*").unwrap();
}

#[derive(Clone)]
pub struct Lexer {
    regex_set: RegexSet,
    token_rules: Vec<(Regex, &'static Fn(&str) -> Token)>,
}

#[derive(Clone)]
pub struct LexerIter<'a, 'b: 'a> {
    lexer: &'a Lexer,
    consumed: usize,
    input: &'b str,
}

/// The types of tokens present in the script
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `-`, here because it can be unary or binary
    Minus,
    /// Here because it can be a general keyword or a boolean unary operation
    Not,
    /// `=`
    Equals,
    /// Comparison operators, like `<`
    Comp(CompOp),
    /// Infix operators, like `+`
    Op(OpCode),
    /// Piecewise infix operators, prefixed by a dot, like `.+`
    DotOp(OpCode),
    /// Assignment operators, suffixed by an equals sign, like `+=`
    AssignOp(OpCode),
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `|`
    Alternate,
    /// `->`
    RightArrow,
    Is,
    In,
    If,
    Then,
    Else,
    Return,
    Break,
    Continue,
    Loop,
    While,
    For,
    Try,
    Catch,
    Throw,
    Fn,
    None,
    /// Boolean value, `true` or `false`
    Bool(bool),
    /// Numeral
    Num(Ratio<i32>),
    /// String literal
    Str(String),
    /// Identifier
    Id(String),
    // / End of line
    //Eol
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    InvalidToken,
}

impl Lexer {
    pub fn parse_iter<'a, 'b>(&'a self, input: &'b str) -> LexerIter<'a, 'b> {
        LexerIter {
            lexer: self,
            consumed: 0,
            input: input,
        }
    }
    pub fn parse(&self, input: &str) ->
        Result<Vec<(usize, Token, usize)>, LexError>
    {
        self.parse_iter(input).collect()
    }
}

impl<'a> Default for Lexer {
    fn default() -> Self {
        // read the data from DEFAULT_TOKEN_RULES
        let mut regexes = Vec::with_capacity(DEFAULT_TOKEN_RULES.len());
        let mut rules = Vec::with_capacity(DEFAULT_TOKEN_RULES.len());
        for &(ref s, ref tok_fn) in DEFAULT_TOKEN_RULES.iter() {
            let wrapped_re = format!("^({})", s);
            let re = Regex::new(&wrapped_re).unwrap();
            regexes.push(wrapped_re);
            rules.push((re, *tok_fn));
        }

        Lexer {
            regex_set: RegexSet::new(regexes).unwrap(),
            token_rules: rules,
        }
    }
}

impl<'a, 'b> Iterator for LexerIter<'a, 'b> {
    type Item = Result<(usize, Token, usize), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // strip left whitespace
        let ma = WS_STRIP_REGEX.find(self.input).unwrap();
        self.input = &self.input[ma.end()..];
        self.consumed += ma.end();

        if self.input.is_empty() {
            return None;
        }

        for found_index in self.lexer.regex_set.matches(self.input) {
            // found a match, now extract it since regex set can't do that
            let &(ref regex, ref tok_fun) = 
                &self.lexer.token_rules[found_index];
            
            let ma = regex.find(self.input).unwrap();

            // store previous used bytes
            let offset = self.consumed;
            // chop off used bytes
            self.input = &self.input[ma.end()..];
            // record chopped off bytes
            self.consumed += ma.end();

            // return the matched token
            return Some(Ok((
                offset + ma.start(),
                tok_fun(ma.as_str()),
                offset + ma.end()
            )));
            // other matches are ignored, if they even exist
        }

        // no rule matched
        self.input = ""; // stop on next iteration
        Some(Err(LexError::InvalidToken))
    }
}
