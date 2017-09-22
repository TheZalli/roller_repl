use std::str::FromStr;

use regex::{Regex, RegexSet};
use num::FromPrimitive;
use num::rational::Ratio;

use op::OpCode;

macro_rules! wrap_token_rule_fun {
    ($e:expr) => (&($e))
}

// regex string and token pairs
const DEFAULT_TOKEN_RULES: [(&'static str, &'static Fn(&str) -> Token); 23] = [
    (r"\(", wrap_token_rule_fun!(|_| Token::LParen)),
    (r"\)", wrap_token_rule_fun!(|_| Token::RParen)),
    (r"\[", wrap_token_rule_fun!(|_| Token::LBracket)),
    (r"\]", wrap_token_rule_fun!(|_| Token::RBracket)),
    (r"\{", wrap_token_rule_fun!(|_| Token::LBrace)),
    (r"\}", wrap_token_rule_fun!(|_| Token::RBrace)),

    (r"==", wrap_token_rule_fun!(|_| Token::Op(OpCode::Equals))),
    (r"\+", wrap_token_rule_fun!(|_| Token::Op(OpCode::Add))),
    (r"\*", wrap_token_rule_fun!(|_| Token::Op(OpCode::Mul))),
    (r"/", wrap_token_rule_fun!(|_| Token::Op(OpCode::Div))),
    (r"\^", wrap_token_rule_fun!(|_| Token::Op(OpCode::Pow))),

    (r"=", wrap_token_rule_fun!(|_| Token::Assign)),
    (r"\.", wrap_token_rule_fun!(|_| Token::Dot)),
    (r",", wrap_token_rule_fun!(|_| Token::Comma)),
    (r":", wrap_token_rule_fun!(|_| Token::Colon)),
    (r"\|", wrap_token_rule_fun!(|_| Token::Alternate)),
    (r"-", wrap_token_rule_fun!(|_| Token::Minus)),

    // TODO add other ops

    (r"none", wrap_token_rule_fun!(|_| Token::None)),
    (r"true", wrap_token_rule_fun!(|_| Token::Bool(true))),
    (r"false", wrap_token_rule_fun!(|_| Token::Bool(false))),

    (r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?", wrap_token_rule_fun!(
        |s| Token::Num(Ratio::from_f32(f32::from_str(s).unwrap()).unwrap())
    )),
    (r#""(.*?[^\\])?""#, wrap_token_rule_fun!(|s| Token::Str(s.to_owned()))),
    (r"[\pL_][\pL\pN_]*", wrap_token_rule_fun!(|s| Token::Id(s.to_owned()))),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LParen, // (
    RParen, // )
    LBracket, // [
    RBracket, // ]
    LBrace, // {
    RBrace, // }
    Minus, // -, here because it can be unary or binary
    Assign, // =
    Op(OpCode),
    DotOp(OpCode),
    AssignOp(OpCode),
    Dot, // .
    Comma, // ,
    Colon, // :
    Alternate, // |
    None, // none
    Bool(bool),
    Num(Ratio<i32>),
    Str(String),
    Id(String),
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
