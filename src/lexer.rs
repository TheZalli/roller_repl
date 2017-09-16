use std::str::{FromStr, CharIndices};

use regex::{Regex};
use num::FromPrimitive;
use num::rational::Ratio;

use op::OpCode;

macro_rules! wrap_token_rule_fun {
    ($e:expr) => (&($e))
}

// regex string and token pairs
const DEFAULT_TOKEN_RULES: [(&'static str, &'static Fn(&str) -> Token); 15] = [
    (r"\(", wrap_token_rule_fun!(|_| Token::LParen)),
    (r"\)", wrap_token_rule_fun!(|_| Token::RParen)),
    (r",", wrap_token_rule_fun!(|_| Token::Comma)),
    (r":", wrap_token_rule_fun!(|_| Token::Colon)),
    (r"\|", wrap_token_rule_fun!(|_| Token::Alternate)),
    (r"-", wrap_token_rule_fun!(|_| Token::Minus)),

    (r"\+", wrap_token_rule_fun!(|_| Token::Op(OpCode::Add))),
    (r"\*", wrap_token_rule_fun!(|_| Token::Op(OpCode::Mul))),
    (r"/", wrap_token_rule_fun!(|_| Token::Op(OpCode::Div))),
    (r"\^", wrap_token_rule_fun!(|_| Token::Op(OpCode::Exp))),

    (r"=", wrap_token_rule_fun!(|_| Token::Op(OpCode::Assign))),

    (r"\.", wrap_token_rule_fun!(|_| Token::Op(OpCode::Dot))),
    // TODO add other ops

    (r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?", wrap_token_rule_fun!(
        |s| Token::Num(Ratio::from_f64(f64::from_str(s).unwrap()).unwrap())
    )),
    (r#"".*?[^\\]""#, wrap_token_rule_fun!(|s| Token::Str(s))),
    (r"[\pL_][\pL\pN_]*", wrap_token_rule_fun!(|s| Token::Id(s))),
];

lazy_static! {
    static ref WS_STRIP_REGEX: Regex = Regex::from_str(r"^\s*").unwrap();
}

#[derive(Clone)]
pub struct Lexer {
    token_rules: Vec<(Regex, &'static Fn(&str) -> Token)>,
}

pub struct LexerIter<'a, 'b: 'a> {
    lexer: &'a Lexer,
    consumed: usize,
    input: &'b str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    LParen, // (
    RParen, // )
    Comma, // ,
    Colon, // :
    Alternate, // |
    Minus, // -, here because it can be unary or binary
    Op(OpCode),
    Num(Ratio<i64>),
    Str(&'a str),
    Id(&'a str),
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
}

impl<'a> Default for Lexer {
    fn default() -> Self {
        let mut rules = Vec::with_capacity(DEFAULT_TOKEN_RULES.len());
        for &(ref s, ref tok_fn) in DEFAULT_TOKEN_RULES.iter() {
            let re = Regex::new(&format!("^({})", s)).unwrap();
            rules.push((re, *tok_fn));
        }

        Lexer {
            token_rules: rules,
        }
    }
}

impl<'a, 'b> Iterator for LexerIter<'a, 'b> {
    type Item = Result<(usize, Token<'b>, usize), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // strip left whitespace
        let ma = WS_STRIP_REGEX.find(self.input).unwrap();
        self.input = &self.input[ma.end()..];
        self.consumed += ma.end();

        if self.input.is_empty() {
            return None;
        }

        for &(ref re, ref token_fn) in self.lexer.token_rules.iter() {
            let opt_ma = re.find(self.input);
            match opt_ma {
                Some(m) => {
                    // found match, take from start and return
                    self.input = &self.input[m.end()..];
                    let offs = self.consumed;
                    self.consumed += m.end();
                    return Some(Ok((
                        offs + m.start(),
                        token_fn(m.as_str()),
                        offs + m.end()
                    )));
                },
                // this regex didn't match try next
                None => continue,
            }
        }
        // none matched
        self.input = ""; // stop on next iteration
        Some(Err(LexError::InvalidToken))
    }
}
