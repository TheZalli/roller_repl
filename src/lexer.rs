use std::str::CharIndices;

use regex::{Regex};

use op::OpCode;

#[derive(Debug)]
pub struct Lexer {
    token_rules: Vec<(Regex, Token)>,
}

#[derive(Debug)]
pub struct LexerIter<'a, 'b> {
    lexer: &'a Lexer,
    input: &'b str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LParen, // (
    RParen, // )
    Comma, // ,
    Colon, // :
    Minus, // -, here because it can be unary or binary
    Op(OpCode),
    Int,
    Float,
    Str,
    Id,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter,
    UnexpectedEnd,
}

impl Default for Lexer {
    fn default() -> Self {
        // regex string and token pairs
        let unbuilt = vec![
            (r"\(", Token::LParen),
            (r"\)", Token::RParen),
            (r",", Token::Comma),
            (r":", Token::Colon),
            (r"-", Token::Minus),

            (r"+", Token::Op(OpCode::Add)),
            (r"*", Token::Op(OpCode::Mul)),
            (r"/", Token::Op(OpCode::Div)),
            (r"\^", Token::Op(OpCode::Exp)),

            (r"=", Token::Op(OpCode::Assign)),

            (r"\.", Token::Op(OpCode::Dot)),
            (r"\|", Token::Op(OpCode::Alternate)),
            // TODO add other ops

            (r"[0-9]+", Token::Int),
            (r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?", Token::Float),
            (r#"".*?[^\\]""#, Token::Str),
            (r"[\pL_][\pL\pN_]*", Token::Id),
        ];
        
        let mut rules = Vec::with_capacity(unbuilt.len());
        for (s, tok) in unbuilt {
            let re = Regex::new(&format!("^({})", s)).unwrap();
            rules.push((re, tok));
        }

        Lexer {
            token_rules: rules,
        }
    }
}

impl<'a, 'b> LexerIter<'a, 'b> {
    pub fn new(lexer: &'a Lexer, input: &'b str) -> Self {
        LexerIter {
            lexer: lexer,
            input: input,
        }
    }
}

impl<'a, 'b> Iterator for LexerIter<'a, 'b> {
    type Item = Result<(usize, Token, usize), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!();
    }
}
