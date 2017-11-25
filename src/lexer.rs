use std::str::FromStr;

use regex::{Regex, RegexSet};
use num::FromPrimitive;
use num::rational::Ratio;

use op::{OpCode, CompOp};

/// Regex rules for matching tokens and the functions to create them
const DEFAULT_TOKEN_RULES: [(&'static str, &'static Fn(&str) -> Token); 35] = [
    (r"\(", &|_| Token::LParen),
    (r"\)", &|_| Token::RParen),
    (r"\[", &|_| Token::LBracket),
    (r"\]", &|_| Token::RBracket),
    (r"\{", &|_| Token::LBrace),
    (r"\}", &|_| Token::RBrace),

    (r"->", &|_| Token::RightArrow),

    (r"<", &|_| Token::Comp(CompOp::Lt)),
    (r"<=", &|_| Token::Comp(CompOp::Lte)),
    (r">", &|_| Token::Comp(CompOp::Gt)),
    (r">=", &|_| Token::Comp(CompOp::Gte)),

    (r"=", &|_| Token::Op(OpCode::Assign)),

    (r"\+", &|_| Token::Op(OpCode::Add)),
    (r"\*", &|_| Token::Op(OpCode::Mul)),
    (r"/", &|_| Token::Op(OpCode::Div)),
    (r"\^", &|_| Token::Op(OpCode::Pow)),

    (r"\.\+", &|_| Token::DotOp(OpCode::Add)),
    (r"\.\*", &|_| Token::DotOp(OpCode::Mul)),
    (r"\./", &|_| Token::DotOp(OpCode::Div)),
    (r"\.\^", &|_| Token::DotOp(OpCode::Pow)),

    (r"=\+", &|_| Token::AssignOp(OpCode::Add)),
    (r"=\*", &|_| Token::AssignOp(OpCode::Mul)),
    (r"=/", &|_| Token::AssignOp(OpCode::Div)),
    (r"=\^", &|_| Token::AssignOp(OpCode::Pow)),

    (r"\.", &|_| Token::Dot),
    (r",", &|_| Token::Comma),
    (r":", &|_| Token::Colon),
    (r";", &|_| Token::Semicolon),
    (r"\|", &|_| Token::Alternate),
    (r"-", &|_| Token::Minus),

    (r"\\\s*$", &|_| Token::MetaContinueNextLine(None)),
    (r"\n", &|_| Token::End),

    // match numerals
    (r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?",
        &|s| Token::Num(Ratio::from_f32(f32::from_str(s).unwrap()).unwrap())
    ),

    // match strings
    (r#""([^\\"]|\\.)*""#, &|s| Token::Str(s[1..s.len()-1].to_owned())),

    // match identifiers and keywords
    (r"[\pL_][\pL\pN_]*", &|s| match s {
        "not" => Token::Not,
        "and" => Token::Op(OpCode::And),
        "or" => Token::Op(OpCode::Or),
        "xor" => Token::Op(OpCode::Xor),
        "is" => Token::Is,
        "isnt" => Token::Comp(CompOp::Nequals),
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
        "global" => Token::Global,
        "local" => Token::Local,
        "var" => Token::Var,
        "none" => Token::None,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Id(s.to_owned()),
    }),
];

lazy_static! {
    static ref WS_STRIP_REGEX: Regex = Regex::new(r"^[\s&&[^\n]]*").unwrap();

    static ref LINE_COMMENT_REGEX: Regex = Regex::new(r"^//").unwrap();
    static ref BLOCK_COMMENT_START_REGEX: Regex =
        Regex::new(r"^.*?/\*").unwrap();
    static ref BLOCK_COMMENT_END_REGEX: Regex =
        Regex::new(r"^.*?\*/").unwrap();
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
    line_num: usize,
    input: &'b str,
    terminate: bool,
    block_comment_depth: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexerState {
    line_num: usize,
    block_comment_depth: u8,
}

impl LexerState {
    /// Returns the default state for a REPL-input lexer.
    pub fn repl_default() -> Self {
        LexerState {
            line_num: 0,
            block_comment_depth: 0,
        }
    }

    /// Returns the default state for a file lexer.
    #[allow(dead_code)] // TODO delete when used
    pub fn file_default() -> Self {
        LexerState {
            line_num: 1,
            block_comment_depth: 0,
        }
    }
}

/// Location of a token in an input string
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    /// Line number.
    /// For REPL environment, this starts at 0, otherwise it starts at 1.
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}

impl Loc {
    pub fn new(line: usize, col_start: usize, col_end: usize) -> Self {
        Loc {
            line: line,
            col_start: col_start,
            col_end: col_end,
        }
    }
}

impl ::std::fmt::Debug for Loc {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "Loc{{{}}}", self)
    }
}

impl ::std::fmt::Display for Loc {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}:{}-{}", self.line, self.col_start, self.col_end)
    }
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
    /// `;`
    Semicolon,
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
    Global,
    Local,
    Var,
    None,
    /// Boolean value, `true` or `false`
    Bool(bool),
    /// Numeral
    Num(Ratio<i32>),
    /// String literal
    Str(String),
    /// Identifier
    Id(String),
    /// A meta-token that tells to continue and to parse the next line and to
    /// glue it after this one. Contains the state needed for the next line.
    MetaContinueNextLine(Option<LexerState>),
    /// End of expression.
    /// Returned on newlines without continuation and end-of-input.
    End
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    InvalidToken,
    MaximumCommentDepthReached,
}

impl Lexer {
    pub fn parse_iter<'a, 'b>(&'a self, input: &'b str, state: LexerState)
        -> LexerIter<'a, 'b>
    {
        LexerIter {
            lexer: self,
            consumed: 0,
            line_num: state.line_num,
            input: input,
            terminate: false,
            block_comment_depth: state.block_comment_depth,
        }
    }

    /// Lexes the given input into tokens and returns them and an optional
    /// state which will be `None` if no continuation is expected, or containing
    /// the state which is needed for the next line of input.
    pub fn parse(&self, input: &str, state: LexerState) ->
        Result<(Vec<(Loc, Token)>, Option<LexerState>), LexError>
    {
        let lexed: Result<Vec<_>, _> = self.parse_iter(input, state).collect();
        let lexed = lexed?;

        // check the last and the next after it token in case of continuation
        if let Some((&(_, Token::MetaContinueNextLine(state_opt)), rest)) =
               lexed.split_last()
        {
            // this token having None at this execution path is not allowed
            assert!(state_opt.is_some());
            return Ok((rest.to_vec(), state_opt));
        }

        Ok((lexed, None))
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

impl<'a, 'b: 'a> LexerIter<'a, 'b> {
    /// Consumes input the given amount of bytes.
    /// Panics if `amount` is larger than the input.
    #[inline]
    fn consume(&mut self, amount: usize) {
        // chop off used bytes
        self.input = &self.input[amount..];
        // record chopped off bytes
        self.consumed += amount;
    }

    fn get_state(&self) -> LexerState {
        LexerState {
            line_num: self.line_num,
            block_comment_depth: self.block_comment_depth,
        }
    }
}

impl<'a, 'b> Iterator for LexerIter<'a, 'b> {
    type Item = Result<(Loc, Token), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.terminate {
            // will never return Some(_) after returning None
            return None;
        }

        loop {
            if let Some(ma) = BLOCK_COMMENT_START_REGEX.find(self.input) {
                // started a block comment
                self.consume(ma.end());
                self.block_comment_depth += 1;
                if self.block_comment_depth >= u8::max_value() {
                    return Some(Err(LexError::MaximumCommentDepthReached));
                }
            } else if let Some(ma) = BLOCK_COMMENT_END_REGEX.find(self.input) {
                // check for block comment ends in this line
                // found a comment end
                self.consume(ma.end());
                self.block_comment_depth -= 1;
            } else if self.block_comment_depth <= 0 {
                break;
            } else {
                // another end or start not found on this line,
                // continue on the next line
                self.terminate = true;
                return Some(Ok((
                    Loc::new(self.line_num, self.consumed, self.consumed),
                    Token::MetaContinueNextLine(Some(self.get_state())),
                )));
            }
        }

        // strip left whitespace
        let ma = WS_STRIP_REGEX.find(self.input).unwrap();
        self.consume(ma.end());

        if self.input.is_empty() ||
           LINE_COMMENT_REGEX.find(self.input).is_some()
        {
            // when the input is empty or we started a line comment, terminate
            // on the next iteration
            self.terminate = true;
            return Some(Ok((
                Loc::new(self.line_num, self.consumed, self.consumed),
                Token::End,
            )));
        }

        for found_index in self.lexer.regex_set.matches(self.input) {
            // found a match, now extract it since regex set can't do that
            let &(ref regex, ref tok_fun) =
                &self.lexer.token_rules[found_index];

            let ma = regex.find(self.input).unwrap();

            // store previous used bytes
            let offset = self.consumed;
            self.consume(ma.end());

            let token = match tok_fun(ma.as_str()) {
                Token::MetaContinueNextLine(_) => {
                    // if we found the continuation we also found the eol
                    self.terminate = true;
                    // fix the state
                    Token::MetaContinueNextLine(Some(self.get_state()))
                },
                tok => tok,
            };

            // return the matched token
            return Some(Ok((
                Loc::new(self.line_num, offset + ma.start(), offset + ma.end()),
                token,
            )));
            // other matches are ignored, if they even exist
        }

        // no rule matched
        self.terminate = true; // stop on next iteration
        Some(Err(LexError::InvalidToken))
    }
}
