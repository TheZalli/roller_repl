use ast::Expr;

/// Operators
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    Expr(Box<Expr>),

    Neg,
    Not,

    And,
    Or,
    Xor,

    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

/// Comparison operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompOp {
    /// `=`
    Equals,
    /// `!=`
    Nequals,
    /// `<`
    Lt,
    /// `<=`
    Lte,
    /// `>`
    Gt,
    /// `>=`
    Gte,
}
