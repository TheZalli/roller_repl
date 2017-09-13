
use value::{Value, IdType};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    Id(IdType),

    Neg,
    Not,

    And,
    Or,
    Xor,

    Add,
    Sub,
    Mul,
    Div,
    Exp,

    Assign, // =

    Dot, // .
    Dice, // d

    Error, // error
}
