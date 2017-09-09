
use value::{Value, IdType};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    Id(IdType),

    Identity,
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

    Is, // is
    Set, // =
    Assign, // :=

    AssignAdd, // +=, etc
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignExp,

    Dot, // .
    Colon, // :
    Alternate, // |
    Dice, // d

    Error, // error
}
