use value::IdType;

#[allow(dead_code)] // TODO delete
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
    Pow,

    Equals,

    Dice, // d

    Error, // error
}
