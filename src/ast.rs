#![allow(dead_code)] // TODO delete when things are used
///! Abstract syntax tree representation.

use std::collections::{BTreeMap, BTreeSet};

use num::rational::Ratio;

/// Type of the internal evaluation errors.
pub type EvalError = String;

/// A Roller expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Value literal
    Val(Value),
    /// Identifier reference
    Id(String),
    /// Function call.
    /// Also applies operators since they are built-in functions.
    Op(FunCall),
    /// List of expressions, will evaluate to `Value::List`.
    List(Vec<Expr>),
    Set(BTreeSet<Expr>),
    Map(BTreeMap<Expr, Expr>),
    /// Control structures.
    Ctrl(Control),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Control {
    Break,
    Continue,
    If {
        cond_expr: Box<Expr>,
        then_expr: Box<Expr>,
        elif_exprs: Vec<Expr>,
        else_expr: Box<Expr>,
    },
    Loop {
        body: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        iterator: String,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
    Try {
        expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
}

/// A Roller value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    None,
    Int(i64),
    /// We use ratios because _fuck_ NaNs.
    Real(Ratio<i64>),
    Bool(bool),
    Str(String),
    Func(FunDef),
    List(Vec<Value>),
    Set(BTreeSet<Value>),
    Map(BTreeMap<Value, Value>),
    /// Evaluation-time error
    Error(EvalError),
}

/// A function application with ordered and/or named arguments.
/// 
/// Not to be confused with `PrankCall`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunCall {
    /// Name of the function or the operator.
    code: OpCode,
    /// The vector of the ordered arguments.
    ordered_args: Vec<Expr>,
    /// The vector of named arguments.
    named_args: Vec<(String, Expr)>,
}

impl FunCall {
    pub fn new(code: OpCode) -> Self {
        FunCall {
            code: code,
            ordered_args: Vec::new(),
            named_args: Vec::new(),
        }
    }

    pub fn new_with_args(code: OpCode, exprs: Vec<Expr>) -> Self {
        FunCall {
            code: code,
            ordered_args: exprs,
            named_args: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunDef {
    code: OpCode,
    // TODO
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    Id(String),

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
    Alternate, // |
    Dice, // d

    Error, // error
}
