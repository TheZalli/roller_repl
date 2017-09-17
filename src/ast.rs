#![allow(dead_code)] // TODO delete when things are used
///! Abstract syntax tree representation.

use std::collections::{BTreeMap, BTreeSet};

use value::{Value, IdType};
use op::OpCode;

/// A Roller expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Value literal
    Val(Value),
    /// Identifier reference
    Id(IdType),
    /// Variable assignment
    Assign(IdType, Box<Expr>),
    /// Function call.
    /// Also applies operators since they are built-in functions.
    Op(FunCall),
    /// List of expressions, will evaluate to `Value::List`.
    List(Vec<Expr>),
    Set(BTreeSet<Expr>),
    Map(BTreeMap<Expr, Expr>),
    /// Control structures.
    Ctrl(Control),
    Distribution(BTreeMap<Expr, Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
        iterator: IdType,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
    Try {
        expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
}

/// A function application with ordered and/or named arguments.
/// 
/// Not to be confused with `PrankCall`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunCall {
    /// Name of the function or the operator.
    pub code: OpCode,
    /// List of argument names.
    pub names: Vec<IdType>,
    /// The vector of the ordered arguments.
    pub ordered_args: Vec<Expr>,
    /// The vector of named arguments.
    pub named_args: Vec<(IdType, Expr)>,
}

impl FunCall {
    pub fn new(code: OpCode, argument_names: Vec<IdType>) -> Self {
        FunCall {
            code: code,
            names: argument_names,
            ordered_args: Vec::new(),
            named_args: Vec::new(),
        }
    }

    pub fn new_with_args(code: OpCode,
                         argument_names: Vec<IdType>,
                         exprs: Vec<Expr>)
                         -> Self
    {
        FunCall {
            code: code,
            names: argument_names,
            ordered_args: exprs,
            named_args: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunDef {
    code: OpCode,
    // TODO
}
