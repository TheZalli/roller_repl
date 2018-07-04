#![allow(dead_code)] // TODO delete when things are used
///! Abstract syntax tree representation.

use std::collections::{BTreeMap, BTreeSet};

use value::{Value, IdType};
use op::OpCode;

/// A Roller expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    // A simple dummy expression that is illegal to be evaluated
    Empty,
    /// Value literal
    Val(Value),
    /// LValue reference
    LVal(LValue),
    /// Assignment
    Assign(LValue, Box<Expr>),
    /// Binary and unary operations
    BinOp(OpCode, Box<Expr>, Box<Expr>),
    /// Function call
    Call(CallExpr),
    /// List of expressions, will evaluate to `Value::List`.
    List(Vec<Expr>),
    /// Set of expressions, will evaluate to `Value::Set`.
    Set(BTreeSet<Expr>),
    /// Map of expressions, will evaluate to `Value::Map`.
    Map(BTreeMap<Expr, Expr>),
    /// Control structures.
    Ctrl(Control),
    /// Distribution expression, will evaluate to `Value::Distribution`.
    Distribution(Vec<(Expr, Expr)>),
}

/// An lvalue reference that can be used to mutate values
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LValue {
    /// Where is this variable visible in.
    pub visibility: Option<LValVis>,
    /// The first variable part.
    pub root: IdType,
    /// Rest of the dot-separated list parts.
    pub trail: Vec<Expr>,
}

impl LValue {
    /// Creates a new `LValue`.
    ///
    /// If visibility is `None` the default visibility, local, is used.
    pub fn new(visibility: Option<LValVis>, root: IdType, trail: Vec<Expr>) -> Self {
        LValue { visibility, root, trail }
    }

    /// Returns true if this `LValue` refers to a global value.
    pub fn is_global(&self) -> bool {
        self.visibility == Some(LValVis::Global)
    }

    /// Returns true if `LValue` refers to a local value.
    ///
    /// Default visibility is local.
    pub fn is_local(&self) -> bool {
        !self.is_global()
    }

    /// Returns true if this `LValue` has the default visibility (local).
    pub fn is_default_visibility(&self) -> bool {
        self.visibility.is_none()
    }
}

/// Visibility of a lvalue reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LValVis {
    /// Global namespace
    Global,
    /// Local namespace
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Control {
    Break,
    Continue,
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        //elif_exprs: Vec<Expr>,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallExpr {
    /// The callable function.
    pub func: Box<Expr>,
    /// The vector of the ordered arguments.
    pub args: Vec<Expr>,
    /// The vector of named arguments.
    pub kw_args: Vec<(IdType, Expr)>,
}

impl CallExpr {
    /// Create a new function call with the given ordered, and named, arguments.
    pub fn new(func: Expr,
               args: Vec<Expr>,
               kw_args: Vec<(IdType, Expr)>)
               -> Self
    {
        CallExpr { func: Box::new(func), args, kw_args }
    }
}

impl Control {
    pub fn new_if(cond: Expr, then_expr: Expr, else_expr: Expr) -> Self
    {
        Control::If {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }
    }
}
