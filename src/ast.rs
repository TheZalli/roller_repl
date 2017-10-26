#![allow(dead_code)] // TODO delete when things are used
///! Abstract syntax tree representation.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use value::{Value, IdType};
use op::{OpCode, CompOp};

/// A Roller expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Value literal
    Val(Value),
    /// Identifier reference
    Id(IdType),
    /// Value assignment
    Assign(LValue, Box<Expr>),
    /// Comparison
    Comp {
        op: CompOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    /// Function call.
    /// Also applies operators since they are built-in functions.
    Op(FunCall),
    /// List of expressions, will evaluate to `Value::List`.
    List(Vec<Expr>),
    Set(BTreeSet<Expr>),
    Map(BTreeMap<Expr, Expr>),
    /// Control structures.
    Ctrl(Control),
    Distribution(Vec<(Expr, Expr)>),
}

/// An lvalue reference that can be used to mutate values
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LValue {
    /// Where is this variable visible in.
    pub visibility: LValVis,
    /// The first variable part.
    pub root: IdType,
    /// Rest of the dot-separated list parts.
    pub trail: Vec<Expr>,
    /// Whether we should declare the variable?
    pub insert: bool,
}

impl LValue {
    /// Creates a new `LValue`.
    ///
    /// If visibility is `None` the default of `LValVis::Local` is used.
    /// If visibility is not `None` however, the value will be inserted into
    /// the namespace as new (declared).
    pub fn new(visibility: Option<LValVis>, root: IdType, trail: Vec<Expr>) -> Self {
        LValue {
            visibility: visibility.unwrap_or(LValVis::Local),
            root: root,
            trail: trail,
            insert: visibility.is_some(),
        }
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
    /// The vector of the ordered arguments.
    pub args: Vec<Expr>,
    /// The vector of named arguments.
    pub kw_args: Vec<(IdType, Expr)>,
}

impl FunCall {
    /// Create a new function call with the given ordered, and named, arguments.
    pub fn new(code: OpCode,
               args: Vec<Expr>,
               kw_args: Vec<(IdType, Expr)>)
               -> Self
    {
        FunCall {
            code: code,
            args: args,
            kw_args: kw_args,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expr::Val(ref x) => write!(f, "{}", x),
            &Expr::Id(ref x) => write!(f, "{}", x),
            _ => write!(f, "{:?}", self) // TODO
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let LValVis::Global = self.visibility {
            write!(f, "global.")?;
        }
        write!(f, "{}", self.root)?;
        for part in self.trail.iter() {
            write!(f, ".{}", part)?;
        }
        Ok(())
    }
}
