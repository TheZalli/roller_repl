use std::fmt;
use std::ops;
use std::collections::{BTreeSet, BTreeMap};

use num::rational::Ratio;

use ast::*;
use error::EvalError;

// Type of the identifier strings.
pub type IdType = String;

/// A Roller value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::None => write!(f, "none"),
            &Value::Int(x) => write!(f, "{}", x),
            &Value::Real(x) => write!(f, "{}", x),
            &Value::Bool(x) => write!(f, "{}", x),
            &Value::Str(ref x) => write!(f, "{}", x),
            &Value::Func(ref x) => write!(f, "{:?}", x), // TODO impl Display
            &Value::List(ref x) => write!(f, "{:?}", x), // ditto.
            &Value::Set(ref x) => write!(f, "{:?}", x), // ditto.
            &Value::Map(ref x) => write!(f, "{:?}", x), // ditto.
            &Value::Error(ref x) => write!(f, "{}", x),
        }
    }
}

impl ops::Add for Value {
    type Output = Value;
    fn add(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Int(x), Value::Int(y)) => (x + y).into(),
            (Value::Int(x), Value::Real(y)) =>
                Value::Real(Ratio::from_integer(x) + y),
            (Value::Real(x), Value::Int(y)) =>
                Value::Real(x + Ratio::from_integer(y)),
            (Value::Real(x), Value::Real(y)) => (x + y).into(),
            _ => Value::Error(EvalError::unsupported_op(
                "addition not supported between these types"
            )) // TODO
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Int(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Value {
        use num::FromPrimitive;
        // TODO ?: remove unwrap
        Value::Real(Ratio::from_f64(f).unwrap())
    }
}

impl From<Ratio<i64>> for Value {
    fn from(r: Ratio<i64>) -> Value {
        Value::Real(r)
    }
}
