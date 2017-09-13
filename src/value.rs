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
    Num(Ratio<i64>),
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
            &Value::Num(x) => write!(f, "{}", x),
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
            (Value::Num(x), Value::Num(y)) => (x + y).into(),
            _ => Value::Error(EvalError::unsupported_op(
                "addition not supported between these types"
            )) // TODO
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        use num::FromPrimitive;
        Value::Num(Ratio::from_i64(i).unwrap())
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Value {
        use num::FromPrimitive;
        // TODO ?: remove unwrap
        Value::Num(Ratio::from_f64(f).unwrap())
    }
}

impl From<Ratio<i64>> for Value {
    fn from(r: Ratio<i64>) -> Value {
        Value::Num(r)
    }
}
