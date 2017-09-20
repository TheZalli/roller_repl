use std::fmt;
use std::ops;
use std::collections::{BTreeSet, BTreeMap};

use num::rational::Ratio;

use ast::*;
use error::{EvalError, Result};

// Type of the identifier strings.
pub type IdType = String;

/// A Roller value.
#[allow(dead_code)] // TODO delete
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
}

macro_rules! impl_op {
    ($name:expr, $fun_name:ident, $called_fun:path) => (
        pub fn $fun_name(&self, rhs: &Value) -> Result<Value> {
            match (self, rhs) {
                (&Value::Num(x), &Value::Num(y)) =>
                    Ok($called_fun(x, y).into()),
                _ => Err(EvalError::unsupported_op(&format!(
                    "{} is not supported between these types", $name
                )))
            }
        }
    )
}

impl Value {
    impl_op!("addition", add, ops::Add::add);
    impl_op!("substraction", sub, ops::Sub::sub);
    impl_op!("multiplication", mul, ops::Mul::mul);
    
    pub fn div(&self, rhs: &Value) -> Result<Value> {
        match (self, rhs) {
            (&Value::Num(x), &Value::Num(y)) => {
                if y.to_integer() == 0 {
                    return Err(EvalError::arithm_error("division by zero"))
                }
                Ok((x / y).into())
            },
            _ => Err(EvalError::unsupported_op(
                "division is not supported between these types"
            ))
        }
    }

    pub fn pow(&self, rhs: &Value) -> Result<Value> {
        match (self, rhs) {
            (&Value::Num(a), &Value::Num(b)) =>
                if *b.denom() == 1 &&
                    *b.numer() <= i32::max_value() as i64 &&
                    *b.numer() >= i32::min_value() as i64
                {
                    Ok(Value::Num(a.pow(*b.numer() as i32)))
                } else {
                    // approximate
                    let a = *a.numer() as f64 / *a.denom() as f64;
                    let b = *b.numer() as f64 / *b.denom() as f64;
                    Ok(a.powf(b).into())
                },
            _ => Err(EvalError::unsupported_op(&format!(
                "raising to power is not supported between these types"
            ))),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! print_container {
            ($start:expr, $iterator:expr, $end:expr) => ({
                let mut it = $iterator;
                write!(f, "{}", $start)?;
                if let Some(val) = it.next() {
                    // print first
                    write!(f, "{}", val)?;
                }
                for val in it {
                    // print rest
                    write!(f, ", {}", val)?;
                }
                write!(f, "{}", $end)
            })
        }

        match self {
            &Value::None => write!(f, "none"),
            &Value::Num(x) => write!(f, "{}", x),
            &Value::Bool(x) => write!(f, "{}", x),
            &Value::Str(ref x) => write!(f, "{}", x),
            &Value::Func(ref x) => write!(f, "{:?}", x), // TODO impl Display
            &Value::List(ref x) => print_container!("[", x.iter(), "]"),
            &Value::Set(ref x) => print_container!("{", x.iter(), "}"),
            &Value::Map(ref x) => write!(f, "{:?}", x), // TODO
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
