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
    Num(Ratio<i32>),
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
    /// Unescapes a double quoted string literal.
    /// 
    /// Panics if the argument string isn't surrounded by double quotes (`"`),
    /// or if the last character before double quotes is backslash (`\`).
    pub fn new_string(s: &str) -> Self {
        let mut out_str = String::new();

        // chop off double quotes
        let mut char_iter = s[1..s.len()-1].chars();

        // iterate over all characters
        while let Some(ch) = char_iter.next()  {
            match ch {
                '\\' => {
                    // get the escaped character
                    match char_iter.next() {
                        Some('\\') | None => out_str.push('\\'), // backspace
                        Some('"') => out_str.push('"'),
                        Some('n') => out_str.push('\n'), // line feed
                        Some('r') => out_str.push('\r'), // carriage return
                        Some('t') => out_str.push('\t'), // tab
                        // TODO unicode escaping
                        // if no escape worked, just push it like that
                        Some(c) => out_str.push(c),
                    }

                },
                // just print normal characters
                _ => out_str.push(ch),
            }
        }

        Value::Str(out_str)
    }

    /// Addition between types.
    /// 
    /// Only supported for numerals.
    impl_op!("addition", add, ops::Add::add);

    /// Substraction between types.
    /// 
    /// Only supported for numerals.
    impl_op!("substraction", sub, ops::Sub::sub);
    
    /// Multiplication between types.
    /// 
    /// Only supported for numerals.
    impl_op!("multiplication", mul, ops::Mul::mul);
    
    /// Division between types.
    /// 
    /// Only supported for numerals.
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
    
    /// Raising value to the power of another.
    /// 
    /// Only supported for numerals.
    pub fn pow(&self, rhs: &Value) -> Result<Value> {
        match (self, rhs) {
            (&Value::Num(a), &Value::Num(b)) =>
                if *b.denom() == 1
                {
                    Ok(Value::Num(a.pow(*b.numer())))
                } else {
                    // approximate
                    let a = *a.numer() as f32 / *a.denom() as f32;
                    let b = *b.numer() as f32 / *b.denom() as f32;
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

impl From<i32> for Value {
    fn from(i: i32) -> Value {
        use num::FromPrimitive;
        Value::Num(Ratio::from_i32(i).unwrap())
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Value {
        use num::FromPrimitive;
        // TODO ?: remove unwrap
        Value::Num(Ratio::from_f32(f).unwrap())
    }
}

impl From<Ratio<i32>> for Value {
    fn from(r: Ratio<i32>) -> Value {
        Value::Num(r)
    }
}
