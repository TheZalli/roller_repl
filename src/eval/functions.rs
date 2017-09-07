use std::fmt;

use ast::*;

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
