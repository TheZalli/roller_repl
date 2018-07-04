use std::fmt;

use super::value::*;
use super::op::*;
use super::ast::*;

fn print_container<T>(f: &mut fmt::Formatter,
                   start: &'static str, it: T, sep: &'static str, end: &'static str) -> fmt::Result
    where T: IntoIterator,
          T::Item: fmt::Display,
{
    let mut it = it.into_iter();
    write!(f, "{}", start)?;
    if let Some(val) = it.next() {
        // print first
        write!(f, "{}", val)?;
    }
    for val in it {
        // print rest
        write!(f, "{}{}", sep, val)?;
    }
    write!(f, "{}", end)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Void => write!(f, ""),
            &Value::None => write!(f, "none"),
            &Value::Num(x) => write!(f, "{}", x),
            &Value::Bool(x) => write!(f, "{}", x),
            &Value::Str(ref x) => write!(f, "{}", x),
            &Value::Func(ref x) => write!(f, "{}", x),
            &Value::List(ref x) => print_container(f, "[", x, ", ", "]"),
            &Value::Map(ref x) =>
                if x.is_empty() {
                    // otherwise empty set is the same as empty map
                    write!(f, "[:]")
                } else {
                    print_container(f, "[",
                        x.iter().map(|(k, v)| format!("{}:{}", k, v)),
                    ", ", "]")
                },
            &Value::Distribution(ref x) =>
                if x.is_empty() {
                    write!(f, "[|]")
                } else {
                    print_container(f, "[",
                        x.iter().map(|(k, v)| format!("{}:{}", k, v)),
                    " | ", "]")
                },
        }
    }
}

impl fmt::Display for FunDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ ")?;
        for arg in &self.arg_names {
            write!(f, "{} ", arg)?;
        }
        write!(f, "; {} }}", self.body)
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OpCode::*;
        write!(f, "{}", match self {
            Neg => "-",
            Not => "not",
            And => "and",
            Or => "or",
            Xor => "xor",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Pow => "^",
            Equals => "is",
            Nequals => "isnt",
            Lt => "<",
            Lte => "<=",
            Gt => ">",
            Gte => ">=",
        })
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match self {
            &Val(ref x) => write!(f, "{}", x),
            &LVal(ref lval) => write!(f, "{}", lval),
            &Assign(ref lval, ref exp) => write!(f, "{} = {}", lval, exp),
            &BinOp(op @ OpCode::Neg, ref lhs, _ ) =>
                write!(f, "({}{})", op, lhs),
            &BinOp(op @ OpCode::Not, ref lhs, _ ) =>
                write!(f, "({} {})", op, lhs),
            &BinOp(ref op, ref lhs, ref rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            &Call(ref callexp) => write!(f, "{}", callexp),
            _ => write!(f, "{:#?}", self) // TODO
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.visibility {
            Some(LValVis::Global) => write!(f, "global.")?,
            Some(LValVis::Local) => write!(f, "local.")?,
            None => {},
        }
        write!(f, "{}", self.root)?;
        for part in self.trail.iter() {
            write!(f, ".{}", part)?;
        }
        Ok(())
    }
}

impl fmt::Display for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.func)?;
        print_container(f, "(", &self.args, ",", ")")
        // TODO kw args
    }
}
