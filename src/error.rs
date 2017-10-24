use std::fmt;
use std::str::FromStr;

pub type Result<T> = ::std::result::Result<T, EvalError>;

/// Type of the internal evaluation errors.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvalError {
    kind: EvalErrorKind,
    message: String,
}

macro_rules! impl_err_kind_builder {
    ($fun_name:ident, $kind:ident) => {
        pub fn $fun_name(msg: &str) -> Self {
            EvalError {
                kind: EvalErrorKind::$kind,
                message: msg.to_owned(),
            }
        }
    };
    ($fun_name:ident, $kind:ident, $msg_func:expr) => {
        pub fn $fun_name(input: &str) -> Self {
            EvalError {
                kind: EvalErrorKind::$kind,
                message: $msg_func(input),
            }
        }
    }
}

impl EvalError {
    pub fn new_from_str_pair(kind: &str, msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::from_str(kind).unwrap(),
            message: msg.to_owned(),
        }
    }

    impl_err_kind_builder!(invalid_arg, InvalidArgument);
    impl_err_kind_builder!(unsupported_op, UnsupportedOperation);
    impl_err_kind_builder!(arithm_error, ArithmeticError);
    impl_err_kind_builder!(var_not_found, ValueNotFound,
        |id| format!("variable `{}` not found in this context", id)
    );
    impl_err_kind_builder!(val_not_found, ValueNotFound,
        |id| format!("value `{}` not found in this context", id)
    );
    impl_err_kind_builder!(unexpected_type, UnexpectedType);
    impl_err_kind_builder!(unimplemented, Unimplemented);
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum EvalErrorKind {
    InvalidArgument,
    UnsupportedOperation,
    ArithmeticError,
    ValueNotFound,
    UnexpectedType,
    Unimplemented,
    /// Remember to check that the name is not the same as one of above
    Custom(String),
}

impl FromStr for EvalErrorKind {
    type Err = (); // we can't fail
    fn from_str(s: &str) -> ::std::result::Result<Self, ()> {
        use self::EvalErrorKind::*;
        Ok(match s {
            "InvalidArgument" => InvalidArgument,
            "UnsupportedOperation" => UnsupportedOperation,
            "ArithmeticError" => ArithmeticError,
            "ValueNotFound" => ValueNotFound,
            "UnexpectedType" => UnexpectedType,
            "Unimplemented" => Unimplemented,
            _ => Custom(s.to_string())
        })
    }
}

impl fmt::Display for EvalErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let &EvalErrorKind::Custom(ref s) = self {
            write!(f, "{}", s)
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {}", self.kind, self.message)
    }
}
