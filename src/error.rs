use std::fmt;

pub type Result<T> = ::std::result::Result<T, EvalError>;

/// Type of the internal evaluation errors.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvalError {
    kind: EvalErrorKind,
    message: String,
}

#[allow(dead_code)] // TODO delete
impl EvalError {
    pub fn invalid_arg(msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::InvalidArgument,
            message: msg.to_owned(),
        }
    }

    pub fn unsupported_op(msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::UnsupportedOperation,
            message: msg.to_owned(),
        }
    }

    pub fn arithm_error(msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::ArithmeticError,
            message: msg.to_owned(),
        }
    }

    pub fn var_not_found(id: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::VariableNotFound,
            message: format!("variable `{}` not found", id),
        }
    }

    pub fn unimplemented(msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::Unimplemented,
            message: msg.to_owned(),
        }
    }

    pub fn custom(custom_kind: &str, msg: &str) -> Self {
        EvalError {
            kind: EvalErrorKind::Custom(custom_kind.to_owned()),
            message: msg.to_owned(),
        }
    }

}

// TODO: Implement display
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[allow(dead_code)] // TODO delete
pub enum EvalErrorKind {
    InvalidArgument,
    UnsupportedOperation,
    ArithmeticError,
    VariableNotFound,
    Unimplemented,
    Custom(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} error: {}", self.kind, self.message)
    }
}
