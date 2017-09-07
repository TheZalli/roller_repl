mod functions;
mod env;

use self::env::RollerNamespace;
use ast::{Expr, EvalError};

#[derive(Debug)]
pub struct EvalContext {
    global_ns: RollerNamespace,
}

impl EvalContext {
    /// Creates a new empty context.
    pub fn new() -> Self {
        EvalContext {
            global_ns: RollerNamespace::new(),
        }
    }

    /// Evaluates the expression AST and returns a printable message.
    pub fn eval(&mut self, ast: Expr) -> Result<String, EvalError> {
        match ast {
            Expr::Val(x) => return Ok(format!("{}", x)),
            _ => Err(EvalError::unimplemented("")),
        }
    }
}

