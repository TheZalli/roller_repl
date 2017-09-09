mod functions;
mod env;

use error::EvalError;
use self::env::RollerNamespace;
use ast::{Expr, Value};

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
    pub fn eval_fmt(&mut self, ast: Expr) -> Result<String, EvalError> {
        match self.global_ns.eval(ast)? {
            Value::Real(x) => return Ok(
                // ratios print differently
                format!("{} ≈ {}", x, *x.numer() as f64 / *x.denom() as f64)
            ),
            x => return Ok(format!("{}", x)),
        }
    }
}

