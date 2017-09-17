use std::collections::BTreeMap;

use error::{EvalError, Result};
use ast::{Expr, FunCall};
use value::{Value, IdType};
use op::OpCode;

/// A namespace for variables.
#[derive(Debug)]
pub struct RollerNamespace {
    variables: BTreeMap<IdType, Value>,
}

impl RollerNamespace {
    /// Create a new empty namespace.
    pub fn new() -> Self {
        RollerNamespace {
            variables: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, id: IdType, value: Value) -> Option<Value> {
        self.variables.insert(id, value)
    }

    /// Return an immutable reference to a variable.
    pub fn var(&self, id: &str) -> Result<&Value> {
        self.variables.get(id).ok_or(
            EvalError::var_not_found(id)
        )
    }

    /// Return a mutable reference to a variable.
    pub fn var_mut(&mut self, id: &str) -> Result<&mut Value> {
        self.variables.get_mut(id).ok_or(
            EvalError::var_not_found(id)
        )
    }
}

#[derive(Debug)]
pub struct Env {
    global_ns: RollerNamespace,
}

impl Env {
    /// Creates a new empty context.
    pub fn new() -> Self {
        Env {
            global_ns: RollerNamespace::new(),
        }
    }

    /// Evaluates the expression and returns a printable message.
    pub fn eval_print(&mut self, ast: Expr) -> String {
        match self.eval(ast) {
            Value::Num(x) if !x.is_integer() =>
                // non-integer numerals print differently
                format!("{} ≈ {}", x, *x.numer() as f64 / *x.denom() as f64),
            x => format!("{}", x),
        }
    }

    /// Evaluates the expression and returns the resulting value.
    pub fn eval(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Val(x) => x,
            Expr::Id(x) => {
                match self.global_ns.var(&x) {
                    Ok(ref value) => (*value).clone(),
                    Err(e) => Value::Error(e),
                }
            },
            Expr::Op(fun_call) => self.eval_call(fun_call),
            _ => Value::Error(EvalError::unimplemented("")),
        }
    }

    /// Evaluates a function call.
    fn eval_call(&mut self, call: FunCall) -> Value {
        // evaluate arguments first
        let vals = call.ordered_args.into_iter()
            .map(|e| self.eval(e)).collect();

        // binary ops
        let acc_op = |code: OpCode, operands: Vec<Value>,
                      func: fn(&Value, &Value) -> Value| -> Value
            {
                if operands.len() < 2 {
                    Value::Error(EvalError::invalid_arg(&format!(
                        "Operator `{:?}` requires at least 2 operands", code
                    )))
                } else {
                    use std::collections::VecDeque;
                    let mut arg_values: VecDeque<Value> = operands.into();

                    // calculate result
                    let mut acc = 
                        func(&arg_values.pop_front().unwrap(),
                            &arg_values.pop_front().unwrap());
                    
                    for val in arg_values {
                        acc = func(&acc, &val);
                    }

                    acc
                }
            };
        
        match call.code {
            OpCode::Add => acc_op(call.code, vals, Value::add),
            OpCode::Sub => acc_op(call.code, vals, Value::sub),
            OpCode::Mul => acc_op(call.code, vals, Value::mul),
            OpCode::Div => acc_op(call.code, vals, Value::div),
            OpCode::Pow => acc_op(call.code, vals, Value::pow),
            x => Value::Error(EvalError::unimplemented(
                &format!("function `{:?}` is still unimplemented", x)
            )),
        }
    }

}
