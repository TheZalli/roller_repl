use std::collections::{BTreeMap, BTreeSet};

use error::*;
use ast::{Expr, Value, IdType, FunCall, OpCode};

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

    pub fn eval(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Val(x) => Ok(x),
            Expr::Id(x) => {
                let val: Value = (*self.var(&x)?).clone();
                Ok(val)
            },
            Expr::Op(fun_call) => self.eval_call(fun_call),
            _ => Err(EvalError::unimplemented("")),
        }
    }

    /// Evaluates a function call
    fn eval_call(&mut self, call: FunCall) -> Result<Value> {
        match call.code {
            OpCode::Add => self.acc_unnamed_op(call, |x, y| x+y),
            x => Err(EvalError::unimplemented(
                &format!("function \"{:?}\" is still unimplemented", x)
            )),
        }
    }

    /// Function that evaluates binary+ operators with no named operands
    fn acc_unnamed_op(&mut self, c: FunCall,
                      func: fn(Value, Value) -> Value) -> Result<Value>
    {
        if !c.named_args.is_empty() {
            Err(EvalError::invalid_arg(
                &format!("No named arguments allowed for `{:?}`", c.code)
            ))
        } else if c.ordered_args.len() < 2 {
            Err(EvalError::invalid_arg(&format!(
                "Operator `{:?}` requires at least 2 operands", c.code
            )))
        } else {
            use std::collections::VecDeque;
            let mut arg_values = VecDeque::with_capacity(
                c.ordered_args.len()
            );

            // evaluate arguments
            for arg in c.ordered_args {
                arg_values.push_back(self.eval(arg)?);
            }
            // calculate result
            let mut acc = 
                func(arg_values.pop_front().unwrap(),
                        arg_values.pop_front().unwrap());
            
            for val in arg_values {
                acc = func(acc, val);
            }

            Ok(acc)
        }
    }
}
