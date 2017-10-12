use std::collections::{BTreeSet, BTreeMap};

use error::{EvalError, Result};
use ast::{Expr, FunCall};
use value::{Value, IdType};
use op::{OpCode, CompOp};

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
    pub fn eval_print(&mut self, ast: &Expr) -> String {
        match self.eval(ast) {
            Ok(val) => {
                // string representation of the value
                let val_str = match val {
                    Value::Num(x) if !x.is_integer() =>
                        // non-integer numerals print differently
                        format!(
                            "{} (≈ {})", x,
                            *x.numer() as f64 / *x.denom() as f64
                        ),
                    Value::Str(s) =>
                        // unescape and print
                        format!("\"{}\"\nPrinted:\n{}",
                            s.chars().map::<String, _>(
                                |c| c.escape_debug().collect()
                            ).collect::<String>(),
                            s
                        ),
                    x => format!("{}", x),
                };

                // print differently according to what we did at AST root
                match ast {
                    &Expr::Assign(ref id, _) =>
                        format!("`{}` is now {}", id, val_str),
                    &Expr::Id(ref id) => format!("`{}` is {}", id, val_str),
                    _ => format!("{}", val_str),
                }
            },
            Err(e) => format!("Error: {}", e)
        }
        
    }

    /// Evaluates the expression and returns the resulting value.
    pub fn eval(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            &Expr::Val(ref x) => Ok(x.clone()),
            &Expr::Id(ref x) => self.global_ns.var(x).map(|x| x.clone()),
            &Expr::Op(ref fun_call) => self.eval_call(fun_call),
            &Expr::Assign(ref id, ref expr) => {
                // assign a value and return it
                let val = self.eval(&*expr)?;
                self.global_ns.insert(id.to_owned(), val.clone());
                return Ok(val);
            },
            &Expr::List(ref x) => Ok(Value::List(
                x.iter().map(|x| self.eval(x)).collect::<Result<Vec<_>>>()?
            )),
            &Expr::Set(ref x) => Ok(Value::Set(
                x.iter().map(|x| self.eval(x)).collect::<Result<BTreeSet<_>>>()?
            )),
            &Expr::Map(ref x) => Ok(Value::Map(
                x.iter().map(
                    |(k, v)| Ok((self.eval(k)?, self.eval(v)?))
                ).collect::<Result<BTreeMap<_, _>>>()?
            )),
            &Expr::Distribution(ref distr_map) => {
                // the output
                let mut out_map = BTreeMap::new();

                for (item, weight_expr) in distr_map.iter() {
                    // the evaluated weight value
                    let val = self.eval(weight_expr)?;

                    if let Value::Num(x) = val {
                        if x.is_integer() && *x.numer() > 0 {
                            // insert the weight value as u32 to output
                            out_map.insert(item.clone(), *x.numer() as u32);
                        } else {
                            return Err(EvalError::unexpected_type(&format!(
                                "Expected a positive (>0) 32-bit unsigned \
                                integer value as the weight, got value {}", val
                            )));
                        }
                    } else {
                        return Err(EvalError::unexpected_type(&format!(
                            "Expected numeral weight value, got value {}", val
                        )));
                    }
                }
                // return value
                Ok(Value::Distribution(out_map))
            },
            &Expr::Comp { op, ref lhs, ref rhs } => {
                let lhs = self.eval(lhs)?;
                let rhs = self.eval(rhs)?;
                Ok(Value::Bool(match op {
                    CompOp::Equals => lhs == rhs,
                    CompOp::Nequals => lhs != rhs,
                    CompOp::Lt => lhs < rhs,
                    CompOp::Lte => lhs <= rhs,
                    CompOp::Gt => lhs > rhs,
                    CompOp::Gte => lhs >= rhs,
                }))
            },
            _ => Err(EvalError::unimplemented("")),
        }
    }

    /// Evaluates a function call.
    fn eval_call(&mut self, call: &FunCall) -> Result<Value> {
        // evaluate arguments first
        let mut vals = Vec::with_capacity(call.ordered_args.len());
        
        for expr in call.ordered_args.iter() {
            vals.push(self.eval(expr)?);
        }

        // binary ops
        let acc_op = |code: &OpCode, operands: Vec<Value>,
                      func: fn(&Value, &Value) -> Result<Value>|
                      -> Result<Value>
            {
                if operands.len() < 2 {
                    Err(EvalError::invalid_arg(&format!(
                        "Operator `{:?}` requires at least 2 operands", code
                    )))
                } else {
                    use std::collections::VecDeque;
                    let mut arg_values: VecDeque<Value> = operands.into();

                    // calculate result
                    let mut acc = 
                        func(&arg_values.pop_front().unwrap(),
                            &arg_values.pop_front().unwrap())?;
                    
                    for val in arg_values {
                        acc = func(&acc, &val)?;
                    }

                    return Ok(acc);
                }
            };
        
        match &call.code {
            &OpCode::Not =>
                if vals.len() != 1 {
                    Err(EvalError::invalid_arg(&format!(
                        "Not-operation requires exactly 1 operand"
                    )))
                } else {
                    vals[0].not()
                },
            &OpCode::And => acc_op(&call.code, vals, Value::and),
            &OpCode::Or  => acc_op(&call.code, vals, Value::or),
            &OpCode::Xor => acc_op(&call.code, vals, Value::xor),
            &OpCode::Neg =>
                if vals.len() != 1 {
                    Err(EvalError::invalid_arg(&format!(
                        "Negation requires exactly 1 operand"
                    )))
                } else {
                    vals[0].neg()
                },
            &OpCode::Add => acc_op(&call.code, vals, Value::add),
            &OpCode::Sub => acc_op(&call.code, vals, Value::sub),
            &OpCode::Mul => acc_op(&call.code, vals, Value::mul),
            &OpCode::Div => acc_op(&call.code, vals, Value::div),
            &OpCode::Pow => acc_op(&call.code, vals, Value::pow),
            x => Err(EvalError::unimplemented(
                &format!("function `{:?}` is still unimplemented", x)
            )),
        }
    }

}
