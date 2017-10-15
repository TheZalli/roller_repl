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
}

#[derive(Debug)]
pub struct Env {
    global_ns: RollerNamespace,
    ns_stack: Vec<RollerNamespace>,
}

impl Env {
    /// Creates a new empty context.
    pub fn new() -> Self {
        Env {
            global_ns: RollerNamespace::new(),
            ns_stack: Vec::new(),
        }
    }

    /// Inserts the variable `id` with `value`.
    /// 
    /// Checks if a local namespace exists and inserts there if it does.
    /// Otherwise inserts the variable to the global namespace.
    pub fn insert(&mut self, id: IdType, value: Value) -> Option<Value> {
        self.ns_stack.last_mut().unwrap_or(&mut self.global_ns)
            .insert(id, value)
    }

    /// Get a refence to a variable with name id.
    /// 
    /// Checks the local namespace first, then if it doesn't exist, the global.
    fn var(&self, id: &str) -> Result<&Value> {
        self.ns_stack.last().unwrap_or(&self.global_ns).var(id)
    }

    /// Evaluates the expression and returns a printable message.
    pub fn eval_print(&mut self, ast: &Expr) -> String {
        match self.eval(ast) {
            Ok(val) => {
                // string representation of the value
                let val_str = match val {
                    // non-integer numerals print differently
                    Value::Num(x) if !x.is_integer() => format!(
                        "{} (≈ {})", x, *x.numer() as f64 / *x.denom() as f64
                    ),
                    Value::Str(s) => format!("\"{:?}\"\nPrinted:\n{}", s, s),
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
            &Expr::Val(Value::Func(ref fun_def)) => {
                // check if the definition is valid
                fun_def.check_valid()?;
                Ok(Value::Func(fun_def.clone()))
            },
            &Expr::Val(ref x) => Ok(x.clone()),
            &Expr::Id(ref x) => self.var(x).map(|x| x.clone()),
            &Expr::Op(ref fun_call) => self.eval_call(fun_call),
            &Expr::Assign(ref id, ref expr) => {
                // assign a value and return it
                let val = self.eval(&*expr)?;
                self.insert(id.to_owned(), val.clone());
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
                let mut out_map = BTreeMap::<Expr, u32>::new();

                for &(ref item, ref weight_expr) in distr_map.iter() {
                    // the evaluated weight value
                    let val = self.eval(weight_expr)?;

                    if let Value::Num(x) = val {
                        let weight = *x.numer();
                        if x.is_integer() && weight > 0 {
                            use std::collections::btree_map::Entry::*;

                            let weight = weight as u32;
                            // insert or update the weight value
                            match out_map.entry(item.clone()) {
                                Vacant(x) => { x.insert(weight); },
                                Occupied(ref mut x) =>
                                    *(x.get_mut()) += weight,
                            }
                        } else {
                            return Err(EvalError::unexpected_type(&format!(
                                "expected a positive (>0) 32-bit unsigned \
                                integer value as the weight, got value {}", val
                            )));
                        }
                    } else {
                        return Err(EvalError::unexpected_type(&format!(
                            "expected numeral weight value, got value {}", val
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
        let mut vals = Vec::with_capacity(call.args.len());
        for expr in call.args.iter() {
            vals.push(self.eval(expr)?);
        }

        let mut kw_vals = BTreeMap::new();
        for &(ref kw, ref expr) in call.kw_args.iter() {
            // later duplicate keyword arguments override previous ones
            kw_vals.insert(kw, self.eval(expr)?);
        }

        // binary ops
        fn acc_op(code: &OpCode,
                  operands: Vec<Value>,
                  func: fn(&Value, &Value) -> Result<Value>)
            -> Result<Value>
        {
            if operands.len() < 2 {
                Err(EvalError::invalid_arg(&format!(
                    "operator `{:?}` requires at least 2 operands", code
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
        }
        
        match &call.code {
            &OpCode::Expr(ref e) => {
                // is it possible to avoid the clone?
                match self.eval(e)? {
                    Value::Func(fun_def) => {
                        // the function's local namespace
                        let mut ns = RollerNamespace::new();

                        // iterator for names
                        let mut name_iter = fun_def.arg_names.iter();
                        // iterator for unordered values
                        let vals_len = vals.len(); // used in an error msg
                        let val_iter = vals.into_iter();

                        // we don't use name_iter.zip(val_iter) since zip
                        // consumes the iterators and we want to use name_iter
                        // after this loop
                        for val in val_iter {
                            let name = name_iter.next().ok_or(
                                // we ran out of parameters to fill!
                                EvalError::invalid_arg(&format!(
                                    "expected {} arguments, got too many \
                                    unordered arguments ({})",
                                    fun_def.arg_names.len(), vals_len
                                ))
                            )?;

                            if ns.insert(name.clone(), val).is_some() {
                                // if we validate function definitions we
                                // should never get here
                                panic!(
                                    "function has argument named `{}` more \
                                    than once!", name
                                );
                            }
                        }
                        
                        // read the rest of the defined arguments and pass 
                        // values from keyword arguments
                        for name in name_iter {
                            // remove the values so in the end we can see if
                            // we had any extra arguments by checking length
                            let val = kw_vals.remove(name).ok_or(
                                EvalError::invalid_arg(&format!(
                                    "argument `{}` not defined", name
                                ))
                            )?;

                            ns.insert(name.clone(), val).ok_or(
                                EvalError::invalid_arg(&format!(
                                    "tried to pass argument {} twice", name
                                ))
                            )?;
                        }

                        if !kw_vals.is_empty() {
                            return Err(EvalError::invalid_arg(&format!(
                                "got {} extra keyword arguments", kw_vals.len()
                            )));
                        }

                        // push a new 'stack frame'
                        // TODO: implement max call stack size and check it here
                        self.ns_stack.push(ns);
                        let return_value = self.eval(&fun_def.body);
                        self.ns_stack.pop();
                        return_value  // pun not intended
                    },
                    // we can't call this if it's not a function
                    val => Err(EvalError::unexpected_type(&format!(
                        "expected a function value, got value {}", val 
                    )))
                }
            },
            &OpCode::Not =>
                if vals.len() != 1 {
                    Err(EvalError::invalid_arg(&format!(
                        "not-operation requires exactly 1 operand"
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
                        "negation requires exactly 1 operand"
                    )))
                } else {
                    vals[0].neg()
                },
            &OpCode::Add => acc_op(&call.code, vals, Value::add),
            &OpCode::Sub => acc_op(&call.code, vals, Value::sub),
            &OpCode::Mul => acc_op(&call.code, vals, Value::mul),
            &OpCode::Div => acc_op(&call.code, vals, Value::div),
            &OpCode::Pow => acc_op(&call.code, vals, Value::pow),
        }
    }

}
