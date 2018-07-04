#![allow(dead_code)] // TODO remove
use std::collections::BTreeMap;
use std::io::Write;

use error::{EvalError, Result};
use ast::{Expr, CallExpr, LValue, Control};
use value::{Value, IdType};
use op::OpCode;

/// A namespace for variables.
#[derive(Debug, Clone)]
struct RollerNamespace {
    variables: BTreeMap<IdType, Value>,
}

impl RollerNamespace {
    /// Create a new empty namespace.
    pub fn new() -> Self {
        RollerNamespace {
            variables: BTreeMap::new(),
        }
    }

    /// Inserts a new variable to the namespace.
    ///
    /// Returns old value if there were any.
    pub fn insert(&mut self, id: IdType, value: Value) -> Option<Value> {
        self.variables.insert(id, value)
    }

    /// Return an immutable reference to a variable.
    pub fn var(&self, id: &str) -> Result<&Value> {
        self.variables.get(id).ok_or(
            EvalError::var_not_found(id)
        )
    }

    /// Return an mutable reference to a variable.
    pub fn var_mut(&mut self, id: &str) -> Result<&mut Value> {
        self.variables.get_mut(id).ok_or(
            EvalError::var_not_found(id)
        )
    }

    /// Return an mutable reference to a variable or insert a default value.
    pub fn var_mut_or_insert(&mut self, id: &str, default: Value) -> &mut Value {
        self.variables.entry(id.to_string()).or_insert(default)
    }
}

type BuiltinFunc = fn(&mut Env, Vec<Value>, BTreeMap<&IdType, Value>) -> Result<Value>;

/// The script's running environment.
pub struct Env {
    /// The 'call stack', first element is the global namespace.
    ns_stack: Vec<RollerNamespace>,
    builtins: BTreeMap<String, BuiltinFunc>,
    out_stream: Box<Write>,
    err_stream: Box<Write>,
}

impl Env {
    /// Create a new empty context.
    pub fn new(out_stream: Box<Write>, err_stream: Box<Write>) -> Self {
        let mut builtins: BTreeMap<String, BuiltinFunc> = BTreeMap::new();
        builtins.insert("println".to_string(), Self::builtin_println_out);

        Env { ns_stack: vec![RollerNamespace::new()], builtins, out_stream, err_stream }
    }

    fn builtin_println_out(&mut self, vals: Vec<Value>, _kw_vals: BTreeMap<&IdType, Value>)
        -> Result<Value>
    {
        for val in vals {
            write!(self.out_stream, "{}", val)?;
        }
        writeln!(self.out_stream, "")?;
        Ok(Value::Void)
    }

    fn get_builtin(&self, name: &str) -> Option<BuiltinFunc> {
        self.builtins.get(name).map(|x| *x)
    }

    fn get_local_ns(&self) -> &RollerNamespace {
        self.ns_stack.last().unwrap()
    }

    fn get_mut_local_ns(&mut self) -> &mut RollerNamespace {
        self.ns_stack.last_mut().unwrap()
    }

    fn get_global_ns(&self) -> &RollerNamespace {
        &self.ns_stack[0]
    }

    fn get_mut_global_ns(&mut self) -> &mut RollerNamespace {
        &mut self.ns_stack[0]
    }

    /// Evaluate the expression and return a printable message.
    ///
    /// Intended to be used in command-line, not piped-in or file input.
    pub fn eval_print(&mut self, ast: &Expr) -> String {
        match self.eval(ast) {
            Ok(val) => {
                // string representation of the value
                let val_str = match val {
                    // non-integer numerals print differently
                    Value::Num(x) if !x.is_integer() => format!(
                        "{} (≈ {})", x, *x.numer() as f64 / *x.denom() as f64
                    ),
                    Value::Str(s) => format!("{:?}", s),
                    x => format!("{}", x),
                };

                // print differently according to what we did at AST root
                match ast {
                    //&Expr::Assign(_, _) => format!(""),
                    _ => format!("{}", val_str),
                }
            },
            Err(e) => format!("Error: {}", e)
        }
    }

    /// Evaluate the expression and return the resulting value.
    pub fn eval(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            &Expr::Empty => Err(EvalError::new_from_str_pair(
                "EmptyExprEvaluated", "empty expression evaluated"
            )),
            &Expr::Val(Value::Func(ref fun_def)) => {
                // check if the definition is valid
                fun_def.check_valid()?;
                Ok(Value::Func(fun_def.clone()))
            },
            &Expr::Val(ref val) => Ok(val.clone()),
            &Expr::LVal(ref lval) => Ok(self.eval_lvalue(lval, false)?.clone()),
            &Expr::Assign(ref lval, ref val) => {
                let val = self.eval(&*val.clone())?;
                *self.eval_lvalue(lval, true)? = val.clone();
                Ok(val)
            },
            &Expr::BinOp(op, ref lhs, ref rhs) => {
                let lhs = self.eval(lhs)?;
                // don't evaluate rhs if the op is unary
                match op {
                    OpCode::Neg => return lhs.neg(),
                    OpCode::Not => return lhs.not(),
                    _ => {}
                }

                let rhs = &self.eval(rhs)?;
                match op {
                    OpCode::Add => lhs.add(rhs),
                    OpCode::Sub => lhs.sub(rhs),
                    OpCode::Mul => lhs.mul(rhs),
                    OpCode::Div => lhs.div(rhs),
                    OpCode::Pow => lhs.pow(rhs),

                    OpCode::And => lhs.and(rhs),
                    OpCode::Or => lhs.or(rhs),
                    OpCode::Xor => lhs.xor(rhs),

                    // TODO: reimplement better ordering
                    comp_op => Ok(Value::Bool(match comp_op {
                        OpCode::Equals => lhs == *rhs,
                        OpCode::Nequals => lhs != *rhs,
                        OpCode::Lt => lhs < *rhs,
                        OpCode::Lte => lhs <= *rhs,
                        OpCode::Gt => lhs > *rhs,
                        OpCode::Gte => lhs >= *rhs,
                        _ => unreachable!()
                    }))
                }
            },
            &Expr::Call(ref call_expr) => self.eval_call(call_expr),
            &Expr::List(ref x) => Ok(Value::List(
                x.iter().map(|x| self.eval(x)).collect::<Result<Vec<_>>>()?
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
            &Expr::Ctrl(
                Control::If { ref cond, ref then_expr, ref else_expr }
            ) => {
                match self.eval(&cond)? {
                    Value::Bool(true) => self.eval(&then_expr),
                    Value::Bool(false) => self.eval(&else_expr),
                    x => Err(EvalError::unexpected_type(&format!(
                        "expected a boolean value, got {}", x
                    )))
                }
            },
            _ => Err(EvalError::unimplemented("")),
        }
    }

    /// Evaluate a function call.
    fn eval_call(&mut self, call: &CallExpr) -> Result<Value> {
        // evaluate arguments first

        // ordered arguments
        let mut vals = Vec::with_capacity(call.args.len());
        for expr in call.args.iter() {
            vals.push(self.eval(expr)?);
        }

        // keyword arguments
        let mut kw_vals = BTreeMap::new();
        for &(ref kw, ref expr) in call.kw_args.iter() {
            // later duplicate keyword arguments override previous ones
            kw_vals.insert(kw, self.eval(expr)?);
        }

        // check if the call is to a builtin function
        if let Expr::LVal(LValue { visibility, ref root, ref trail }) = *call.func {
            if let Some(builtin) = self.get_builtin(&root) {
                if !visibility.is_none() || !trail.is_empty() {
                    return Err(EvalError::new_from_str_pair(
                        "InvalidCall", &format!("invalid call to builtin {}", root)
                    ));
                }
                return builtin(self, vals, kw_vals);
            }
        }

        match self.eval(&*call.func)? {
            // evaluate a function call
            Value::Func(fun_def) => {
                // the function's local namespace
                let mut ns = RollerNamespace::new();

                // iterator for names
                let mut name_iter = fun_def.arg_names.iter();
                // iterator for ordered values
                let vals_len = vals.len(); // used in an error message
                let val_iter = vals.into_iter();

                // we don't use name_iter.zip(val_iter) since zip
                // consumes the iterators and we want to use name_iter
                // after this loop
                for val in val_iter {
                    let name = name_iter.next().ok_or(
                        // we ran out of parameters to fill!
                        EvalError::invalid_arg(&format!(
                            "expected {} arguments, got too many \
                            ordered arguments ({})",
                            fun_def.arg_names.len(), vals_len
                        ))
                    )?;

                    if ns.insert(name.clone(), val).is_some() {
                        // if we validate function definitions we
                        // should never get here
                        panic!("function has an argument named `{}` more than once!", name);
                    }
                }

                // read the rest of the defined arguments and pass
                // values from keyword arguments
                for name in name_iter {
                    // remove the values so in the end we can see if
                    // we had any extra arguments by checking length
                    let val = kw_vals.remove(name).ok_or(
                        EvalError::invalid_arg(&format!("argument `{}` not defined", name))
                    )?;

                    if let Some(_) = ns.insert(name.clone(), val) {
                        return Err(EvalError::invalid_arg(&format!(
                            "tried to pass argument `{}` twice", name
                        )))
                    };
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
            // evaluate a list indexing
            mut indexable =>
                if vals.len() != 1 {
                    Err(EvalError::invalid_arg(&format!(
                        "got {} ordered arguments for list indexing, \
                        expected one", vals.len()
                    )))
                } else if kw_vals.len() != 0 {
                    Err(EvalError::invalid_arg(&format!(
                        "got {} named arguments for list indexing, \
                        expected none", kw_vals.len()
                    )))
                } else {
                    indexable.index_mut(&vals[0], false).map(|x| x.clone())
                },
        }
    }

    fn eval_lvalue(&mut self, lval: &LValue, insert: bool)
        -> Result<&mut Value>
    {
        if self.get_builtin(&lval.root).is_some() {
            return Err(EvalError::invalid_arg(&format!(
                "identifier `{}` is a built-in function",
                lval.root
            )));
        }

        // get the root value
        let mut val: *mut Value = {
            // the namespace we want to change
            let val_ns =
                if lval.is_global() {
                    self.get_mut_global_ns()
                } else {
                    self.get_mut_local_ns()
                };

            if insert {
                val_ns.var_mut_or_insert(&lval.root, Value::None)
            } else {
                val_ns.var_mut(&lval.root)?
            }
        };

        if lval.trail.is_empty() {
            // no reason to check the trail anymore, our job here is done
            return Ok( unsafe { &mut *val } );
        }

        // check the trail of values
        let trail =
            if insert {
                // the last is where we will insert
                &lval.trail[0..lval.trail.len()-1]
            } else {
                &lval.trail
            };

        for expr in trail.iter() {
            let index_val = self.eval(expr)?;
            // advance to next indexable
            // TODO: check a way to remove this unsafe
            val = unsafe { (*val).index_mut(&index_val, false)? };
        }

        if insert {
            // insert the value to the last position
            let index_val = self.eval(lval.trail.last().unwrap())?;
            val = unsafe { (*val).index_mut(&index_val, true)? };
        }

        let val = unsafe { &mut *val };
        // the final value
        Ok(val)
    }
}
