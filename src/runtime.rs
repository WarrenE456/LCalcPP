use crate::expr::*;
use crate::scanner::{Token, TokenType};

use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Abstraction {
    pub param: Token,
    pub paramtype: Option<Type>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub enum Val {
    Number(f64),
    String(String),
    Abstraction(Abstraction, Option<Box<Type>>), 
    Unit,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Abstraction(Option<Box<Type>>, Option<Box<Type>>),
    Unit,
}

impl Type {
    pub fn from_typename(typename: &str) -> Option<Option<Self>> {
        match typename {
            "Number" => Some(Some(Type::Number)),
            "String" => Some(Some(Type::String)),
            "Unit"   => Some(Some(Type::Unit)),
            "Any"    => Some(None),
            _ => None,
        }
    }
    pub fn from_slice(types: &[Option<Type>]) -> Option<Type> {
        if types.len() == 0 {
            Some(Type::Unit)
        }
        else if types.len() == 1 {
            types[0].clone()
        }
        else {
            Some(
                Type::Abstraction(
                    types.first().unwrap().clone().map(|v| Box::new(v)),
                    Box::new(Self::from_slice(&types[1..])).map(|v| Box::new(v))
                )
            )
        }

    }
    pub fn from_tokens(tokens: &Vec<Token>) -> Result<Option<Type>, RuntimeError> {
        let mut types = Vec::new();
        for tok in tokens.iter() {
            let new_argt = Type::from_typename(&tok.lexeme).ok_or_else(|| {
                let msg = format!("Reference to unbound type '{}'.", tok.lexeme);
                RuntimeError { token: tok.clone(), msg }
            })?;
            types.push(new_argt);
        }
        Ok(Type::from_slice(&types.as_slice()))
    }
    pub fn to_string(&self) -> String {
        match self {
            Type::Number =>   "Number".to_string(),
            Type::String =>   "String".to_string(),
            Type::Unit   =>   "Unit".to_string(),
            Type::Abstraction(a, b) =>
                format!("({} -> {})",
                    if let Some(a) = a { (*a).to_string() } else { "Any".to_string() },
                    if let Some(b) = b { (*b).to_string() } else { "Any".to_string() }
                ),
        }
    }
    pub fn deep_equality(target: &Self, checked: &Self) -> bool {
        match (target, checked) {
            (Type::Abstraction(param1, return1),
             Type::Abstraction(param2, return2)) => {
                let params_type_check = if let (Some(p1), Some(p2)) = (param1, param2) {
                    Type::deep_equality(p1, p2)
                } else {
                    true
                };
                if !params_type_check { return false; }
                let returns_type_check = if let (Some(r1), Some(r2)) = (return1, return2) {
                    Type::deep_equality(r1, r2)
                } else {
                    true
                };
                returns_type_check
            }
            (a, b) => a == b,
        }
    }
}

impl Val {
    pub fn to_string(&self) -> String {
        match self {
            Val::Number(n) => n.to_string(),
            Val::String(s) => s.clone(),
            Val::Unit => "()".to_string(),
            Val::Abstraction(_, _) => "<abstraction>".to_string(),
        }
    }
    pub fn to_type(&self) -> Type {
        match self {
            Val::Number(_) => Type::Number,
            Val::String(_) => Type::String,
            Val::Unit => Type::Unit,
            Val::Abstraction(abs, val) => {
                Type::Abstraction(abs.paramtype.clone().map(|v| Box::new(v)), val.clone())
            }
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub msg: String,
}

fn str_mul(s: &mut String, n: f64) -> Result<(), String> {
    if n.fract() != 0.0 || n < 0.0  {
        Err("Attempt to multiply a string by anything but a positive integer.".to_string())
    } else {
        let n: usize = n as usize;
        let sub = s.clone();
        for _ in 1..n {
            s.push_str(&sub);
        }
        Ok(())
    }
}

// TODO: Get rid of env and replace with beta reduction
pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }
    pub fn visit_binary(& self, binary: & Binary) -> Result<Val, RuntimeError> {
        let left = self.interpret(&binary.left)?;
        let right = self.interpret(&binary.right)?;

        use crate::scanner::TokenType::*;

       match binary.op.t.clone() {
            Plus => {
                match (left, right) {
                    (Val::Number(a), Val::Number(b)) => {
                        Ok(Val::Number(a + b))
                    }
                    (Val::String(mut a), Val::Number(b)) => {
                        a.push_str(&b.to_string());
                        Ok(Val::String(a))
                    }
                    (Val::Number(a), Val::String(mut b)) => {
                        b.push_str(&a.to_string());
                        Ok(Val::String(b))
                    }
                    (Val::String(mut b), Val::String(a)) => {
                        b.push_str(&a);
                        Ok(Val::String(b))
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to use plus operator on {} and {}.",
                            a.to_type().to_string(), b.to_type().to_string());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            Minus => {
                match (left, right) {
                    (Val::Number(a), Val::Number(b)) => {
                        Ok(Val::Number(a - b))
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to use minus operator on {} and {}.",
                            a.to_type().to_string(), b.to_type().to_string());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            Star => {
                match (left, right) {
                    (Val::Number(a), Val::Number(b)) => {
                        Ok(Val::Number(a * b))
                    }
                    (Val::Number(a), Val::String(mut b)) => {
                        match str_mul(&mut b, a) {
                            Ok(()) => Ok(Val::String(b)),
                            Err(msg) => {
                               let token = binary.op.clone(); 
                                Err(RuntimeError { token, msg})
                            }
                        }
                    }
                    (Val::String(mut a), Val::Number(b)) => {
                        match str_mul(&mut a, b) {
                            Ok(()) => Ok(Val::String(a)),
                            Err(msg) => {
                               let token = binary.op.clone(); 
                                Err(RuntimeError { token, msg})
                            }
                        }
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to multiplication operator on {} and {}.",
                            a.to_type().to_string(), b.to_type().to_string());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            Slash => {
                match (left, right) {
                    (Val::Number(a), Val::Number(b)) => {
                        Ok(Val::Number(a / b))
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to division operator on {} and {}.",
                            a.to_type().to_string(), b.to_type().to_string());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            _ => panic!("INTERPRETER FAILED in visist_binary: Operator '{}' type did not match any.", binary.op.lexeme),
        }
    }
    pub fn visit_binding(&self, binding: &Binding) -> Result<Val, RuntimeError> {
        let mut val = self.interpret(&binding.val)?;

        let val_type = val.to_type();
        let target_type =  if let Some(typename) = &binding.typename {
            let target_type = Type::from_tokens(typename)?;
            if let Some(target_type) = &target_type {
                // Type checking
                if !Type::deep_equality(&val_type, &target_type) {
                    let msg = format!("Cannot bind value of type {} to variable of type {}.",
                        val_type.to_string(), target_type.to_string());
                    return Err(RuntimeError { token: typename[0].clone(), msg });
                }
            }
            Some(target_type)
        } else {
            None
        };

        // Set the abstraction type
        match (&mut val, target_type) {
            (
                Val::Abstraction(abs, return_val),
                Some(Some(Type::Abstraction(target_param, target_return)))
            ) => {
                abs.paramtype = target_param.clone().map(|b| *b);
                *return_val = target_return.clone();
            }
            _ => {}
        }

        Ok(
            if let Some(in_expr) = &binding.in_expr {
                self.interpret(&in_expr.beta_reduction(&binding.name, &val))?
            } else {
                Val::Unit
            }
        )
    }
    pub fn visit_primary(&self, tok: &Token) -> Result<Val, RuntimeError> {
        match tok.t {
            TokenType::String => {
                Ok(Val::String(tok.lexeme[1..tok.lexeme.len() - 1].to_string()))
            }
            TokenType::Number => {
                Ok(Val::Number(tok.lexeme.parse().unwrap()))
            }
            TokenType::Unit => {
                Ok(Val::Unit)
            }
            TokenType::Identifer => {
                let msg = format!("Reference to unbound variable {}.", tok.lexeme);
                Err(RuntimeError{ token: tok.clone(), msg })

            }
            _ => panic!("INTERPRETER FAILED in visit_primary: Found token not of type String, Number, Unit, or Identifier.")
        }
    }
    fn workout_return_type(expr: &Expr) -> Result<Option<Box<Type>>, RuntimeError> {
        match expr {
            Expr::Abstraction(def) => {
                let argtype = def.paramtype
                    .clone()
                    .map(|v| Type::from_tokens(&v))
                    .transpose()?
                    .flatten()
                    .map(|v| Box::new(v));
                Ok(Some(Box::new(Type::Abstraction(argtype, None))))
            }
            _ => Ok(None)
        }
    }
    pub fn visit_abstraction(&self, def: &AbstractionDef)-> Result<Val, RuntimeError> {
        let arg = def.param.clone();
        let argtype = def.paramtype
            .clone()
            .map(|v| Type::from_tokens(&v))
            .transpose()?
            .flatten();
        let body = def.body.clone();
        let return_type = Self::workout_return_type(&body)?;

        Ok(Val::Abstraction(Abstraction { param: arg, paramtype: argtype, body }, return_type))
    }
    fn visit_call(&self, call: &Call) -> Result<Val, RuntimeError> {
        let callee = self.interpret(&call.callee)?;
        let arg = self.interpret(&call.arg)?;
        match callee {
            Val::Abstraction(abstraction, target_return_t) => {
                // Type check the argument and parameter
                let argument_type = arg.to_type();
                let parameter_type = abstraction.paramtype;
                if let Some(parameter_type) = &parameter_type {
                    if !Type::deep_equality(&argument_type, &parameter_type) {
                        let msg = format!("Attempt to bind argument of type {} to parameter of type {}.",
                            argument_type.to_string(), parameter_type.to_string());
                        return Err( RuntimeError { token: abstraction.param, msg })
                    }

                }
                // Run the function
                let return_val = self.interpret(&abstraction.body.beta_reduction(&abstraction.param.lexeme, &arg))?;

                // Type check the return value
                if let Some(target_return_t) = target_return_t {
                    let return_t = return_val.to_type();
                    if !Type::deep_equality(&return_t, &(*target_return_t)) {
                        let _msg = format!("Function returned type {}, expected type {}.",
                            return_t.to_string(), target_return_t.to_string()
                        );
                        panic!("Function returned {}, expected {}.", return_t.to_string(), target_return_t.to_string());
                        // TODO
                        // return Err( RuntimeError { token: , msg })
                    }
                }

                Ok(return_val)
            }
            _ => {
                // TODO
                panic!("Attempt to call non-abstraction.");
            }
        }
    }
    pub fn interpret(&self, expr: &Expr) -> Result<Val, RuntimeError> {
        match expr {
            Expr::Binary(binary) => Ok(self.visit_binary(&(*binary))?),
            Expr::Binding(binding) => Ok(self.visit_binding(&(*binding))?),
            Expr::Primary(tok) => Ok(self.visit_primary(tok)?),
            Expr::Abstraction(def) => Ok(self.visit_abstraction(&(*def))?),
            Expr::Call(call) => Ok(self.visit_call(&(*call))?),
            Expr::Beta(val) => Ok((**val).clone()),
        }
    }
}
