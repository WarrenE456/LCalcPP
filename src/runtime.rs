use crate::expr::*;
use crate::builtin::*;
use crate::scanner::{Token, TokenType};
use crate::parser::ParserType;

use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Abstraction {
    pub param: Token,
    pub paramtype: Option<Type>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub enum Arg {
    Expr(Box<Expr>, Option<(Type, Token)>),
    Val(Box<Val>),
}

impl Arg {
    pub fn new(e: Expr) -> Self {
        Self::Expr(Box::new(e), None)
    }
    pub fn get(&mut self) -> Result<Val, RuntimeError> {
        match self {
            Self::Expr(expr, tup) => {
                let val = Interpreter::interpret(&*expr)?;
                if let Some((t, param_tok)) = tup {
                    let arg_type = val.to_type();
                    if !Type::deep_equality(t, &arg_type) {
                        let msg = format!("Attempt to bind argument of type {} to parameter of type {}.",
                            arg_type.to_string(), t.to_string());
                        return Err( RuntimeError { token: param_tok.clone(), msg })
                    }
                }
                *self = Self::Val(Box::new(val.clone()));
                Ok(val)
            }
            Self::Val(val) => {
                Ok(*val.clone())
            }
        }
    }
    pub fn restrict_type(&mut self, target: Type, param_tok: Token) -> Result<(), RuntimeError> {
        match self {
            Arg::Expr(_, tup) => {
                *tup = Some((target, param_tok)); 
                Ok(())
            }
            Arg::Val(v) => {
                let arg_type = v.to_type();
                if !Type::deep_equality(&target, &arg_type) {
                    let msg = format!("Attempt to bind argument of type {} to parameter of type {}.",
                        arg_type.to_string(), target.to_string());
                    Err( RuntimeError { token: param_tok.clone(), msg })
                } else {
                    Ok(())
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Val {
    Number(f64),
    String(String),
    Abstraction(Abstraction, Option<Box<Type>>), 
    BuiltIn(BuiltIn),
    Arg(RefCell<Arg>),
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
    pub fn new_false() -> Val {
        let body = Expr::Abstraction(
            Box::new(
                AbstractionDef {
                    param: Token { t: TokenType::Identifer, lexeme: "b".to_string(), line: 0, col: 0 },
                    paramtype: None,
                    body: Expr::Primary(Token { t: TokenType::Identifer, lexeme: "b".to_string(), line: 0, col: 0 })
                }
            )
        );
        Val::Abstraction(Abstraction {
            param: Token { t: TokenType::Identifer, lexeme: "a".to_string(), line: 0, col: 0 },
            paramtype: None,
            body
        }, None)
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
    pub fn from_token(tok: &Token) -> Result<Option<Type>, RuntimeError> {
            Type::from_typename(&tok.lexeme).ok_or_else(|| {
                let msg = format!("Reference to unbound type '{}'.", tok.lexeme);
                RuntimeError { token: tok.clone(), msg }
            })
    }
    pub fn parsertype_to_runtimetype(ptype: &ParserType) -> Result<Option<Type>, RuntimeError> {
        match ptype {
            ParserType::Base(tok) => {
                Self::from_token(tok)
            }
            ParserType::Abs(first, second) => {
                Ok(Some(Type::Abstraction(
                    Self::parsertype_to_runtimetype(&first)?.map(|x| Box::new(x)),
                    Self::parsertype_to_runtimetype(&second)?.map(|x| Box::new(x)),
                )))
            }
        }
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
    pub fn unwrap(&self) -> Result<Val, RuntimeError> {
        match self {
            Val::Arg(ev) => ev.borrow_mut().get(),
            val => Ok(val.clone())
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Val::Number(n)                => n.to_string(),
            Val::String(s)             => s.clone(),
            Val::Unit                           => "()".to_string(),
            Val::Abstraction(_, _)              => "<abstraction>".to_string(),
            Val::BuiltIn(_)                     => "<built-in>".to_string(),
            Val::Arg(ev) => match &*ev.borrow() {
                Arg::Val(v)  => (*v).to_string(),
                Arg::Expr(_, _)            => panic!("INTERPRETER FAILED: Attempt to convert ExprVal::Expr to string."),
            },
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
            Val::BuiltIn(b) => b.to_type(),
            Val::Arg(ev) => match &*ev.borrow() {
                Arg::Val(v)  => (*v).to_type(),
                Arg::Expr(_, _)            => panic!("INTERPRETER FAILED: Attempt to convert ExprVal::Expr to type."),
            },
        }
    }
    pub fn new_true() -> Val {
        let body = Expr::Abstraction(
            Box::new(
                AbstractionDef {
                    param: Token { t: TokenType::Identifer, lexeme: "b".to_string(), line: 0, col: 0 },
                    paramtype: None,
                    body: Expr::Primary(Token { t: TokenType::Identifer, lexeme: "a".to_string(), line: 0, col: 0 })
                }
            )
        );
        Val::Abstraction(Abstraction {
            param: Token { t: TokenType::Identifer, lexeme: "a".to_string(), line: 0, col: 0 },
            paramtype: None,
            body
        }, None)
    }
    pub fn new_false() -> Val {
        let body = Expr::Abstraction(
            Box::new(
                AbstractionDef {
                    param: Token { t: TokenType::Identifer, lexeme: "b".to_string(), line: 0, col: 0 },
                    paramtype: None,
                    body: Expr::Primary(Token { t: TokenType::Identifer, lexeme: "b".to_string(), line: 0, col: 0 })
                }
            )
        );
        Val::Abstraction(Abstraction {
            param: Token { t: TokenType::Identifer, lexeme: "a".to_string(), line: 0, col: 0 },
            paramtype: None,
            body
        }, None)
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub msg: String,
}

fn str_mul(s: &mut String, n: f64) -> Result<(), String> {
    if n.fract() != 0.0 || n < 0.0  {
        Err("Attempt to multiply a string by anything but a natural number.".to_string())
    } else {
        let n: usize = n as usize;
        *s = s.repeat(n);
        Ok(())
    }
}

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { }
    }
    pub fn visit_binary(binary: &Binary) -> Result<Val, RuntimeError> {
        let left = Self::interpret(&binary.left)?.unwrap()?;
        let right = Self::interpret(&binary.right)?.unwrap()?;

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
                    (Val::Number(a), Val::String(b)) => {
                        let mut a = a.to_string();
                        a.push_str(&b);
                        Ok(Val::String(a))
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
    pub fn visit_binding(binding: &Binding) -> Result<Val, RuntimeError> {
        let mut val = Self::interpret(&binding.val)?;

        let val_type = val.to_type();
        let target_type =  if let Some(ptype) = &binding.ptype {
            let target_type = Type::parsertype_to_runtimetype(ptype)?;
            if let Some(target_type) = &target_type {
                // Type checking
                if !Type::deep_equality(&val_type, &target_type) {
                    let msg = format!("Cannot bind value of type {} to variable of type {}.",
                        val_type.to_string(), target_type.to_string());
                    return Err(RuntimeError { token: ptype.tok(), msg });
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
                Self::interpret(&in_expr.beta_reduction(&binding.name, &val))?
            } else {
                Val::Unit
            }
        )
    }
    pub fn visit_primary(tok: &Token) -> Result<Val, RuntimeError> {
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
                let builtin_mp = HashMap::from([
                    (String::from("EQUAL"), BuiltIn::Equal(Box::new(Equal::new()))),
                    (String::from("GREATER"), BuiltIn::Greater(Box::new(Greater::new()))),
                    (String::from("PRINT"), BuiltIn::Print),
                ]);
                if let Some(built_in) = builtin_mp.get(&tok.lexeme) {
                    Ok(Val::BuiltIn(built_in.clone()))
                } else {
                    let msg = format!("Reference to unbound variable {}.", tok.lexeme);
                    Err(RuntimeError{ token: tok.clone(), msg })
                }

            }
            _ => panic!("INTERPRETER FAILED in visit_primary: Found token not of type String, Number, Unit, or Identifier.")
        }
    }
    fn workout_return_type(expr: &Expr) -> Result<Option<Box<Type>>, RuntimeError> {
        match expr {
            Expr::Abstraction(def) => {
                let argtype = def.paramtype
                    .clone()
                    .map(|v| Type::parsertype_to_runtimetype(&v))
                    .transpose()?
                    .flatten()
                    .map(|v| Box::new(v));
                Ok(Some(Box::new(Type::Abstraction(argtype, None))))
            }
            _ => Ok(None)
        }
    }
    pub fn visit_abstraction(def: &AbstractionDef)-> Result<Val, RuntimeError> {
        let arg = def.param.clone();
        let argtype = def.paramtype
            .clone()
            .map(|v| Type::parsertype_to_runtimetype(&v))
            .transpose()?
            .flatten();
        let body = def.body.clone();
        let return_type = Self::workout_return_type(&body)?;

        Ok(Val::Abstraction(Abstraction { param: arg, paramtype: argtype, body }, return_type))
    }
    fn visit_call(call: &Call) -> Result<Val, RuntimeError> {
        let callee = Self::interpret(&call.callee)?.unwrap()?;
        let arg = call.arg.wrap();

        match callee {
            Val::Abstraction(abstraction, target_return_t) => {

                // Type check the argument and parameter
                let parameter_type = abstraction.paramtype;
                if let (Some(parameter_type), Val::Arg(arg)) = (&parameter_type, &arg) {
                    arg.borrow_mut().restrict_type(parameter_type.clone(), abstraction.param.clone())?;  
                }

                // Run the function
                let return_val =
                    Self::interpret(&abstraction.body.beta_reduction(&abstraction.param.lexeme, &arg))?
                    .unwrap()?;

                // Type check the return value
                if let Some(target_return_t) = target_return_t {
                    let return_t = return_val.to_type();
                    if !Type::deep_equality(&return_t, &(*target_return_t)) {
                        let msg = format!("Function returned type {}, expected type {}.",
                            return_t.to_string(), target_return_t.to_string()
                        );
                        return Err(RuntimeError { msg, token: call.callee_tok.clone() });
                    }
                }

                Ok(return_val)
            }
            Val::BuiltIn(b) => {
                Ok(b.call(&arg)?)
            }
            _ => {
                let msg = format!("Attempt to call non-abstraction {}.", callee.to_type().to_string());
                return Err(RuntimeError { msg, token: call.callee_tok.clone() });
            }
        }
    }
    pub fn interpret(expr: &Expr) -> Result<Val, RuntimeError> {
        match expr {
            Expr::Binary(binary) => Ok(Self::visit_binary(&(*binary))?),
            Expr::Binding(binding) => Ok(Self::visit_binding(&(*binding))?),
            Expr::Primary(tok) => Ok(Self::visit_primary(tok)?),
            Expr::Abstraction(def) => Ok(Self::visit_abstraction(&(*def))?),
            Expr::Call(call) => Ok(Self::visit_call(&(*call))?),
            Expr::Beta(val) => {
                Ok((**val).clone())
            },
        }
    }
}
