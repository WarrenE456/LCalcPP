// TODO fix string operations
use crate::expr::*;
use crate::scanner::{Token, TokenType};

use std::cell::RefCell;

#[derive(Clone)]
pub enum Val {
    Number(f64),
    String(String),
    Unit,
}

#[derive(Eq, PartialEq)]
enum Primative {
    Number,
    String,
    Unit
}

impl Primative {
    pub fn from_typename(typename: &str) -> Option<Self> {
        match typename {
            "Number" => Some(Primative::Number),
            "String" => Some(Primative::String),
            "Unit"   => Some(Primative::Unit),
            _ => None,
        }
    }
    pub fn to_str(&self) -> &str {
        match self {
            Primative::Number =>   "Number",
            Primative::String =>   "String",
            Primative::Unit   =>   "Unit",
        }
    }
}

impl Val {
    pub fn to_string(&self) -> String {
        match self {
            Val::Number(n) => n.to_string(),
            Val::String(s) => s.clone(),
            Val::Unit => "()".to_string(),
        }
    }
    pub fn to_primative(&self) -> Primative {
        match self {
            Val::Number(_) => Primative::Number,
            Val::String(_) => Primative::String,
            Val::Unit => Primative::Unit,
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

pub struct Interpreter {
    env: RefCell<Vec<(String, Val)>>,
}

// TODO: represent arithmatic with functions
impl Interpreter {
    pub fn new() -> Self {
        Interpreter { env: RefCell::new(vec![]) }
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
                        Ok(Val::String(a))
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to use plus operator on {} and {}.",
                            a.to_primative().to_str(), b.to_primative().to_str());
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
                            a.to_primative().to_str(), b.to_primative().to_str());
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
                            a.to_primative().to_str(), b.to_primative().to_str());
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
                            a.to_primative().to_str(), b.to_primative().to_str());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            In => Ok(right),
            _ => panic!("INTERPRETER FAILED in visist_binary: Operator '{}' type did not match any.", binary.op.lexeme),
        }
    }
    pub fn visit_binding(&self, binding: &Binding) -> Result<Val, RuntimeError> {
        let val = self.interpret(&binding.val)?;

        // Type checking
        if let Some(typename) = &binding.typename {
            let target_type = match Primative::from_typename(&typename.lexeme) {
                Some(v) => Ok(v),
                None => {
                    let msg = format!("Reference to unbound type '{}'.", typename.lexeme);
                    Err(RuntimeError { token: typename.clone(), msg })
                }
            }?;
            
            let val_type = val.to_primative();
            if val_type != target_type {
                let msg = format!("Cannot bind value of type {} to variable of type {}.",
                    val_type.to_str(), target_type.to_str());
                return Err(RuntimeError { token: typename.clone(), msg });
            }
        }

        self.env.borrow_mut().push((binding.name.clone(), val));

        Ok(Val::Unit)
    }
    pub fn visit_primary(&self, tok: &Token) -> Result<Val, RuntimeError> {
        match tok.t {
            TokenType::String => {
                Ok(Val::String(tok.lexeme[1..tok.lexeme.len() - 1].to_string()))
            }
            TokenType::Number => {
                Ok(Val::Number(tok.lexeme.parse().unwrap()))
            }
            TokenType::Identifer => {
                // TODO: Stop granny shiftin', not double clutching like you should
                for (name, val) in self.env.borrow().iter().rev() {
                    if *name == tok.lexeme {
                        return Ok(val.clone())
                    }
                }
                let msg = format!("Reference to unbound variable {}.", tok.lexeme);
                Err(RuntimeError{ token: tok.clone(), msg })

            }
            _ => panic!("INTERPRETER FAILED in visit_primary: Found token not of type String, Number, or Identifier.")
        }
    }
    pub fn interpret(&self, expr: &Expr) -> Result<Val, RuntimeError> {
        match expr {
            Expr::Binary(binary) => Ok(self.visit_binary(&(*binary))?),
            Expr::Binding(binding) => Ok(self.visit_binding(&(*binding))?),
            Expr::Primary(tok) => Ok(self.visit_primary(tok)?),
        }
    }
}
