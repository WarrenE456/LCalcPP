use crate::expr::*;
use crate::scanner::{Token, TokenType};

pub enum Val {
    Number(f64),
    String(String),
}

impl Val {
    pub fn to_string(&self) -> String {
        match self {
            Val::Number(n) => n.to_string(),
            Val::String(s) => s.clone(),
        }
    }
}

impl Val {
    pub fn type_to_str(&self) -> &str {
        match self {
            Val::Number(_) => "Number",
            Val::String(_) => "String",
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
    // TODO: env
}

// TODO: represent arithmatic with functions
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
                        Ok(Val::String(a))
                    }
                    (a, b) => {
                        let token = binary.op.clone();
                        let msg = format!("Attempt to use plus operator on {} and {}.", a.type_to_str(), b.type_to_str());
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
                        let msg = format!("Attempt to use minus operator on {} and {}.", a.type_to_str(), b.type_to_str());
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
                        let msg = format!("Attempt to multiplication operator on {} and {}.", a.type_to_str(), b.type_to_str());
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
                        let msg = format!("Attempt to division operator on {} and {}.", a.type_to_str(), b.type_to_str());
                        Err(RuntimeError { token, msg })
                    }
                }
            }
            _ => panic!("INTERPRETER FAILED in visist_binary: Operator '{}' type did not match any.", binary.op.lexeme),
        }
    }
    pub fn visit_primary(&self, tok: &Token) -> Val {
        match tok.t {
            TokenType::String => {
                return Val::String(tok.lexeme[1..tok.lexeme.len() - 1].to_string())
            }
            TokenType::Number => {
                return Val::Number(tok.lexeme.parse().unwrap());
            }
            _ => panic!("INTERPRETER FAILED in visit_primary: Found token not of type String or Number.")
        }
    }
    pub fn interpret(&self, expr: &Expr) -> Result<Val, RuntimeError> {
        match expr {
            Expr::Primary(tok) => Ok(self.visit_primary(tok)),
            Expr::Binary(binary) => Ok(self.visit_binary(&(*binary))?),
        }
    }
}
