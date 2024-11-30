use crate::runtime::{Val, Type, RuntimeError, Interpreter};
use crate::scanner::Token;

fn equality_check(a: &Val, b: &Val, interpreter: &Interpreter) -> Result<bool, RuntimeError> {
    match (a.unwrap(interpreter)?, b.unwrap(interpreter)?) {
        (Val::Number(a), Val::Number(b)) => Ok(a == b),
        (Val::String(a), Val::String(b)) => Ok(*a == *b),
        (Val::Unit, Val::Unit) => Ok(true),
        _ => {
            let msg = format!("Attempted equality check between invalid types {} and {}.",
                a.to_type().to_string(), b.to_type().to_string());
            Err(RuntimeError { token: Token::garbage(), msg })
        }
    }
}

fn greater_than_check(a: &Val, b: &Val, interpreter: &Interpreter) -> Result<bool, RuntimeError> {
    match (a.unwrap(interpreter)?, b.unwrap(interpreter)?) {
        (Val::Number(a), Val::Number(b)) => Ok(a > b),
        _ => {
            let msg = format!("Attempted greater than check between invalid types {} and {}.",
                a.to_type().to_string(), b.to_type().to_string());
            Err(RuntimeError { token: Token::garbage(), msg })
        }
    }
}

#[derive(Clone, Debug)]
pub struct Equal {
    a: Option<Val>,
}

// TODO fix type of, say, 'EQUAL 1 1'. Currently it is (Any -> Any), but it should be (Any -> (Any -> Any)).
impl Equal {
    pub fn new() -> Self {
        Equal { a: None }
    }
    pub fn call(&self, arg: &Val, interpreter: &Interpreter) -> Result<Val, RuntimeError> {
        if let Some(a) = &self.a {
            let is_equal = equality_check(&a, &arg, interpreter)?;
            Ok(
                if  is_equal { Val::new_true()  }
                else         { Val::new_false() }
            )
        }
        else {
            Ok(Val::BuiltIn(BuiltIn::Equal(Box::new(Equal { a: Some(arg.clone()) }))))
        }
    }
    pub fn to_type(&self) -> Type {
        Type::Abstraction(
            None,
            if let Some(_) = &self.a {
                None
            } else {
                Some(Box::new(Type::Abstraction(None, None)))
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct Greater {
    a: Option<Val>,
}

impl Greater {
    pub fn new() -> Self {
        Self { a: None }
    }
    pub fn call(&self, arg: &Val, interpreter: &Interpreter) -> Result<Val, RuntimeError> {
        if let Some(a) = &self.a {
            let is_greater = greater_than_check(&a, &arg, interpreter)?;
            Ok(
                if  is_greater { Val::new_true()  }
                else         { Val::new_false() }
            )
        }
        else {
            Ok(Val::BuiltIn(BuiltIn::Greater(Box::new(Greater { a: Some(arg.clone()) }))))
        }
    }
    pub fn to_type(&self) -> Type {
        Type::Abstraction(
            None,
            if let Some(_) = &self.a {
                None
            } else {
                Some(Box::new(Type::Abstraction(None, None)))
            }
        )
    }
}

struct Print {
}

impl Print {
    pub fn call(arg: &Val) {
        println!("{}", arg.to_string());
    }
    pub fn to_type() -> Type {
        Type::Abstraction(None, None)
    }
}

#[derive(Clone, Debug)]
pub enum BuiltIn {
    Equal(Box<Equal>),
    Greater(Box<Greater>),
    Print,

}

impl BuiltIn {
    pub fn to_type(&self) -> Type {
        match self {
            Self::Equal(eq) => eq.to_type(),
            Self::Greater(gr) => gr.to_type(),
            Self::Print => Print::to_type(),
        }
    }
    pub fn call(&self, arg: &Val, interpreter: &Interpreter) -> Result<Val, RuntimeError> {
        match self {
            Self::Equal(eq) => eq.call(arg, interpreter),
            Self::Greater(gr) => gr.call(arg, interpreter),
            Self::Print => { Print::call(&arg.unwrap(interpreter)?); Ok(Val::Unit) },
        }
    }
}
