use crate::runtime::{Val, Type, RuntimeError};
use crate::scanner::{TokenType, Token};

fn equality_check(a: &Val, b: &Val) -> Result<bool, String> {
    match (a, b) {
        (Val::Number(a), Val::Number(b)) => Ok(*a == *b),
        (Val::String(a), Val::String(b)) => Ok(*a == *b),
        (Val::Unit, Val::Unit) => Ok(true),
        _ => {
            let msg = format!("Attempted equality check between invalid types {} and {}.",
                a.to_type().to_string(), b.to_type().to_string());
            Err(msg)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Equal {
    a: Option<Val>,
}

impl Equal {
    pub fn new() -> Self {
        Equal { a: None }
    }
    pub fn call(&self, arg: &Val) -> Result<Val, RuntimeError> {
        if let Some(a) = &self.a {
            let is_equal = equality_check(&a, &arg).map_err(|msg| RuntimeError { token: Token::garbage(), msg })?;
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
pub enum BuiltIn {
    Equal(Box<Equal>),
}

impl BuiltIn {
    pub fn to_type(&self) -> Type {
        match self {
            Self::Equal(eq) => eq.to_type(),
        }
    }
    pub fn call(&self, arg: &Val) -> Result<Val, RuntimeError> {
        match self {
            Self::Equal(eq) => eq.call(arg),
        }
    }
}
