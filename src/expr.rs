use crate::parser::ParserType;
use crate::scanner::{Token, TokenType};
use crate::runtime::{Val, Arg};

use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Negate {
    pub op: Token,
    pub val: Expr,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Expr,
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub ptype: Option<ParserType>,
    pub op: Token,
    pub val: Expr,
    pub in_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeBinding {
    pub name: String,
    pub op: Token,
    pub val: ParserType,
    pub in_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct AbstractionDef {
    pub param: Token,
    pub paramtype: Option<ParserType>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub callee_tok: Token,
    pub arg: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Primary(Token),
    Negate(Box<Negate>),
    Binary(Box<Binary>),
    Binding(Box<Binding>),
    TypeBinding(Box<TypeBinding>),
    Abstraction(Box<AbstractionDef>),
    Call(Box<Call>),
    Beta(Box<Val>),
}

impl Expr {
    pub fn wrap(&self) -> Val {
        match self {
            Expr::Beta(val) => {
                match &**val {
                    Val::Arg(_) => *val.clone(),
                    val => val.clone(),
                }
            }
            expr => Val::Arg(RefCell::new(Arg::Expr(Box::new(expr.clone()), None))),
        }
    }
    pub fn beta_reduction(&self, name: &String, val: &Val) -> Expr {
        match self {
            Expr::Primary(tok) => {
                match tok.t {
                    TokenType::Identifer => {
                        if tok.lexeme == *name {
                            Self::Beta(Box::new(val.clone()))
                        } else {
                            self.clone()
                        }
                    }
                    _ => self.clone(),
                }
            }
            Expr::Negate(negate) => {
                Expr::Negate(Box::new(Negate{
                    op: negate.op.clone(),
                    val: negate.val.clone().beta_reduction(name, val),
                }))
            }
            Expr::Binary(bin) => {
                Expr::Binary(Box::new(Binary{
                    left: bin.left.beta_reduction(name, val),
                    op: bin.op.clone(),
                    right: bin.right.beta_reduction(name, val)
                }))
            }
            Expr::Binding(bind) => {
                if *name != bind.name {
                    Expr::Binding(Box::new(Binding{
                        name: bind.name.clone(),
                        ptype: bind.ptype.clone(),
                        op: bind.op.clone(),
                        val: bind.val.beta_reduction(name, val),
                        in_expr: bind.in_expr.clone().map(|v| v.beta_reduction(name, val)),
                    }))
                } else {
                    self.clone()
                }
            }
            Expr::TypeBinding(binding) => {
                Expr::TypeBinding(Box::new(TypeBinding{
                    name: binding.name.clone(),
                    op: binding.op.clone(),
                    val: binding.val.clone(),
                    in_expr: binding.in_expr.clone().map(|v| v.beta_reduction(name, val)),
                }))
            }
            Expr::Abstraction(abs) => {
                if *name != abs.param.lexeme {
                    Expr::Abstraction(Box::new(AbstractionDef {
                        param: abs.param.clone(),
                        paramtype: abs.paramtype.clone(),
                        body: abs.body.beta_reduction(name, val),
                    }))
                } else {
                    self.clone()
                }
            }
            Expr::Call(call) => {
                Expr::Call(Box::new(Call {
                    callee: call.callee.beta_reduction(name, val),
                    callee_tok: call.callee_tok.clone(),
                    arg: call.arg.beta_reduction(name, val),
                }))
            }
            Expr::Beta(_) => {
                self.clone()
            }
        }
    }
}
