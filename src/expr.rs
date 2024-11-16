use crate::scanner::{Token, TokenType};
use crate::runtime::Val;

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Expr,
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub typename: Option<Vec<Token>>,
    pub op: Token,
    pub val: Expr,
    pub in_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct AbstractionDef {
    pub param: Token,
    pub paramtype: Option<Vec<Token>>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub arg: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Primary(Token),
    Binary(Box<Binary>),
    Let(Box<Binding>),
    Abstraction(Box<AbstractionDef>),
    Call(Box<Call>),
    Beta(Box<Val>),
}

impl Expr {
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
            Expr::Binary(bin) => {
                Expr::Binary(Box::new(Binary{
                    left: bin.left.beta_reduction(name, val),
                    op: bin.op.clone(),
                    right: bin.right.beta_reduction(name, val)
                }))
            }
            Expr::Let(let_expr) => {
                Expr::Let(Box::new(Binding{
                    name: let_expr.name.clone(),
                    typename: let_expr.typename.clone(),
                    op: let_expr.op.clone(),
                    val: let_expr.val.beta_reduction(name, val),
                    in_expr: let_expr.in_expr.clone().map(|v| v.beta_reduction(name, val)),
                }))
            }
            Expr::Abstraction(abs) => {
                Expr::Abstraction(Box::new(AbstractionDef {
                    param: abs.param.clone(),
                    paramtype: abs.paramtype.clone(),
                    body: abs.body.beta_reduction(name, val),
                }))
            }
            Expr::Call(call) => {
                Expr::Call(Box::new(Call {
                    callee: call.callee.beta_reduction(name, val),
                    arg: call.arg.beta_reduction(name, val),
                }))
            }
            Expr::Beta(_) => {
                self.clone()
            }
        }
    }
}
