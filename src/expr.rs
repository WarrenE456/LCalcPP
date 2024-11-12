use crate::scanner::Token;

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
}

#[derive(Debug, Clone)]
pub struct AbstractionDef {
    pub arg: String,
    pub argtype: Vec<Token>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Primary(Token),
    Binary(Box<Binary>),
    Binding(Box<Binding>),
    Abstraction(Box<AbstractionDef>),
}
