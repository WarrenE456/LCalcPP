use crate::scanner::Token;

#[derive(Debug)]
pub struct Binary {
    pub left: Expr,
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Binary>),
    Primary(Token)
}
