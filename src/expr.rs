use crate::scanner::Token;

#[derive(Debug)]
pub struct Binary {
    pub left: Expr,
    pub op: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Binding {
    pub name: String,
    pub typename: Option<Token>,
    pub op: Token,
    pub val: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Primary(Token),
    Binary(Box<Binary>),
    Binding(Box<Binding>),
}
