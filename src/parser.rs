use crate::scanner::{Token, TokenType};
use crate::expr::*;

use std::cell::Cell;

/*
* Context-free Grammar
*
* program -> expr EOF ;
* expr -> term ;
* term -> factor ( ("+" | "-") factor )* ;
* factor -> group ( ("*" | "/") group )* ;
* group -> "(" expr ")" | primary ;
* primary -> STRING | NUMBER | IDENTIFIER ;
*
*/

pub struct ParserError {
    pub line: usize,
    pub col: usize,
    pub msg: String,
}

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    cur: Cell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        let cur = Cell::new(0);
        Self { tokens, cur }
    }

    fn advance(&self) -> Token {
        let cur = self.cur.get();
        if cur < self.tokens.len() {
            self.cur.set(cur + 1);
            self.tokens[cur].clone()
        }
        else {
            self.tokens.last().unwrap().clone()
        }
    }

    fn consume(&self, t: TokenType) {
        let token = self.advance();
        assert_eq!(token.t, t);
    }

    fn peek(&self) -> Token {
        let cur = self.cur.get();
        if cur < self.tokens.len() {
            self.tokens[cur].clone()
        }
        else {
            self.tokens.last().unwrap().clone()
        }
    }

    fn is_match(&self, t: TokenType) -> bool {
        self.peek().t == t
    }

    fn any_match(&self, types: &[TokenType]) -> bool {
        let current_token = self.peek();
        for t in types {
            if current_token.t == *t {
                return true;
            }
        }
        false
    }

    // primary -> STRING | NUMBER | IDENTIFIER ;
    fn primary(&self) -> Result<Expr, ParserError> {
        if self.any_match(&[TokenType::String, TokenType::Number, TokenType::Identifer]) {
            Ok(Expr::Primary(self.advance()))
        }
        else {
            let token = self.advance();
            let line = token.line;
            let col = token.col;
            let msg = format!("Expected primary expression, but found '{}'.", token.lexeme);
            Err(ParserError{ line, col, msg })
        }
    }

    // group -> "(" expr ")" | primary ;
    fn group(&self) -> Result<Expr, ParserError> {
        if self.is_match(TokenType::LParen) {
            self.consume(TokenType::LParen);
            let expr = self.expr()?;
            self.consume(TokenType::RParen);
            Ok(expr)
        }
        else {
            Ok(self.primary()?)
        }
    }

    // factor -> group ( ("*" | "/") group )* ;
    fn factor(&self) -> Result<Expr, ParserError> {
        let mut expr = self.group()?;
        while self.any_match(&[TokenType::Star, TokenType::Slash]) {
            let op = self.advance();
            let right = self.group()?;
            expr = Expr::Binary( Box::new(Binary{left: expr, op, right}) )
        }
        Ok(expr)
    }

    // term -> factor ( ("+" | "-") factor )* ;
    fn term(&self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while self.any_match(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.advance();
            let right = self.factor()?;
            expr = Expr::Binary( Box::new(Binary{left: expr, op, right}) )
        }
        Ok(expr)
    }

    // expr -> term ;
    fn expr(&self) -> Result<Expr, ParserError> {
        self.term()
    }

    // program -> expr EOF ;
    fn program(&self) -> Result<Expr, ParserError> {
        let expr = self.expr();

        let next = self.advance();
        if next.t != TokenType::EOF {
            let line = next.line;
            let col = next.col;
            let msg = "Expected end of file.".to_string();
            return Err(ParserError{ line, col, msg });
        }

        expr
    }

    pub fn parse(&self) -> Result<Expr, ParserError> {
        self.program()
    }
}
