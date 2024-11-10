use crate::scanner::{Token, TokenType};
use crate::expr::*;

use std::cell::Cell;

/*
* Context-free Grammar
*
* program -> expr EOF ;
* expr -> in ;
* in -> binding ( "in" binding )* ;
* binding -> "let" IDENTIFIER ( ":" type )? "=" binding | term;
* term -> factor ( ("+" | "-") factor )* ;
* factor -> group ( ("*" | "/") group )* ;
* group -> "(" expr ")" | primary ;
* primary -> STRING | NUMBER | IDENTIFIER ;
*
* type -> IDENTIFIER ;
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

    fn expect(&self, t: TokenType) -> Result<Token, Token> {
        let token = self.advance();
        if token.t == t {
            Ok(token)
        } else {
            Err(token)
        }
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
            let _ = self.advance();
            let expr = self.expr()?;
            let _ = self.advance();
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
    
    // type -> IDENTIFIER ;
    fn _type(&self) -> Result<Token, ParserError> {

        let typename =  self.expect(TokenType::Identifer).map_err(|e| {
            let msg = format!("Expected a type, found '{}'.", e.lexeme);
            ParserError { line: e.line, col: e.col, msg }
        })?;

        Ok(typename)
    }

    // binding -> "let" IDENTIFIER ( ":" type ) "=" term | term;
    fn binding(&self) -> Result<Expr, ParserError> {
        if self.is_match(TokenType::Let) {
            let _ = self.advance(); 

            let name =  self.expect(TokenType::Identifer).map_err(|e| {
                let msg = format!("Expected a variable name, found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?.lexeme;

            let typename = if self.is_match(TokenType::Colon) {
                let _ = self.advance();
                Some(self._type())
            } else {
                None
            }.transpose()?;

            let op = self.expect(TokenType::Equal).map_err(|e| {
                let msg = format!("Expected '=', found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?;

            let val = self.term()?;

            Ok(Expr::Binding(Box::new(Binding { name, typename, op, val })))
        }   
        else {
            self.term()
        }
    }

    // in -> binding ( "in" binding )* ;
    fn _in(&self) -> Result<Expr, ParserError> {
        let mut expr = self.binding()?;
        while self.is_match(TokenType::In) {
            let op = self.advance();
            let right = self.binding()?;
            expr = Expr::Binary(Box::new(Binary { left: expr, op, right }));
        }
        Ok(expr)
    }

    // expr -> in ;
    fn expr(&self) -> Result<Expr, ParserError> {
        self._in()
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
