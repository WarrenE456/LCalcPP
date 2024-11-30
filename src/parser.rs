// TODO nested type definitions (e.g. (Number -> Number) -> (Number -> Number))
use crate::scanner::{Token, TokenType};
use crate::expr::*;

use std::cell::Cell;

/*
* Context-free Grammar
*
* program   -> expr EOF ;
* expr      -> binding;
* binding   -> "let" IDENTIFIER ( ":" types )? "=" lambda ( "in" binding )? | lambda;
* lambda    -> "L" IDENTIFIER ":" types "." expr | term ";"
* term      -> factor ( ("+" | "-") factor )* ;
* factor    -> apply ( ("*" | "/") apply )* ;
* apply     -> group group* ;
* group     -> "(" expr ")" | primary ;
* primary   -> STRING | NUMBER | IDENTIFIER ;
*
* types     -> type ( "->" type )* ;
* type      -> ( "(" types ")" | IDENTIFIER ) ;
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


#[derive(Debug, Clone)]
pub enum ParserType {
    Abs(Box<ParserType>, Box<ParserType>),
    Base(Token),
}

impl ParserType {
    pub fn tok(&self) -> Token {
        match self {
            Self::Base(tok) => {
                tok.clone()
            }
            Self::Abs(first, _) => {
                (*first).tok()
            }
        }
    }
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
        if self.any_match(&[TokenType::String, TokenType::Number, TokenType::Identifer, TokenType::Unit]) {
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
            if let Err(tok) = self.expect(TokenType::RParen) {
                let msg = format!("Expected ')' to match '(', found '{}'.", tok.lexeme);
                Err( ParserError { line: tok.line, col: tok.line, msg } )
            } else {
                Ok(expr)
            }
        }
        else {
            Ok(self.primary()?)
        }
    }

    // apply -> group group* ;
    fn apply(&self) -> Result<Expr, ParserError> {
        let callee_tok = &self.peek();
        let mut expr = self.group()?;
        while self.any_match(&[TokenType::LParen, TokenType::Number, TokenType::String, TokenType::Identifer, TokenType::Unit]) {
            expr = Expr::Call(Box::new(Call { callee: expr, callee_tok: callee_tok.clone(), arg: self.group()? }));
        }
        Ok(expr)
    }

    // factor -> apply ( ("*" | "/") apply )* ;
    fn factor(&self) -> Result<Expr, ParserError> {
        let mut expr = self.apply()?;
        while self.any_match(&[TokenType::Star, TokenType::Slash]) {
            let op = self.advance();
            let right = self.apply()?;
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

    // type -> ( "(" types ")" | IDENTIFIER ) ;
    fn _type(&self) -> Result<ParserType, ParserError> {
        if self.is_match(TokenType::LParen) {
            let _ = self.advance();
            let _type = self._types()?;
            if let Err(tok) = self.expect(TokenType::RParen) {
                let msg = format!("Expected ')' to match '(', found '{}'.", tok.lexeme);
                Err(ParserError { msg, line: tok.line, col: tok.col })
            } else {
                Ok(_type)
            }
        } else {
            Ok(ParserType::Base(self.expect(TokenType::Identifer).map_err(|e| ParserError {
                msg: format!("Expected type identifier found {}.", e.lexeme),
                line: e.line,
                col: e.col
            })?))
        }
    }
    
    // types -> type ( "->" types )? ;
    fn _types(&self) -> Result<ParserType, ParserError> {
        let mut _type = self._type()?;
        if self.is_match(TokenType::Arrow) {
            let _ = self.advance();
            _type = ParserType::Abs(
                Box::new(_type),
                Box::new(self._types()?)
            );
        }
        Ok(_type)
    }

    // Fix 'end of file' error message when attemping to use untyped lambda e.g. L x. x
    // lambda -> "L" IDENTIFIER ":" type "." expr | term ";"
    fn lambda(&self) -> Result<Expr, ParserError> {
        if self.is_match(TokenType::Lambda) {
            let _ = self.advance();

            let param = self.expect(TokenType::Identifer).map_err(|e| {
                let msg = format!("Expected identifier an identifier after the lambda, found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?.clone();

            let paramtype = 
            if self.is_match(TokenType::Colon) { 
                let _ = self.advance();
                Some(self._types()?)
            } else {
                None
            };

            let _ = self.expect(TokenType::Dot).map_err(|e| {
                let msg = format!("Expected a dot after the argument type, found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?;

            let body = self.expr()?;

            Ok(Expr::Abstraction(Box::new(AbstractionDef { param, paramtype, body })))
        } 
        else {
            Ok(self.term()?)
        }
    }


    // binding -> "let" IDENTIFIER ( ":" type )? "=" lambda ( "in" binding )? | lambda;
    fn binding(&self) -> Result<Expr, ParserError> {
        if self.is_match(TokenType::Let) {
            let _ = self.advance(); 

            let name =  self.expect(TokenType::Identifer).map_err(|e| {
                let msg = format!("Expected a variable name, found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?.lexeme;

            let typename = if self.is_match(TokenType::Colon) {
                let _ = self.advance();
                Some(self._types())
            } else {
                None
            }.transpose()?;

            let op = self.expect(TokenType::Equal).map_err(|e| {
                let msg = format!("Expected '=', found '{}'.", e.lexeme);
                ParserError { line: e.line, col: e.col, msg }
            })?;

            let val = self.lambda()?;

            let in_expr = if self.is_match(TokenType::In) {
                let _ = self.advance();
                Some(self.binding()?)
            } else {
                None
            };

            Ok(Expr::Binding(Box::new(Binding { name, ptype: typename, op, val, in_expr })))
        }   
        else {
            self.lambda()
        }
    }

    // expr -> in ;
    fn expr(&self) -> Result<Expr, ParserError> {
        self.binding()
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
