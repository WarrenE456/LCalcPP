use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // One character
    Plus,   Star,   Slash,
    Dot,    Equal,  Colon,

    // One or two chararacter
    Minus,  Arrow,

    // Variable characters or keyword
    String, Number, In, Let,
    Type,   Lambda, Identifer,
    LParen, RParen, Unit, Any,

    // Misc.
    EOF
}

#[derive(Debug)]
pub struct ScannerError {
    pub line: usize,
    pub col: usize,
    pub msg: String
}

#[derive(Debug, Clone)]
pub struct Token {
    pub t: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub col: usize,
} 

impl Token {
    pub fn garbage() -> Self {
        Token { t: TokenType::Identifer, lexeme: "".to_string(), line: 0, col: 0 }
    }
}

pub struct Scanner<'a> {
    program: &'a[u8],
    cur: Cell<usize>,
    line: Cell<usize>,
    col: Cell<usize>,
}

fn is_numeric(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_alpha(c: u8) -> bool {
    (b'a' <= c && c <= b'z') || (b'A' <= c && c <= b'Z') || c == b'_'
}

fn is_alpha_numeric(c: u8) -> bool {
    is_alpha(c) || is_numeric(c)
}


impl<'a> Scanner<'a> {
    pub fn new(program: &'a str) -> Self {
        let program = program.as_bytes();
        Scanner { program, cur: Cell::new(0), line: Cell::new(1), col: Cell::new(0) } 
    }
    fn advance(&self) -> Option<u8> {
        self.col.set(self.col.get() + 1);

        let cur = self.cur.get();
        return if cur < self.program.len() {
            self.cur.set(cur + 1);
            Some(self.program[cur])
        }
        else {
            None
        };
    }
    fn peek(&self) -> Option<u8> {
        let cur = self.cur.get();
        return if cur < self.program.len() {
            Some(self.program[self.cur.get()])
        } else {
            None
        };
    }
    fn is_match(&self, t: u8) -> bool {
        let c = self.peek();
        return if c.is_some() && c.unwrap() == t {
            true
        } else {
            false
        };
    }
    fn gen_token(&self, start: usize, t: TokenType) -> Token {
        let lexeme = String::from_utf8(self.program[start..self.cur.get()].to_vec()).unwrap();
        Token { t, lexeme, col: self.col.get(), line: self.line.get() }
    }
    fn get_next_token(&self) -> Result<Token, ScannerError> {
        use TokenType::*;
        let start = self.cur.get();
        let token_type = match self.advance() {
            None => Some(EOF),
            Some(c) => match c {
                // Sinlge characters
                b'+' => Some(Plus),
                b'*' => Some(Star),
                b'/' => Some(Slash),
                b'.' => Some(Dot),
                b'=' => Some(Equal),
                b':' => Some(Colon),

                // One or two characters
                b'-' => Some(if self.is_match(b'>') { self.advance(); Arrow} else { Minus }),
                b'(' => Some(if self.is_match(b')') { self.advance(); Unit} else { LParen }),
                b')' => Some(RParen), // Isn't actual two characters, but I want it to be with LParen

                // Identifiers and keywords

                // Whitespace
                b'\n' => { self.line.set(self.line.get() + 1); self.col.set(0); None }
                b' ' | b'\t' | b'\r' => None,

                // Comments
                b'#' => {
                    loop {
                        if let Some(a) = self.advance() {
                            if a == b'\n' { 
                                self.line.set(self.line.get() + 1); self.col.set(0);
                                break;
                            }
                        }
                        else {
                            break;
                        }
                    }
                    return self.get_next_token();
                }

                _ => {
                    // Hashmap of keywords and their type
                    let keywords: HashMap<&str, TokenType> = HashMap::from([
                        ("in", TokenType::In),
                        ("L", TokenType::Lambda),
                        ("let", TokenType::Let),
                        ("type", TokenType::Type),
                    ]);

                    // String
                    if c == b'"' {
                        self.advance();
                        // Find the end of the string
                        loop {
                            match self.advance() {
                                Some(c) => {
                                    if c == b'"' {
                                        break;
                                    }
                                }
                                None => {
                                    let line = self.line.get();
                                    let col = self.col.get();
                                    let msg = "Unterminated string.".to_string();
                                    return Err(ScannerError{ line, col, msg })
                                }
                            }
                        }
                        Some(String)
                    }
                    // Number
                    else if is_numeric(c) {
                        // Find whole part
                        while is_numeric(self.peek().unwrap_or(b'\0')) {
                            self.advance();
                        }
                        // Optionally, find factional part
                        if self.is_match(b'.') {
                            self.advance();
                            while is_numeric(self.peek().unwrap_or(b'\0')) {
                                self.advance();
                            }
                        }
                        Some(Number)
                    }
                    // Identifiers/keywords
                    else if is_alpha(c) {
                        loop {
                            if self.peek().is_some() && !is_alpha_numeric(self.peek().unwrap()) {
                                break;
                            }
                            else {
                                self.advance();
                            }
                        }
                        let lexeme = std::str::from_utf8(&self.program[start..self.cur.get()]).unwrap();
                        if let Some(t) = keywords.get(lexeme) {
                            Some(t.clone())
                        } else {
                            Some(Identifer)
                        }
                    }
                    else {
                        let line = self.line.get();
                        let col = self.col.get();
                        let msg = format!("Unexpected character '{}'.", c as char);
                        return Err(ScannerError { line, col, msg });
                    }
                }
            },
        };
        if let Some(t) = token_type {
           Ok(self.gen_token(start, t))
        } else {
            self.get_next_token()
        }
    }
    pub fn scan_tokens(& self) -> Result<Vec<Token>, ScannerError> {
        let mut tokens = Vec::<Token>::new();

        loop {
            let next_token = self.get_next_token()?;
            let is_eof = match next_token.t { TokenType::EOF => true, _ => false };
            tokens.push(next_token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }
}

