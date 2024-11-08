use std::env;
use std::fs::read_to_string;
use std::process::ExitCode;
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum TokenType {
    // One character
    Plus,   Star,   Slash,  Dot,
    Equal,  Colon,  LParen, RParen,

    // One or two chararacter
    Minus,  Arrow,

    // Variable characters or keyword
    String, Number, In,
    Lambda, Identifer,

    // Misc.
    EOF
}


#[derive(Debug)]
struct Token<'a> {
    t: TokenType,
    lexeme: &'a str,
    line: usize,
    col: usize,
} 

struct Scanner<'a> {
    program: &'a[u8],
    cur: Cell<usize>,
    line: Cell<usize>,
    col: Cell<usize>,
}

fn is_numeric(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_alpha(c: u8) -> bool {
    (b'a' <= c && c <= b'z') || (b'A' <= c && c <= b'Z')
}

fn is_alpha_numeric(c: u8) -> bool {
    is_alpha(c) || is_numeric(c)
}


impl<'a> Scanner<'a> {
    pub fn new(program: &'a str) -> Self {
        let program = program.as_bytes();
        Scanner { program, cur: Cell::new(0), line: Cell::new(0), col: Cell::new(0) } 
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
        let lexeme = std::str::from_utf8(&self.program[start..self.cur.get()]).unwrap();
        Token { t, lexeme, col: self.col.get(), line: self.line.get() }
    }
    fn get_next_token(&self) -> Token {
        use TokenType::*;
        let start = self.cur.get();
        let token_type = match self.advance() {
            None => Some(EOF),
            Some(c) => match c {
                // Sinlge characters
                b'+' => Some(Plus),
                b'*' => Some(Star),
                b'/' => Some(Slash),
                b'L' => Some(Lambda),
                b'.' => Some(Dot),
                b'=' => Some(Equal),
                b':' => Some(Colon),
                b'(' => Some(LParen),
                b')' => Some(RParen),

                // One or two characters
                b'-' => Some(if self.is_match(b'>') { self.advance(); Arrow} else { Minus }),

                // Identifiers and keywords

                // Whitespace
                b'\n' => { self.line.set(self.line.get() + 1); self.col.set(1); None }
                b' ' | b'\t' | b'\r' => None,

                _ => {
                    // Hashmap of keywords and their type
                    let keywords: HashMap<&str, TokenType> = HashMap::from([
                        ("in", TokenType::In),
                        ("L", TokenType::Lambda),
                    ]);

                    // String
                    if self.is_match(b'"') {
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
                                    panic!("Unterminated string.");
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
                        panic!("Unexpected character '{}'.", c as char);
                    }
                }
            },
        };
        if let Some(t) = token_type {
            self.gen_token(start, t)
        } else {
            self.get_next_token()
        }
    }
    pub fn scan_tokens(& self) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();

        loop {
            let next_token = self.get_next_token();
            let is_eof = match next_token.t { TokenType::EOF => true, _ => false };
            tokens.push(next_token);
            if is_eof {
                break;
            }
        }

        tokens
    }
}

fn run_program(program: &str) {
    let scanner = Scanner::new(program);
    let tokens = scanner.scan_tokens();
    println!("{:?}", tokens);
}

fn main() -> ExitCode {

    let args = env::args().collect::<Vec<String>>();

    if args.len() == 2 {

        let file_path = args[1].clone();

        let program =
        match read_to_string(&file_path).map_err(|_| {
            println!("Could not read file '{}.'", file_path);
            ExitCode::from(0)
        }) {
            Err(exit_code) => return exit_code,
            Ok(program) => program,
        };

        run_program(&program);

        return ExitCode::from(0)
    }
    else {

        println!("Usage: ./lcalc [file_path]");
        
        return ExitCode::from(1)
    };
}
