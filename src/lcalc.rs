use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::runtime::Interpreter;

use std::io::{stdin, stdout, Write};

pub struct Lcalc {
}

impl Lcalc {
    pub fn new() -> Self {
        Lcalc {}
    }
    pub fn report_error(&self, err_msg: String) {
        println!("{}", err_msg);
    }
    pub fn gen_error(&self, line: usize, col: usize, msg: String) {
        let err_msg = format!("Error:{}:{}: {}", line, col, msg);
        self.report_error(err_msg);
    }
    // TODO: seperate out into run_file and run
    pub fn run_file(&self, program: &str) -> u8 {
        let scanner = Scanner::new(&program);
        let tokens = match scanner.scan_tokens() {
            Ok(tokens) => tokens,
            Err(e) => {
                self.gen_error(e.line, e.col, e.msg);
                return 1;
            }
        };

        let parser = Parser::new(&tokens);
        let expr = match parser.parse() {
            Ok(expr) => expr,
            Err(e) => {
                self.gen_error(e.line, e.col, e.msg);
                return 1;
            }
        };

        let interpreter = Interpreter::new();
        let val = match interpreter.interpret(&expr) {
            Ok(val) => val,
            Err(e) => {
                self.gen_error(e.token.line, e.token.col, e.msg);
                return 1;
            }
        };
        println!("\n({}) {}", val.to_type().to_string(), val.to_string());

        return 0;
    }
    pub fn run_prompt(&self) {

        let mut line = String::new();
        loop {
            print!(">  ");
            let _ = stdout().flush();
            stdin().read_line(&mut line).expect("Failed to read line.");
            self.run_file(&line);
            line.clear();
        }

    }
}
