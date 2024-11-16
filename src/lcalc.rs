use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::runtime::{Interpreter, Val};

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
    pub fn gen_error(&self, line: usize, col: usize, msg: String, program: &str) {
        let err_msg = format!(r#"
Error:{}:{}: {}
{}
{}^
        "#, line, col, msg, program.trim_end(), " ".repeat(col - 1));
        self.report_error(err_msg);
    }
    pub fn run_file(&self, program: &str,) -> u8 {
        if let Err(code) = self.run(program) {
            code
        } else {
            0
        }
    }
    pub fn run(&self, program: &str) -> Result<Val, u8> {
        let scanner = Scanner::new(&program);
        let tokens = match scanner.scan_tokens() {
            Ok(tokens) => tokens,
            Err(e) => {
                self.gen_error(e.line, e.col, e.msg, program);
                return Err(1);
            }
        };

        println!("{:?}", tokens);

        let parser = Parser::new(&tokens);
        let expr = match parser.parse() {
            Ok(expr) => expr,
            Err(e) => {
                self.gen_error(e.line, e.col, e.msg, program);
                return Err(1);
            }
        };

        let interpreter = Interpreter::new();

        match interpreter.interpret(&expr) {
            Ok(val) => 
                Ok(val),
            Err(e) => {
                self.gen_error(e.token.line, e.token.col, e.msg, program);
                Err(1)
            }
        }
    }
    pub fn run_prompt(&self) {

        let mut line = String::new();
        loop {
            print!(">  ");
            let _ = stdout().flush();
            stdin().read_line(&mut line).expect("Failed to read line.");
            if let Ok(val) = self.run(&line) {
                // TODO: new-line shit
                println!("({}) {}\n", val.to_type().to_string(), val.to_string());
            }
            line.clear();
        }

    }
}
