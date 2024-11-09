use crate::scanner::Scanner;

use std::process::ExitCode;

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
    pub fn run(&self, program: String) -> ExitCode {
        let scanner = Scanner::new(&program);
        let tokens = match scanner.scan_tokens() {
            Ok(tokens) => tokens,
            Err(error) => {
                self.gen_error(error.line, error.col, error.msg);
                return ExitCode::from(1);
            }
        };

        println!("{:?}", tokens);

        return ExitCode::from(0)
    }
}
