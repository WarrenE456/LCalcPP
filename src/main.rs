// TODO: negation op..
pub mod scanner;
pub mod lcalc;
pub mod parser;
pub mod expr;
pub mod runtime;
pub mod builtin;

use lcalc::Lcalc;

use std::env;
use std::fs::read_to_string;
use std::process::ExitCode;


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

        let lcalc = Lcalc::new();

        return ExitCode::from(lcalc.run_file(&program));
    }
    else if args.len() == 1 {
        let lcalc = Lcalc::new();
        lcalc.run_prompt();
        // Unreachable due to infinite loop in Lcalc::run_prompt
        return ExitCode::from(0);
    }
    else {

        println!("Usage: ./lcalc [file_path]");
        
        return ExitCode::from(1)
    };
}
