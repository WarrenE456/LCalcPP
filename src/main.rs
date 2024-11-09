pub mod scanner;
pub mod lcalc;

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

        return lcalc.run(program);
    }
    else {

        println!("Usage: ./lcalc [file_path]");
        
        return ExitCode::from(1)
    };
}
