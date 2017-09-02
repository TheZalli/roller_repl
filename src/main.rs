extern crate libc;
extern crate rustyline;

mod parser;

use rustyline::error::ReadlineError;
use parser::expr;

fn main() {
    ::std::process::exit(real_main());
}

fn real_main() -> i32 {
    // return value
    let mut return_status = 1;

    let is_in_tty = unsafe { libc::isatty(libc::STDIN_FILENO as i32) } != 0;
    //let is_out_tty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;

    let mut rl = rustyline::Editor::<()>::new();
    let prompt = "> ";

    if is_in_tty {
        println!("Interactive Roller REPL started");
    }

    loop {
        // read a line
        let line = rl.readline(prompt);
        match line {
            Ok(ref input) if input.trim() == "" => continue, // empty input

            Ok(input) => {
                // ok input
                rl.add_history_entry(&input.trim_right());
                let input = input.trim();
                let parsed_res = expr::parse_Expr(input);

                println!("Result is: {:?}", parsed_res);
            },

            // TODO: maybe check ReadlineError::WindowResize when on windows

            Err(ReadlineError::Interrupted) => {
                // received interrupt (ctrl+C)
                println!("Interrupt received");
                return_status = 0;
                break;
            },

            Err(ReadlineError::Eof) => {
                // received EOF (ctrl+D, or end of pipe input)
                if is_in_tty {
                    println!("End-of-file received");
                }
                return_status = 0;
                break;
            },

            Err(e) => {
                // other error, maybe IO error
                // in some cases we might want to continue, but we don't want 
                // any infinite loops
                println!("Encountered error: {:?}", e);
                break;
            },
        }
    }
    println!("Exiting");

    return_status
}
