#[cfg(unix)]
extern crate libc;
#[macro_use]
extern crate lazy_static;

extern crate rustyline;
extern crate num;
extern crate regex;

mod error;
mod value;
mod op;
mod ast;
mod lexer;
mod parser;
mod eval;

use std::io;
use std::io::BufRead;

use rustyline::error::ReadlineError;

use lexer::Lexer;
use parser::expr;
use eval::Env;

fn main() {
    ::std::process::exit(real_main());
}

fn real_main() -> i32 {
    let in_isatty;
    #[cfg(unix)] {
        in_isatty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    }
    #[cfg(not(unix))] {
        in_isatty = true;
    }

    let out_isatty;
    #[cfg(unix)] {
        out_isatty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    }
    #[cfg(not(unix))] {
        out_isatty = true;
    };

    let prompt = "» ";

    // rustyline input & output
    let mut rl = rustyline::Editor::<()>::new();

    // pipe input, since reading rustyline changes stdout
    let mut pipe_reader = io::BufReader::new(io::stdin()).lines();

    if in_isatty && out_isatty {
        println!("Interactive Roller REPL started");
    }

    // the lexer
    let lexer = Lexer::default();

    // the evaluation environment holding the runtime data
    let mut env = Env::new();

    // return value
    let return_status = loop {
        // read a line
        let line_res = if out_isatty {
            rl.readline(prompt)
        } else {
            pipe_reader.next()
                .map_or(
                    Err(ReadlineError::Eof),
                    |res| res.map_err(&ReadlineError::from),
                )
        };

        match line_res {
            Ok(ref input) if input.trim().is_empty() => continue, // empty input

            Ok(input) => {
                // ok input
                if out_isatty {
                    rl.add_history_entry(&input.trim_right());
                } else {
                    // print prompt and input into pipe output
                    // we might want to give a flag to supress this
                    println!("{}{}", prompt, input);
                }
                match lexer.parse(&input) {
                    Ok(lexed) => {
                        println!("Lexed: {:?}\n", lexed);

                        let tokens = lexed.into_iter().map(|(_, x, _)| x);
                        let parse_res = expr::parse_expr(tokens);
                        println!("Parsed: {:?}\n", parse_res);

                        if let Ok(exp) = parse_res {
                            println!("{}", env.eval_print(&exp));
                        }
                    },
                    Err(e) => println!("Lexing error: {:?}", e),
                }
            },

            // TODO: maybe check ReadlineError::WindowResize when on windows
            // doesn't seem to affect anything though?

            Err(ReadlineError::Interrupted) => {
                // received interrupt (ctrl+C)
                eprintln!("Interrupt received");
                break 0;
            },

            Err(ReadlineError::Eof) => {
                // received EOF (ctrl+D, or end of pipe input)
                if in_isatty {
                    eprintln!("End-of-file received");
                }
                break 0;
            },

            Err(e) => {
                // other error, maybe IO error
                // in some cases we might want to continue, but we don't want 
                // any infinite loops
                eprintln!("Encountered error: {:?}", e);
                break 1;
            },
        }
    };
    
    if in_isatty {
        println!("Exiting");
    }

    return_status
}
