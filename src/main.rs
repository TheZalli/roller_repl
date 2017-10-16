///! Roller REPL implementation.
///! 
///! Works as a reference implementation.
///! To see what is and what is not implemented, check the readme.

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

use lexer::{Lexer, LexerState};
use parser::expr;
use eval::Env;

fn main() {
    // this is a command-line application so we want to return the status number
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
    let prompt_continue = "› ";

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

    // a helper token holder
    let mut temp_tokens = Vec::new();
    // the continuation flag
    let mut continuing = false;
    // the lexer state, for storing data between continuations
    let mut state = LexerState::repl_default();
    
    // return value
    let return_status = loop {
        // read a line
        let line_res = if out_isatty {
            rl.readline(
                if continuing { prompt_continue } else { prompt }
            )
        } else {
            pipe_reader.next()
                .map_or(
                    Err(ReadlineError::Eof),
                    |res| res.map_err(&ReadlineError::from),
                )
        };

        match line_res {
            Ok(input) => {
                // ok input
                if out_isatty {
                    rl.add_history_entry(&input.trim_right());
                } else {
                    // print prompt and input into pipe output
                    // we might want to give a flag to supress this
                    println!("{}{}", prompt, input);
                }

                // by-default do not continue
                continuing = false;

                match lexer.parse(&input, state) {
                    // continue to the next iteration
                    Ok((lexed, Some(new_state))) => {
                        temp_tokens.extend(lexed);
                        continuing = true;
                        state = new_state;
                    },
                    Ok((lexed, None)) => {
                        temp_tokens.extend(lexed);

                        // steal the temporary tokens
                        let mut tokens = Vec::new();
                        ::std::mem::swap(&mut tokens, &mut temp_tokens);

                        println!("Lexed: {:?}\n", tokens);

                        // empty token streams are not valid
                        if tokens.len() <= 1 {
                            // has only the end token
                            continue;
                        }

                        // strip the location data
                        let tokens = tokens.into_iter().map(|(_, x)| x);
                        let parse_res = expr::parse_line(tokens);
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

        // reset the state if not continuing
        if !continuing {
            state = LexerState::repl_default();
        }
    }; // end of the infinite loop
    
    if in_isatty {
        println!("Exiting");
    }

    return_status
}
