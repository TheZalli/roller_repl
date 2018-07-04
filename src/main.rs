///! Roller REPL implementation.
///!
///! To see what is and what is not implemented, check the readme.

#[cfg(unix)] extern crate libc;
#[macro_use] extern crate lazy_static;
extern crate rustyline;
extern crate num;
extern crate regex;
#[macro_use] extern crate clap;

// TODO: rework the module files when the new module RFC lands
mod error;
mod value;
mod op;
mod ast;
mod lexer;
mod parser;
mod eval;
mod fmt;

use std::io::{Read, BufRead, BufReader, Lines, stdin, stdout, stderr};
use std::fs::File;

use rustyline::error::ReadlineError;
use clap::{Arg, App};

use lexer::{Lexer, LexerState};
use eval::Env;

fn main() {
    let clap_matches =
        App::new(crate_name!())
            .about("Roller script REPL")
            .arg(Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("Enables debug prints"))
            .arg(Arg::with_name("input file")
                .short("i")
                .long("input")
                .value_name("FILE PATHS")
                .help("Evaluates the input files in the given order instead of reading from stdin"))
            .arg(Arg::with_name("output file")
                .short("o")
                .long("output")
                .value_name("FILE PATH")
                .max_values(1)
                .help("Prints the output of evaluation into the given file"))
            .get_matches();

    let mut debug_mode = clap_matches.is_present("debug");
    let use_input_file = clap_matches.is_present("input file");
    let _use_output_file = clap_matches.is_present("output file");

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

    // opens the given file or on error exits the process with the status code 1
    macro_rules! open_file_or_exit {
        ($filepath: expr, $files_purpose: expr) => {
            match File::open($filepath) {
                Ok(file) => file,
                Err(e) => {
                    println!("Error opening {} file: {}", $files_purpose, e);
                    ::std::process::exit(1)
                  }
            }
        };
    }

    // handles pipe or file input, since reading with rustyline outputs to stdout and dirties it
    let mut pipe_reader: Lines<BufReader<Box<Read>>> =
        BufReader::new(
            if use_input_file {
                let mut path_iter = clap_matches.values_of("input file").unwrap();

                let file = open_file_or_exit!(path_iter.next().unwrap(), "input");
                let mut file = Box::new(file) as Box<Read>;

                // chain all the other input files after the first
                for filepath in path_iter {
                    file = Box::new(file.chain(open_file_or_exit!(filepath, "input")))
                }

                file
            } else {
                Box::new(stdin()) as Box<Read>
            }
        ).lines();

    if in_isatty && out_isatty && !use_input_file {
        println!("Interactive Roller REPL started");
        if debug_mode {
            println!("(Debug prints are active)");
        }
    }

    // the lexer
    let lexer = Lexer::default();

    // the evaluation environment holding the runtime data like variables
    let mut env = Env::new(Box::new(stdout()), Box::new(stderr()));

    // a helper token holder
    let mut temp_tokens = Vec::new();
    // the continuation flag
    let mut continuing = false;
    // the lexer state, for storing data between continuations
    let mut lexer_state = LexerState::repl_default();

    // return value
    let return_status = loop {
        // read a line
        let line_res = if out_isatty && !use_input_file {
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
                if out_isatty {
                    rl.add_history_entry(&input.trim_right());
                } else {
                    // print prompt and input into pipe output
                    // we might want to give a flag to supress this
                    println!("{}{}", prompt, input);
                }

                // by-default do not continue
                continuing = false;

                // check interpreter commands
                if input.trim_left().starts_with('#') {
                    let mut word_iter = input[1..].split_whitespace();
                    match word_iter.next() {
                        Some("debug") => match word_iter.next() {
                            None => eprintln!("Debug prints are {}",
                                             if debug_mode { "on" } else { "off" }),

                            Some(s) => if let Some(b) = parse_bool(s) {
                                debug_mode = b;
                                eprintln!("Debug prints are now {}",
                                         if debug_mode { "on" } else { "off" })

                            } else {
                                eprintln!("Invalid value for debug command");
                            }
                        }
                        Some(cmd) => eprintln!("Invalid command `{}`", cmd),
                        None => eprintln!("Invalid empty command")
                    }
                    continue;
                }

                match lexer.parse(&input, lexer_state) {
                    // continue to the next iteration
                    Ok((lexed, Some(new_state))) => {
                        temp_tokens.extend(lexed);
                        continuing = true;
                        lexer_state = new_state;
                    },
                    Ok((lexed, None)) => {
                        temp_tokens.extend(lexed);

                        // steal the temporary tokens
                        let mut tokens = Vec::new();
                        ::std::mem::swap(&mut tokens, &mut temp_tokens);

                        if debug_mode {
                            println!("Lexed: {:?}\n", tokens);
                        }

                        // empty token streams are not valid
                        if tokens.len() <= 1 {
                            // has only the end token
                            continue;
                        }

                        // strip the location data
                        let tokens = tokens.into_iter().map(|(_, x)| x);
                        let parse_res = parser::parse_line(tokens);

                        if debug_mode {
                            println!("Parsed: {:#?}\n", parse_res);
                        }

                        match parse_res {
                            Ok(exp) => println!("{}", env.eval_print(&exp)),
                            Err(e) => println!("Parse error: {:#?}", e),
                        }
                    },
                    Err(e) => println!("Lexing error: {:#?}", e),
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
                if in_isatty && !use_input_file {
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
            lexer_state = LexerState::repl_default();
        }
    };

    if in_isatty && !use_input_file {
        println!("Exiting");
    }

    // this is a command-line application so we want to return the status number
    ::std::process::exit(return_status);
}

fn parse_bool<'a>(input: &'a str) -> Option<bool> {
    match input.to_ascii_lowercase().as_str() {
        "1"|"t"|"true"|"y"|"yes"|"on" => Some(true),
        "0"|"f"|"false"|"n"|"no"|"off" => Some(false),
        _ => None
    }
}
