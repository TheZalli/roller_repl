extern crate libc;
extern crate rustyline;

use rustyline::error::ReadlineError;

fn main() {
    let is_in_tty = unsafe { libc::isatty(libc::STDIN_FILENO as i32) } != 0;
    let is_out_tty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;

    let mut rl = rustyline::Editor::<()>::new().history_ignore_space(true);
    let prompt = "> "; //if is_in_tty { "> " } else { "" };

    if is_in_tty && is_out_tty {
        println!("Interactive REPL started");
    }

    loop {
        // read a line
        let line = rl.readline(prompt);
        match line {
            Ok(ref s) if s.trim() == "" => continue, // empty input
            Ok(s) => println!("Hello, {}!", s),
            // TODO: maybe check ReadlineError::WindowResize when on windows
            Err(ReadlineError::Interrupted) => {
                // received interrupt (ctrl + C)
                println!("Interrupt received, exiting");
                break;
            },
            Err(ReadlineError::Eof) => {
                // received EOF (ctrl + D, or end of pipe input)
                println!("End-of-line received, exiting");
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
}
