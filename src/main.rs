use std::io::{self, BufRead, Write};

// this is a form of inport in JS
mod lexer;
mod parser;

use lexer::Lexer;
use parser::{Parser, Evaluate};

fn main() {
    print!("T-Lang Console\n");
    print!("--------------------------------------\n");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        // can only handle one line at a time right now
        let ref mut buffer: Vec<u8> = Vec::new();

        handle.read_until(b'\n', buffer).unwrap();

        // set the io handle for reading the stream
        let mut reader = Lexer::new(&buffer);
        let tokens = reader.parse_tokens();

        print!("= ");

        let parser = Parser::new(tokens);

        match parser.generate_syntax_tree() {
            Ok(ast) => {
                print!("{}", ast.evaluate());
            }
            Err(msg) => {
                print!("{}", msg);
            }
        }
        print!("\n");
        io::stdout().flush().unwrap();
    }
}
