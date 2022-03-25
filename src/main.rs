use std::io::{self, BufRead, Write};

// this is a form of inport in JS
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

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

        let parser = Parser::new(tokens);
        let root = parser.generate_syntax_tree();

        print!("= ");
        if let Ok(Some(node)) = &root {
            print!("{:#?}\n", node);
        }
        if let Err(msg) = &root {
            print!("{}\n", msg);
        }
        io::stdout().flush().unwrap();
    }
}
