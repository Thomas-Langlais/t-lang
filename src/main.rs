use std::io;

// this is a form of inport in JS
mod lexer;
mod parser;

use lexer::{Lexer};
use parser::{Parser};

// struct SingleLineReader<R> where R: Read {
//     inner: R
// }

// impl<R> Read for SingleLineReader<R> where R: Read {

// }

fn main() {
    println!("T-Lang Console");
    println!("--------------------------------------");

    loop {
        print!("> ");
        // set the io handle for reading the stream
        let mut reader = Lexer::new(&io::stdin);
        reader.parse_tokens();

        let mut parser = Parser::new(&reader.tokens);

        let root = parser.generate_syntax_tree();

        println!("{:#?}", root);
    }
}
