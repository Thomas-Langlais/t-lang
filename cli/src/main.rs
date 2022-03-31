use std::io::{self, BufRead, Write};

// this is a form of inport in JS
use core::lexer::Lexer;
use core::parser::Parser;
use core::interpreter::Execute;

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


        let mut parser = Parser::new(tokens);
        let ast_result = parser.generate_syntax_tree();
        
        if let Err(err) = ast_result {
            print!("{}\n", err);
            continue;
        }

        let ast = unsafe { ast_result.unwrap_unchecked() };
        match ast.execute() {
            Ok(inter_type) => {
                print!("= {}\n", inter_type);
            }
            Err(err) => {
                print!("{}\n", err);
            }
        }
    }
}
