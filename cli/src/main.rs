use std::collections::HashMap;
use std::io::{self, BufRead, Write};

// this is a form of inport in JS
use core::interpreter::{ExecutionContext, Interpret, SymbolTable};
use core::lexer::Lexer;
use core::parser::Parser;

fn main() {
    print!("T-Lang Console\n");
    print!("--------------------------------------\n");

    let global_symbol_table = SymbolTable::new(HashMap::new());

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
        let tokens_result = reader.parse_tokens();

        if let Err(err) = tokens_result {
            print!("{}\n", err);
            continue;
        }
        let tokens = unsafe { tokens_result.unwrap_unchecked() };

        let mut parser = Parser::new(tokens);
        let ast_result = parser.generate_syntax_tree();

        if let Err(err) = ast_result {
            print!("{}\n", err);
            continue;
        }

        let ast = unsafe { ast_result.unwrap_unchecked() };
        let mut context = ExecutionContext::new(
            String::from_utf8(buffer.to_vec()).unwrap(),
            &global_symbol_table,
        );
        match ast.interpret(&mut context) {
            Ok(inter_type) => {
                print!("= {}\n", inter_type);
            }
            Err(err) => {
                print!("{}\n", err);
            }
        }
    }
}
