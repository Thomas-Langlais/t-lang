use std::collections::HashMap;
use std::io::{self, BufRead, Write};

// this is a form of inport in JS
use core::interpreter::{ExecutionContext, Interpret, SymbolEntry, SymbolTable, SymbolValue};
use core::lexer::Lexer;
use core::parser::Parser;

fn main() {
    print!("T-Lang Console\n");
    print!("--------------------------------------\n");

    let global_symbol_table = SymbolTable::new(HashMap::from([
        (
            String::from("true"),
            SymbolEntry {
                value: SymbolValue::Bool(true),
                is_constant: true,
            },
        ),
        (
            String::from("false"),
            SymbolEntry {
                value: SymbolValue::Bool(false),
                is_constant: true,
            },
        ),
    ]));

    'repl: loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        // can only handle one line at a time right now
        let mut buffer: Vec<u8> = Vec::new();

        'read: loop {
            let bytes = handle.read_until(b'\n', &mut buffer).unwrap();
            if bytes <= 1 && buffer.len() == bytes {
                // restart the repl loop when only a newline is in the buffer
                continue 'repl;
            } else if bytes == 1 {
                // allow newline spacing
                continue 'read;
            }

            if bytes >= 2 && buffer[buffer.len() - 2] != b';' {
                // leave if there was no line termination char
                break 'read;
            }
        }

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
