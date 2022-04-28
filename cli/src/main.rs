use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::str;

// this is a form of inport in JS
use core::interpreter::{ExecutionContext, Interpret, SymbolEntry, SymbolTable, SymbolValue};
use core::lexer::Lexer;
use core::parser::Parser;

static REPL_EXEC: &str = ".exec\n";

fn main() {
    println!("T-Lang Console");
    println!("--------------------------------------");

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
        let mut bytes_read_offset = 0;

        'read: loop {
            let bytes = handle.read_until(b'\n', &mut buffer).unwrap();
            if bytes <= 1 && buffer.len() == bytes {
                // restart the repl loop when only a newline is in the buffer
                continue 'repl;
            } else if bytes == 1 {
                // allow newline spacing
                continue 'read;
            }

            if bytes >= REPL_EXEC.len()
                && &buffer[(bytes_read_offset + bytes - REPL_EXEC.len())..(buffer.len())]
                    == REPL_EXEC.as_bytes()
            {
                for _ in 0..REPL_EXEC.len() {
                    buffer.pop();
                }
                break 'read;
            }
            if buffer[bytes_read_offset..(buffer.len() - 2)]
                .iter()
                .all(|&b| b == b' ' || b == b'\n')
            {
                // if all whitespaces, continue the read
                continue 'read;
            }

            bytes_read_offset = buffer.len();
            
            if buffer[buffer.len() - 2] == b'\\' {
                buffer.remove(buffer.len() - 2);
                bytes_read_offset -= 1;
                continue 'read;
            }
            if buffer[buffer.len() - 2] != b';' {
                // leave if there was no line termination char
                break 'read;
            }
        }

        // set the io handle for reading the stream
        let mut reader = Lexer::new(&buffer);
        let tokens_result = reader.parse_tokens();

        if let Err(err) = tokens_result {
            println!("{}", err);
            continue;
        }
        let tokens = unsafe { tokens_result.unwrap_unchecked() };

        let mut parser = Parser::new(tokens, buffer.as_slice());
        let ast_result = parser.generate_syntax_tree();

        if let Err(err) = ast_result {
            println!("{}", err);
            continue;
        }

        let ast = unsafe { ast_result.unwrap_unchecked() };
        let mut context = ExecutionContext::new(
            String::from_utf8(buffer.to_vec()).unwrap(),
            &global_symbol_table,
        );
        match ast.interpret(&mut context) {
            Ok(inter_type) => {
                println!("= {}", inter_type);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
