use std::collections::HashMap;
use std::io::{self, stdin, stdout, Read, Write};

// this is a form of inport in JS
use core::errors;
use core::exec::{ExecutionContext, Interpret};
use core::parser::Parser;
use core::symbol_table::{SymbolEntry, SymbolTable, SymbolValue};

fn main() -> io::Result<()> {
    println!("T-Lang Console");
    println!("--------------------------------------");
    println!("To execute the t-lang code, press Ctrl+D.");
    println!();
    println!("If you are running t-lang in windows, unfortunately");
    println!("I do not know the key control to trigger the EOF");
    println!();

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

    loop {
        print!("> ");
        stdout().lock().flush()?;
        let stdin = stdin();
        let mut handle = stdin.lock();
        let mut buf = String::new();

        match handle.read_to_string(&mut buf) {
            Ok(_) => (),
            Err(_) => break,
        };

        let input = &mut buf.as_bytes();
        let mut parser = Parser::from(input);

        let ast = match parser.parse_one() {
            Ok(None) => continue,
            Ok(Some(node)) => node,
            Err(err) => {
                stdout()
                    .lock()
                    .write_all(&errors::format_err(err, buf, None))?;
                println!();
                continue;
            }
        };

        let mut context = ExecutionContext::new(&global_symbol_table);
        match ast.interpret(&mut context) {
            Ok(inter_type) => {
                println!("= {}", inter_type);
            }
            Err(err) => {
                stdout()
                    .lock()
                    .write_all(&errors::format_err(err, buf, Some(context.source())))?;
                println!();
            }
        }
    }

    Ok(())
}
