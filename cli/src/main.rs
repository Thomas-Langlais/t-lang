use std::io::{self, stdin, stdout, Read, Write};

// this is a form of inport in JS
use core::errors;
use core::exec::Machine;

#[tokio::main]
async fn main() -> io::Result<()> {
    println!("T-Lang Console");
    println!("--------------------------------------");
    println!("To execute the t-lang code, press Ctrl+D.");
    println!();
    println!("If you are running t-lang in windows, unfortunately");
    println!("I do not know the key control to trigger the EOF");
    println!();

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
        let mut machine = Machine::new();
        match machine.exec(input).await {
            Ok(Some(output)) => println!("= {}", output),
            Err(err) => {
                stdout().lock().write_all(&errors::format_err(err, buf))?;
                println!();
            }
            _ => {}
        }
    }

    Ok(())
}
