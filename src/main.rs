use std::io;

// this is a form of inport in JS
mod lexer;

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
        let mut reader = lexer::Lexer::new(&io::stdin);
        reader.parse_tokens();
        println!("{:#?}", reader.tokens);
    }
}
