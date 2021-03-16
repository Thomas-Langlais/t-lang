use std::io::{self, Read};

fn main() {
    for byte in io::stdin().lock().bytes() {
        let c = byte.unwrap() as char;
        print!("{}", c)
    }
}