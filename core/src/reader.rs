use std::char;
use std::io::{self, BufReader, BufRead, Read};

enum State {
    Uniniatized,
    Chars(Vec<char>, usize),
    Eof,
    Error(Option<io::Error>),
}

pub struct CharReader<'a> {
    reader: BufReader<&'a mut dyn Read>,
    state: State,
}

impl<'a> From<&'a mut dyn Read> for CharReader<'a> {
    fn from(reader: &'a mut dyn Read) -> Self {
        CharReader {
            reader: io::BufReader::new(reader),
            state: State::Uniniatized,
        }
    }
}

impl<'a> Iterator for CharReader<'a> {
    type Item = io::Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            State::Uniniatized => self.refill_and_next(),
            State::Eof => None,
            State::Chars(chars, last) => {
                if *last == chars.len() {
                    self.refill_and_next()
                } else {
                    let ch = chars[*last];
                    *last += 1;
                    Some(Ok(ch))
                }
            }
            State::Error(e) => match e.take() {
                Some(e) => Some(Err(e)),
                None => Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Invalid state; error already consumed",
                ))),
            },
        }
    }
}

impl<'a> CharReader<'a> {
    fn refill_and_next(&mut self) -> Option<io::Result<char>> {
        self.state = {
            let mut line = String::new();
            match self.reader.read_line(&mut line) {
                Ok(0) => State::Eof,
                Ok(_) => State::Chars(line.chars().collect(), 0),
                Err(e) => State::Error(Some(e)),
            }
        };
        self.next()
    }
}
