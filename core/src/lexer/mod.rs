use std::iter::Peekable;

use std::io;

mod reader;
mod rules;
mod strings;

// statics
static KEYWORDS: &[&str] = &["let", "if", "else", "for", "while", "brk", "con"];

// Tokens structures
#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenType,
    pub source: Source,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LogicType {
    NOT,
    AND,
    OR,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CompType {
    EE,
    NE,
    LT,
    GT,
    LTE,
    GTE,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperationTokenType {
    EQ,
    Arithmetic(char),
    Logic(LogicType),
    Comparison(CompType),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Keyword(&'static str),
    Identifier(String),
    LParen(char),
    RParen(char),
    LBlock,
    RBlock,
    Operation(OperationTokenType),
    Int(i64),
    Float(f64),
    LineTerm,
    // These two variants signal that something went wrong
    // and the parser should handle them with care
    EOF,
    BadRead(&'static str, char),
    BadParse(&'static str, Source),
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Source {
    pub start: Position,
    pub end: Position,
}

impl Source {
    pub fn new(start: Position, end: Position) -> Self {
        Source { start, end }
    }

    pub fn new_single(pos: Position) -> Self {
        Source {
            start: pos,
            end: pos,
        }
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct Position {
    pub index: usize,
    pub column: usize,
    pub line: usize,
}

impl Position {
    fn advance(&mut self, new_char: char) {
        self.index += 1;
        self.column += 1;

        if new_char == '\n' {
            self.line += 1;
            // offset for the next char
            self.column = 0;
        }
    }
}

// #[derive(Clone)]
// pub struct LexerError {
//     pub name: &'static str,
//     pub details: &'static str,
//     pub source: String,
//     pub position: Position,
// }

// impl fmt::Display for LexerError {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> FormatResult {
//         let line_header = format!("line {line}: ", line = self.position.line);

//         let underline = (1..self.position.column + line_header.len())
//             .map(|_| ' ')
//             .chain((0..1).map(|_| '^'))
//             .collect::<String>();

//         let source = self
//             .source
//             .lines()
//             .enumerate()
//             .skip_while(|(i, _)| i + 1 != self.position.line)
//             .map(|(_, line)| line)
//             .next()
//             .unwrap();

//         write!(
//             f,
//             "{name} - {details}\n\
//             {line_header}{source}\n\
//             {underline}",
//             name = self.name,
//             details = self.details
//         )
//     }
// }

// impl LexerError {
//     pub fn new(name: &str, details: String, position: Position, source: String) -> LexerError {
//         LexerError {
//             name: name.to_string(),
//             details,
//             source,
//             position,
//         }
//     }
// }

trait CharOps {
    fn is_separator(&self) -> bool;

    fn is_space(&self) -> bool;

    fn is_word(&self) -> bool;
}

impl CharOps for char {
    fn is_separator(&self) -> bool {
        match *self {
            '(' | ')' | '=' | '<' | '>' | ';' | '+' | '-' | '*' | '/' => true,
            ch => ch.is_space(),
        }
    }

    fn is_space(&self) -> bool {
        matches!(*self, '\n' | ' ' | '\t' | '\r')
    }

    fn is_word(&self) -> bool {
        match *self {
            '_' => true,
            ch => ch.is_alphanumeric(),
        }
    }
}

/* lifetimes are important here because we need to define
that the scope of the readable is where the methods are being called. */
pub struct Lexer<'a> {
    input: Peekable<reader::CharReader<'a>>,
    reading: bool,
    read_buffer: Vec<char>,
    src: Position,
}

impl<'a> From<&'a mut dyn io::Read> for Lexer<'a> {
    fn from(reader: &'a mut dyn io::Read) -> Self {
        Lexer {
            input: reader::CharReader::from(reader).peekable(),
            reading: false,
            read_buffer: vec![],
            src: Position {
                index: 0,
                line: 1,
                column: 1,
            },
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = io::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read())
    }
}

impl<'a> Lexer<'a> {
    fn handle_bad_read(&mut self, msg: &'static str, ch: char) -> io::Result<Token> {
        let pos = self.src;
        loop {
            match self.input.peek() {
                Some(Ok(ch)) if ch.is_separator() => break,
                Some(Ok(_)) => {
                    self.advance().unwrap()?;
                }
                Some(Err(_)) => return Err(self.advance().unwrap().unwrap_err()),
                None => break,
            }
        }
        Ok(Token {
            value: TokenType::BadRead(msg, ch),
            source: Source::new_single(pos),
        })
    }

    fn handle_bad_peek(&mut self, msg: &'static str) -> io::Result<Token> {
        let ch = self.advance().unwrap().unwrap();
        self.handle_bad_read(msg, ch)
    }

    fn advance_to_next(&mut self) -> io::Result<Option<char>> {
        loop {
            match self.advance() {
                Some(Ok(ch)) if ch.is_space() => (),
                Some(Ok(ch)) => return Ok(Some(ch)),
                Some(Err(e)) => return Err(e),
                None => return Ok(None),
            }
        }
    }

    fn advance(&mut self) -> Option<io::Result<char>> {
        let reading = self.reading;
        match self.input.peek() {
            Some(Ok(_)) => {
                let c = self.input.next().unwrap().unwrap();
                if reading {
                    self.src.advance(c);
                } else {
                    self.reading = true;
                }
                self.read_buffer.push(c);
                Some(Ok(c))
            }
            Some(Err(_)) => {
                let e = self.input.next().unwrap().unwrap_err();
                Some(Err(e))
            }
            None => None,
        }
    }

    pub fn read(&mut self) -> io::Result<Token> {
        let result = self.advance_to_next()?;
        if result.is_none() {
            return Ok(Token {
                value: TokenType::EOF,
                source: Source::new_single(self.src),
            });
        }
        let ch = result.unwrap();

        // check the contents of the context
        match ch {
            'a'..='z' | 'A'..='Z' => self.parse_identifier(ch, self.src),
            '0'..='9' => self.parse_number(ch, self.src),
            '-' => self.parse_minus(self.src),
            '=' => self.parse_equal(self.src),
            '!' => self.parse_not(self.src),
            '&' => self.parse_and(self.src),
            '|' => self.parse_or(self.src),
            '<' => self.parse_lesser(self.src),
            '>' => self.parse_greater(self.src),
            op @ ('+' | '*' | '/') => {
                // create the token
                let token = Token {
                    value: TokenType::Operation(OperationTokenType::Arithmetic(op)),
                    source: Source::new_single(self.src),
                };
                Ok(token)
            }
            '(' => {
                let token = Token {
                    value: TokenType::LParen('('),
                    source: Source::new_single(self.src),
                };
                Ok(token)
            }
            ')' => {
                // create the token
                let token = Token {
                    value: TokenType::RParen(')'),
                    source: Source::new_single(self.src),
                };
                Ok(token)
            }
            ';' => {
                let token = Token {
                    value: TokenType::LineTerm,
                    source: Source::new_single(self.src),
                };
                Ok(token)
            }
            ch => self.handle_bad_read("Unknown character", ch),
        }
    }

    // fn peekable(self) ->
}

// pub struct PeekableLexer<'a> {
//     lexer: Lexer<'a>,
//     peeked:
// }
