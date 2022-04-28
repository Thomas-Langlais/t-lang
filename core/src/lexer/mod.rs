use std::fmt::{self, Result as FormatResult};

mod rules;
mod strings;

// statics
static KEYWORDS: &[&str] = &["let", "if", "else"];

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
    EOF,
}

#[derive(Default, Debug, Clone, Copy)]
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

    pub fn new_unhandled(start: Position, end: Position) -> Self {
        Source {
            start,
            end: {
                if start.line != end.line {
                    Position {
                        index: end.index - 1,
                        line: start.line,
                        column: end.index - 1 - start.index + start.column,
                    }
                } else {
                    Position {
                        index: end.index - 1,
                        line: end.line,
                        column: end.column - 1,
                    }
                }
            },
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
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

#[derive(Clone)]
pub struct LexerError {
    pub name: String,
    pub details: String,
    pub source: String,
    pub position: Position,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> FormatResult {
        let line_header = format!("line {line}: ", line = self.position.line);

        let underline = (1..self.position.column + line_header.len())
            .map(|_| ' ')
            .chain((0..1).map(|_| '^'))
            .collect::<String>();

        let source = self
            .source
            .lines()
            .enumerate()
            .skip_while(|(i, _)| i + 1 != self.position.line)
            .map(|(_, line)| line)
            .next()
            .unwrap();

        write!(
            f,
            "{name} - {details}\n\
            {line_header}{source}\n\
            {underline}",
            name = self.name,
            details = self.details
        )
    }
}

impl LexerError {
    pub fn new(name: &str, details: String, position: Position, source: String) -> LexerError {
        LexerError {
            name: name.to_string(),
            details,
            source,
            position,
        }
    }
}

/* lifetimes are important here because we need to define
that the scope of the readable is where the methods are being called. */
pub struct Lexer<'a> {
    buffer: &'a [u8],
    pos: usize,
    byte: Option<&'a u8>,
    src: Position,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        // return the token generated from the loop
        // loop and return the next token we make
        return loop {
            // check the contents of the context
            match self.byte {
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => {
                    break self.parse_identifier(self.src);
                }
                Some(b'0'..=b'9') => {
                    // implement here.
                    break self.parse_number(self.src);
                }
                Some(op @ (b'+' | b'*' | b'/')) => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation(OperationTokenType::Arithmetic(*op as char)),
                        source: Source::new_single(self.src),
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'-') => break self.parse_minus(self.src),
                Some(b'(') => {
                    // create the token
                    let token = Token {
                        value: TokenType::LParen('('),
                        source: Source::new_single(self.src),
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b')') => {
                    // create the token
                    let token = Token {
                        value: TokenType::RParen(')'),
                        source: Source::new_single(self.src),
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'=') => {
                    break self.parse_equal(self.src);
                }
                Some(b'!') => {
                    break self.parse_not(self.src);
                }
                Some(b'&') => {
                    break self.parse_and(self.src);
                }
                Some(b'|') => {
                    break self.parse_or(self.src);
                }
                Some(b'<') => {
                    break self.parse_lesser(self.src);
                }
                Some(b'>') => {
                    break self.parse_greater(self.src);
                }
                // this is empty as we don't need to do any parsing on white spaces or tabs
                Some(b' ') | Some(b'\t') | Some(b'\n') => {
                    // advance the iterator context to the next char
                    self.advance();
                }
                Some(b';') => {
                    let token = Token {
                        value: TokenType::LineTerm,
                        source: Source::new_single(self.src),
                    };
                    self.advance();
                    break Some(Ok(token));
                }
                Some(byte) => {
                    break Some(Err(LexerError::new(
                        "Illegal char error",
                        (*byte as char).to_string(),
                        self.src,
                        self.buffer.iter().map(|b| *b as char).collect(),
                    )));
                }
                _ => break None,
            }
        };
    }
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a [u8]) -> Lexer<'a> {
        Lexer {
            buffer: buf,
            pos: 0,
            byte: buf.get(0),
            src: Position {
                index: 0,
                column: 1,
                line: 1,
            },
        }
    }

    pub fn parse_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = vec![];

        while let Some(result_token) = self.next() {
            tokens.push(result_token?);
        }

        tokens.push(Token {
            value: TokenType::EOF,
            source: Source::new_single(self.src),
        });

        Ok(tokens)
    }

    // update the lexer by setting the byte and
    // new position
    fn advance(&mut self) {
        self.pos += 1;
        self.byte = self.buffer.get(self.pos);

        if let Some(&b) = self.byte {
            self.src.advance(b as char)
        }
    }
}
