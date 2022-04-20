use std::fmt::{self, Result as FormatResult};
use std::string::ToString;

// statics
static KEYWORDS: &[&str] = &["let"];

// Tokens structures
pub struct Token {
    pub value: TokenType,
    pub source: Location,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Identifier(String),
    LParen(char),
    RParen(char),
    Operation(char),
    Int(i64),
    Float(f64),
    EOF,
}

impl ToString for TokenType {
    fn to_string(&self) -> String {
        match self {
            TokenType::Identifier(identifier) => identifier.to_string(),
            TokenType::LParen(lp) => lp.to_string(),
            TokenType::RParen(rp) => rp.to_string(),
            TokenType::Operation(op) => op.to_string(),
            TokenType::Int(int) => int.to_string(),
            TokenType::Float(float) => float.to_string(),
            TokenType::EOF => "\\n".to_string(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Location {
    pub start: Position,
    pub end: Position,
}

#[derive(Copy, Clone)]
pub struct Position {
    pub column: usize,
    pub line: usize,
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
        let line_header = format!("line {line}: ", line = self.position.line + 1);

        let underline = (0..self.position.column + line_header.len())
            .map(|_| ' ')
            .chain((0..1).map(|_| '^'))
            .collect::<String>();

        let source = &self.source;

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
    buffer: &'a Vec<u8>,
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
                Some(b'0'..=b'9') => {
                    // implement here.
                    break self.parse_number(self.src);
                }
                Some(b'+') => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation('+'),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'-') => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation('-'),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'*') => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation('*'),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'/') => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation('/'),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b'(') => {
                    // create the token
                    let token = Token {
                        value: TokenType::LParen('('),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                Some(b')') => {
                    // create the token
                    let token = Token {
                        value: TokenType::RParen(')'),
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
                // this is empty as we don't need to do any parsing on white spaces or tabs
                Some(b' ') | Some(b'\t') => {
                    // advance the iterator context to the next char
                    self.advance();
                }
                Some(b'\n') => {
                    // handle a line change for when we hold debug data
                    break Some(Ok(Token {
                        value: TokenType::EOF,
                        source: Location {
                            start: self.src,
                            end: self.src,
                        },
                    }));
                }
                Some(byte) => {
                    break Some(Err(LexerError::new(
                        "Illegal char error",
                        (*byte as char).to_string(),
                        self.src,
                        self.buffer
                            .iter()
                            .map(|b| *b as char)
                            .take_while(|c| *c != '\n')
                            .collect(),
                    )));
                }
                _ => break None,
            }
        };
    }
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a Vec<u8>) -> Lexer<'a> {
        Lexer {
            buffer: buf,
            pos: 0,
            byte: buf.get(0),
            src: Position { column: 0, line: 0 },
        }
    }

    pub fn parse_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = vec![];

        while let Some(result_token) = self.next() {
            match result_token {
                Ok(token) => {
                    tokens.push(token);
                    if tokens.last().unwrap().value == TokenType::EOF {
                        break;
                    }
                }
                Err(err) => return Err(err),
            }
        }

        Ok(tokens)
    }

    // update the lexer by setting the byte and
    // new position
    fn advance(&mut self) {
        self.pos += 1;
        self.byte = self.buffer.get(self.pos);
        self.src.column += 1;
    }

    // generic number parser, it will out put either a int or float
    fn parse_number(&mut self, starting_position: Position) -> Option<Result<Token, LexerError>> {
        // starting number
        let mut number = 0i64;

        loop {
            // go to the next byte
            match self.byte {
                Some(b @ b'0'..=b'9') => {
                    // update the number
                    number = (number * 10) + i64::from(b - b'0');
                    // get the next byte
                    self.advance();
                }
                Some(b'.') => {
                    // get the next byte
                    self.advance();
                    // return the float option
                    return self.parse_float(number, starting_position);
                }
                Some(_) => {
                    let token = Token {
                        value: TokenType::Int(number),
                        source: Location {
                            start: starting_position,
                            end: Position {
                                line: self.src.line,
                                column: self.src.column - 1,
                            },
                        },
                    };
                    // return the token
                    return Some(Ok(token));
                }
                None => {
                    return None;
                }
            }
        }
    }

    // parse the number into a float token
    fn parse_float(
        &mut self,
        number: i64,
        starting_position: Position,
    ) -> Option<Result<Token, LexerError>> {
        // the decimal point
        let mut decimal_point: f64 = 0.0;
        // starting point for the power, it updates t o10 to react to the first decimal point digit
        let mut power: f64 = 1.0;

        loop {
            match self.byte {
                Some(b @ b'0'..=b'9') => {
                    // update the power
                    power = power * 10f64;
                    // don't calculate if it's zero, as nothing will happen
                    if *b == b'0' {
                        continue;
                    }
                    // update the number
                    decimal_point = decimal_point + (f64::from(b - b'0') / power);
                    // get the next byte
                    self.advance();
                }
                Some(_) => {
                    let token = Token {
                        // this is a lossy conversion and there will be no stoping from capturing
                        // this error at runtime atm. TODO
                        value: TokenType::Float(f64::from(number as u32) + decimal_point),
                        source: Location {
                            start: starting_position,
                            end: Position {
                                line: self.src.line,
                                column: self.src.column - 1,
                            },
                        },
                    };
                    // return the token
                    return Some(Ok(token));
                }
                None => {
                    return None;
                }
            }
        }
    }
}
