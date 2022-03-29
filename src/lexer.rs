use std::fmt;

// Tokens structures
pub struct Token {
    pub value: TokenType,
    pub source: Location
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    LParen(char),
    RParen(char),
    Operation(char),
    Int(u64),
    Float(f64),
    EOF
}

#[derive(Clone, Copy)]
pub struct Location {
    pub start: Position,
    pub end: Position
}

#[derive(Copy, Clone)]
pub struct Position {
    pub column: usize,
    pub line: usize,
}

/* lifetimes are important here because we need to define
that the scope of the readable is where the methods are being called. */
pub struct Lexer<'a> {
    buffer: &'a Vec<u8>,
    pos: usize,
    byte: Option<&'a u8>,
    src: Position
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
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
                    let token = Some(Token {
                        value: TokenType::Operation('+'),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                Some(b'-') => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('-'),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                Some(b'*') => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('*'),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                Some(b'/') => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('/'),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                Some(b'(') => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::LParen('('),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                Some(b')') => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::RParen(')'),
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                    // advance the iterator context to the next char
                    self.advance();
                    break token;
                }
                // this is empty as we don't need to do any parsing on white spaces or tabs
                Some(b' ') | Some(b'\t') => {
                    // advance the iterator context to the next char
                    self.advance();
                }
                Some(b'\n') => {
                    // handle a line change for when we hold debug data
                    break Some(Token {
                        value: TokenType::EOF,
                        source: Location {
                            start: self.src,
                            end: self.src
                        }
                    });
                }
                _ => {
                    panic!("ERROR");
                    // return Err(format!("unexpected char {}", b))
                }
            }
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a Vec<u8>) -> Lexer<'a> {
        Lexer {
            buffer: buf,
            pos: 0,
            byte: buf.get(0),
            src: Position {
                column: 0,
                line: 0
            }
        }
    }

    pub fn parse_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(token) = self.next() {
            let token_type = token.value;
            tokens.push(token);
            if let TokenType::EOF = token_type {
                break;
            }
        }

        tokens
    }
    
    // update the lexer by setting the byte and
    // new position
    fn advance(&mut self) {
        self.pos += 1;
        self.byte = self.buffer.get(self.pos);
        self.src.column += 1;
    }

    // generic number parser, it will out put either a int or float
    fn parse_number(&mut self, starting_position: Position) -> Option<Token> {
        // starting number
        let mut number = 0u64;
        
        loop {
            // go to the next byte
            match self.byte {
                Some(b @ b'0'..=b'9') => {
                    // update the number
                    number = (number * 10) + u64::from(b - b'0');
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
                            }
                        }
                    };
                    // return the token
                    return Some(token);
                }
                None => {
                    return None;
                }
            }
        }
    }

    // parse the number into a float token
    fn parse_float(&mut self, number: u64, starting_position: Position) -> Option<Token> {
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
                            }
                        }
                    };
                    // return the token
                    return Some(token);
                }
                None => {
                    return None;
                }
            }
        }
    }
}