use std::io::{Read, Error};
use std::fmt;

// Tokens structures
pub struct Token {
    pub value: TokenType,
    source: Location
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

#[derive(Debug)]
pub enum TokenType {
    LParen(char),
    RParen(char),
    Operation(char),
    Int(u64),
    Float(f64)
}

#[derive(Clone, Copy)]
struct Location {
    start: Position,
    end: Position
}

#[derive(Clone, Copy)]
struct Position {
    column: usize,
    line: usize,
}

// the span must be clonable because we are copying the span
// to the token struct
struct LexerContext {
    result: Option<Result<u8, Error>>,
    position: Position
}

/* lifetimes are important here because we need to define
that the scope of the readable is where the methods are being called. */
pub struct Lexer<'a, R> where R: Read {
    lazy: &'a Fn() -> R,
    input: Option<R>,
    context: LexerContext,
    pub tokens: Vec<Token>,
}

impl<'a, R> Iterator for Lexer<'a, R> where R: Read {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if let None = self.input {
            // create the readable from the iterator call
            self.input = Some((self.lazy)());
            // set the first byte at the init position
            // we know that the unwrap call is safe here because of the initial input check
            self.context.result = self.input.as_mut().unwrap().by_ref().bytes().next();
        }
        // return the token generated from the loop
        // loop and return the next token we make
        return loop {
            // check the contents of the context
            match self.context.result {
                Some(Ok(b'0'..=b'9')) => {
                    // implement here.
                    break self.parse_number(self.context.position);
                }
                Some(Ok(b'+')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('+'),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                Some(Ok(b'-')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('-'),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                Some(Ok(b'*')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('*'),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                Some(Ok(b'/')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::Operation('/'),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                Some(Ok(b'(')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::LParen('('),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                Some(Ok(b')')) => {
                    // create the token
                    let token = Some(Token {
                        value: TokenType::RParen(')'),
                        source: Location {
                            start: self.context.position,
                            end: self.context.position
                        }
                    });
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    break token;
                }
                // this is empty as we don't need to do any parsing on white spaces or tabs
                Some(Ok(b' ')) | Some(Ok(b'\t')) => {
                    // advance the iterator context to the next char
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                }
                Some(Ok(b'\n')) => {
                    // handle a line change for when we hold debug data
                    break None
                }
                _ => {
                    panic!("ERROR");
                    // return Err(format!("unexpected char {}", b))
                }
            }
        }
    }
}

impl<'a, R> Lexer<'a, R> where R: Read<> {
    pub fn new(lazy: &'a Fn() -> R) -> Lexer<'a, R> {
        Lexer {
            lazy,
            input: None,
            context: LexerContext {
                result: None,
                position: Position {
                    column: 0,
                    line: 0
                }
            },
            tokens: vec![]
        }
    }

    pub fn parse_tokens(&mut self) {
        // create the token vector from the input Readable
        while let Some(token) = self.next() {
            self.tokens.push(token);
        }
    }

    // generic number parser, it will out put either a int or float
    fn parse_number(&mut self, starting_position: Position) -> Option<Token> {
        // starting number
        let mut number = 0u64;
        
        loop {
            // go to the next byte
            match self.context.result {
                Some(Ok(b @ b'0'..=b'9')) => {
                    // update the number
                    number = (number * 10) + u64::from(b - b'0');
                    // get the next byte
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                }
                Some(Ok(b'.')) => {
                    // get the next byte
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                    // return the float option
                    return self.parse_float(number, starting_position);
                }
                Some(Ok(b)) => {
                    let token = Token {
                        value: TokenType::Int(number),
                        source: Location {
                            start: starting_position,
                            end: Position {
                                line: self.context.position.line,
                                column: self.context.position.column - 1,
                            }
                        }
                    };
                    // return the token
                    return Some(token);
                }
                Some(Err(_)) => {
                    panic!("Error, parsing the number failed");
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
            match self.context.result {
                Some(Ok(b @ b'0'..=b'9')) => {
                    // update the power
                    power = power * 10f64;
                    // don't calculate if it's zero, as nothing will happen
                    if b == b'0' {
                        continue;
                    }
                    // update the number
                    decimal_point = decimal_point + (f64::from(b - b'0') / power);
                    // get the next byte
                    self.context.advance(self.input.as_mut().unwrap().by_ref().bytes().next());
                }
                Some(Ok(b)) => {
                    let token = Token {
                        // this is a lossy conversion and there will be no stoping from capturing
                        // this error at runtime atm. TODO
                        value: TokenType::Float(f64::from(number as u32) + decimal_point),
                        source: Location {
                            start: starting_position,
                            end: Position {
                                line: self.context.position.line,
                                column: self.context.position.column - 1,
                            }
                        }
                    };
                    // return the token
                    return Some(token);
                }
                Some(Err(_)) => {
                    panic!("Error, parsing the number failed");
                }
                None => {
                    return None;
                }
            }
        }
    }
}

impl LexerContext {
    // update the context by setting the byte and
    // new position
    fn advance(&mut self, result: Option<Result<u8, Error>>) {
        // update the column 
        self.position.column += 1;
        // set the result
        self.result = result;
    }
}