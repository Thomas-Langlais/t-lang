use std::io;
use std::iter::Peekable;

use crate::reader;

// statics
static KEYWORDS: &[&str] = &[
    "let", "if", "else", "fn", "for", "while", "brk", "con", "ret",
];

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
    Comma,
    LineTerm,
    // These two variants signal that something went wrong
    // and the parser should handle them with care
    EOF,
    Bad(&'static str, Source),
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

pub(crate) trait CharOps {
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
pub(crate) struct Lexer<'a> {
    pub(crate) input: Peekable<reader::CharReader<'a>>,
    pub(crate) reading: bool,
    pub(crate) src: Position,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = io::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read())
    }
}

impl<'a> From<&'a mut dyn io::Read> for Lexer<'a> {
    fn from(reader: &'a mut dyn io::Read) -> Self {
        Lexer {
            input: reader::CharReader::from(reader).peekable(),
            reading: false,
            src: Position {
                index: 0,
                line: 1,
                column: 1,
            },
        }
    }
}

impl<'a> Lexer<'a> {
    fn handle_bad_read(&mut self, msg: &'static str, pos: Position) -> io::Result<Token> {
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
            value: TokenType::Bad(msg, Source::new(pos, self.src)),
            source: Source::new(pos, self.src),
        })
    }

    fn handle_bad_peek(&mut self, msg: &'static str, pos: Position) -> io::Result<Token> {
        self.advance().unwrap().unwrap();
        self.handle_bad_read(msg, pos)
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
                Some(Ok(c))
            }
            Some(Err(_)) => {
                let e = self.input.next().unwrap().unwrap_err();
                Some(Err(e))
            }
            None => None,
        }
    }

    pub(crate) fn read(&mut self) -> io::Result<Token> {
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
            ',' => {
                let token = Token {
                    value: TokenType::Comma,
                    source: Source::new_single(self.src),
                };
                Ok(token)
            }
            _ => self.handle_bad_read("Unknown character", self.src),
        }
    }

    fn parse_identifier(&mut self, first: char, starting_position: Position) -> io::Result<Token> {
        let mut identifier = String::from(first);

        loop {
            match self.input.peek() {
                Some(Ok(ch)) if ch.is_word() => identifier.push(self.advance().unwrap()?),
                Some(Ok(ch)) if ch.is_separator() => break,
                Some(Ok(_)) => {
                    return self.handle_bad_peek(
                        "Unexpected character in identifier/keyword",
                        starting_position,
                    );
                }
                Some(Err(_)) => return Err(self.advance().unwrap().unwrap_err()),
                None => break,
            }
        }

        if let Some(&word) = KEYWORDS.iter().find(|&&word| word == identifier) {
            Ok(Token {
                value: TokenType::Keyword(word),
                source: Source::new(starting_position, self.src),
            })
        } else {
            Ok(Token {
                value: TokenType::Identifier(identifier),
                source: Source::new(starting_position, self.src),
            })
        }
    }

    // generic number parser, it will out put either a int or float
    fn parse_number(&mut self, first: char, starting_position: Position) -> io::Result<Token> {
        let mut s = String::from(first);
        let mut found_dot = false;

        loop {
            match self.input.peek() {
                Some(Ok('.')) => {
                    if found_dot {
                        return self.handle_bad_peek(
                            "Too many dots in numeric literal",
                            starting_position,
                        );
                    }
                    s.push(self.advance().unwrap()?);
                    found_dot = true;
                }
                Some(Ok(ch)) if ch.is_digit(10) => s.push(self.advance().unwrap()?),
                Some(Ok(ch)) if ch.is_separator() => break,
                Some(Ok(_)) => {
                    return self.handle_bad_peek(
                        "Unexpected character in numeric literal",
                        starting_position,
                    );
                }
                Some(Err(_)) => return Err(self.advance().unwrap().unwrap_err()),
                None => break,
            }
        }
        if found_dot {
            if s.ends_with('.') {
                return self.handle_bad_read("Unknown character: .", self.src);
            }
            match s.parse::<f64>() {
                Ok(f) => Ok(Token {
                    value: TokenType::Float(f),
                    source: Source::new(starting_position, self.src),
                }),
                Err(_) => Ok(Token {
                    value: TokenType::Bad(
                        "Float could not be parsed",
                        Source::new(starting_position, self.src),
                    ),
                    source: Source::new(starting_position, self.src),
                }),
            }
        } else {
            match s.parse::<i64>() {
                Ok(i) => Ok(Token {
                    value: TokenType::Int(i),
                    source: Source::new(starting_position, self.src),
                }),
                Err(_) => Ok(Token {
                    value: TokenType::Bad(
                        "Integer could not be parsed",
                        Source::new(starting_position, self.src),
                    ),
                    source: Source::new(starting_position, self.src),
                }),
            }
        }
    }

    fn parse_minus(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('|')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::RBlock,
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => {
                // create the token
                let token = Token {
                    value: TokenType::Operation(OperationTokenType::Arithmetic('-')),
                    source: Source::new_single(start),
                };
                Ok(token)
            }
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_equal(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('=')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::EE)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => Ok(Token {
                value: TokenType::Operation(OperationTokenType::EQ),
                source: Source::new_single(start),
            }),
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_not(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('=')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::NE)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => Ok(Token {
                value: TokenType::Operation(OperationTokenType::Logic(LogicType::NOT)),
                source: Source::new_single(start),
            }),
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_and(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('&')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Logic(LogicType::AND)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => self.handle_bad_peek("Incomplete token error - Expected '&&'", start),
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_or(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('|')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Logic(LogicType::OR)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok('-')) => {
                self.advance();
                let end = self.src;
                Ok(Token {
                    value: TokenType::LBlock,
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => {
                self.handle_bad_peek("Incomplete token error - Expected '||' or '|-'", start)
            }
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_lesser(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('=')) => {
                let end = self.src;
                self.advance();
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::LTE)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => Ok(Token {
                value: TokenType::Operation(OperationTokenType::Comparison(CompType::LT)),
                source: Source::new_single(start),
            }),
            _ => panic!("Should not have been called"),
        }
    }

    fn parse_greater(&mut self, start: Position) -> io::Result<Token> {
        match self.input.peek() {
            Some(Err(_)) => Err(self.advance().unwrap().unwrap_err()),
            Some(Ok('=')) => {
                let end = self.src;
                self.advance();
                Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::GTE)),
                    source: Source::new(start, end),
                })
            }
            Some(Ok(_)) => Ok(Token {
                value: TokenType::Operation(OperationTokenType::Comparison(CompType::GT)),
                source: Source::new_single(start),
            }),
            _ => panic!("Should not have been called"),
        }
    }
}
