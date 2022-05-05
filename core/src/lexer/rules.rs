use super::{
    CharOps, CompType, Lexer, LogicType, OperationTokenType, Position, Source, Token, TokenType,
    KEYWORDS,
};

use std::io;

impl<'a> Lexer<'a> {
    pub fn parse_identifier(
        &mut self,
        first: char,
        starting_position: Position,
    ) -> io::Result<Token> {
        let mut identifier = String::from(first);

        loop {
            match self.input.peek() {
                Some(Ok(ch)) if ch.is_word() => identifier.push(self.advance().unwrap()?),
                Some(Ok(ch)) if ch.is_separator() => break,
                Some(Ok(_)) => {
                    return self.handle_bad_peek("Unexpected character in identifier/keyword", starting_position);
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
    pub fn parse_number(&mut self, first: char, starting_position: Position) -> io::Result<Token> {
        let mut s = String::from(first);
        let mut found_dot = false;

        loop {
            match self.input.peek() {
                Some(Ok('.')) => {
                    if found_dot {
                        return self.handle_bad_peek("Too many dots in numeric literal", starting_position);
                    }
                    s.push(self.advance().unwrap()?);
                    found_dot = true;
                }
                Some(Ok(ch)) if ch.is_digit(10) => s.push(self.advance().unwrap()?),
                Some(Ok(ch)) if ch.is_separator() => break,
                Some(Ok(_)) => {
                    return self.handle_bad_peek("Unexpected character in numeric literal", starting_position);
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

    pub fn parse_minus(&mut self, start: Position) -> io::Result<Token> {
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

    pub fn parse_equal(&mut self, start: Position) -> io::Result<Token> {
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

    pub fn parse_not(&mut self, start: Position) -> io::Result<Token> {
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

    pub fn parse_and(&mut self, start: Position) -> io::Result<Token> {
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
            // Err(LexerError::new(
            //     "Incomplete token error",
            //     "Expected '&&'".to_string(),
            //     self.src,
            //     self.buffer.iter().map(|b| *b as char).collect(),
            // ))),
            _ => panic!("Should not have been called"),
        }
    }

    pub fn parse_or(&mut self, start: Position) -> io::Result<Token> {
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
            Some(Ok(_)) => self.handle_bad_peek("Incomplete token error - Expected '||' or '|-'", start),
            // Some(_) => Some(Err(LexerError::new(
            //     "Incomplete token error",
            //     "Expected '||'".to_string(),
            //     self.src,
            //     self.buffer.iter().map(|b| *b as char).collect(),
            // ))),
            _ => panic!("Should not have been called"),
        }
    }

    pub fn parse_lesser(&mut self, start: Position) -> io::Result<Token> {
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

    pub fn parse_greater(&mut self, start: Position) -> io::Result<Token> {
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
