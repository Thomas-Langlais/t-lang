use super::{
    CompType, Lexer, LexerError, LogicType, OperationTokenType, Position, Source, Token, TokenType,
    KEYWORDS,
};

impl<'a> Lexer<'a> {
    pub fn parse_identifier(
        &mut self,
        starting_position: Position,
    ) -> Option<Result<Token, LexerError>> {
        let mut identifier = vec![*self.byte.unwrap()];
        self.advance();

        while let Some(byte) = self.byte {
            match byte {
                (b'a'..=b'z') | (b'A'..=b'Z') | b'_' => {
                    identifier.push(*byte);
                    self.advance();
                }
                _ => {
                    return Some(Ok({
                        if let Some(word) =
                            KEYWORDS.iter().find(|word| *word.as_bytes() == identifier)
                        {
                            Token {
                                value: TokenType::Keyword(word),
                                source: Source::new_unhandled(starting_position, self.src),
                            }
                        } else {
                            Token {
                                value: TokenType::Identifier(
                                    String::from_utf8(identifier).unwrap(),
                                ),
                                source: Source::new_unhandled(starting_position, self.src),
                            }
                        }
                    }));
                }
            }
        }

        None
    }

    // generic number parser, it will out put either a int or float
    pub fn parse_number(
        &mut self,
        starting_position: Position,
    ) -> Option<Result<Token, LexerError>> {
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
                        source: Source::new_unhandled(starting_position, self.src),
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
    pub fn parse_float(
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
                    power *= 10_f64;
                    self.advance();
                    // update the number
                    decimal_point += f64::from(b - b'0') / power;
                }
                Some(_) => {
                    let float = number as f64 + decimal_point;
                    let token = Token {
                        // this is a lossy conversion and there will be no stoping from capturing
                        // this error at runtime atm. TODO
                        value: TokenType::Float(float),
                        source: Source::new_unhandled(starting_position, self.src),
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

    pub fn parse_minus(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'|') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::RBlock,
                    source: Source::new(start, end),
                }))
            },
            Some(_) => {
                // create the token
                let token = Token {
                    value: TokenType::Operation(OperationTokenType::Arithmetic('-')),
                    source: Source::new_single(start),
                };
                Some(Ok(token))
            }
            None => None
        }
    }

    pub fn parse_equal(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'=') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::EE)),
                    source: Source::new(start, end),
                }))
            }
            Some(_) => Some(Ok(Token {
                value: TokenType::Operation(OperationTokenType::EQ),
                source: Source::new_single(start),
            })),
            None => None,
        }
    }

    pub fn parse_not(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'=') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::NE)),
                    source: Source::new(start, end),
                }))
            }
            Some(_) => Some(Ok(Token {
                value: TokenType::Operation(OperationTokenType::Logic(LogicType::NOT)),
                source: Source::new_single(start),
            })),
            None => None,
        }
    }

    pub fn parse_and(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'&') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Logic(LogicType::AND)),
                    source: Source::new(start, end),
                }))
            }
            _ => Some(Err(LexerError::new(
                "Incomplete token error",
                "Expected '&&'".to_string(),
                self.src,
                self.buffer.iter().map(|b| *b as char).collect(),
            ))),
        }
    }

    pub fn parse_or(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'|') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Logic(LogicType::OR)),
                    source: Source::new(start, end),
                }))
            }
            Some(b'-') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::LBlock,
                    source: Source::new(start, end),
                }))
            }
            Some(_) => Some(Err(LexerError::new(
                "Incomplete token error",
                "Expected '||'".to_string(),
                self.src,
                self.buffer.iter().map(|b| *b as char).collect(),
            ))),
            None => None
        }
    }

    pub fn parse_lesser(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'=') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::LTE)),
                    source: Source::new(start, end),
                }))
            }
            Some(_) => Some(Ok(Token {
                value: TokenType::Operation(OperationTokenType::Comparison(CompType::LT)),
                source: Source::new_single(start),
            })),
            None => None,
        }
    }

    pub fn parse_greater(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
        self.advance();
        match self.byte {
            Some(b'=') => {
                let end = self.src;
                self.advance();
                Some(Ok(Token {
                    value: TokenType::Operation(OperationTokenType::Comparison(CompType::GTE)),
                    source: Source::new(start, end),
                }))
            }
            Some(_) => Some(Ok(Token {
                value: TokenType::Operation(OperationTokenType::Comparison(CompType::GT)),
                source: Source::new_single(start),
            })),
            None => None,
        }
    }
}
