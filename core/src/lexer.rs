use std::fmt::{self, Result as FormatResult};
use std::string::ToString;

// statics
static KEYWORDS: &[&str] = &["let"];

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
    Operation(OperationTokenType),
    Int(i64),
    Float(f64),
    LineTerm,
    EOF,
}

impl ToString for TokenType {
    fn to_string(&self) -> String {
        match self {
            TokenType::Keyword(word) => word.to_string(),
            TokenType::Identifier(identifier) => identifier.to_string(),
            TokenType::LParen(lp) => lp.to_string(),
            TokenType::RParen(rp) => rp.to_string(),
            TokenType::Operation(op) => op.to_string(),
            TokenType::Int(int) => int.to_string(),
            TokenType::Float(float) => float.to_string(),
            TokenType::LineTerm => ";".to_string(),
            TokenType::EOF => "\\n".to_string(),
        }
    }
}

impl ToString for OperationTokenType {
    fn to_string(&self) -> String {
        match self {
            Self::EQ => "=".to_string(),
            Self::Arithmetic(op) => op.to_string(),
            Self::Comparison(cmp) => cmp.to_string(),
            Self::Logic(lgc) => lgc.to_string(),
        }
    }
}

impl ToString for CompType {
    fn to_string(&self) -> String {
        match self {
            Self::EE => "==".to_string(),
            Self::NE => "!=".to_string(),
            Self::LT => "<".to_string(),
            Self::GT => ">".to_string(),
            Self::LTE => "<=".to_string(),
            Self::GTE => ">=".to_string(),
        }
    }
}

impl ToString for LogicType {
    fn to_string(&self) -> String {
        match self {
            Self::AND => "&&".to_string(),
            Self::OR => "||".to_string(),
            Self::NOT => "!".to_string(),
        }
    }
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
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => {
                    break self.parse_identifier(self.src);
                }
                Some(b'0'..=b'9') => {
                    // implement here.
                    break self.parse_number(self.src);
                }
                Some(op @ (b'+' | b'-' | b'*' | b'/')) => {
                    // create the token
                    let token = Token {
                        value: TokenType::Operation(OperationTokenType::Arithmetic(*op as char)),
                        source: Source::new_single(self.src),
                    };
                    // advance the iterator context to the next char
                    self.advance();
                    break Some(Ok(token));
                }
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
                        self.buffer
                            .iter()
                            .map(|b| *b as char)
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

    fn parse_identifier(
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

    fn parse_equal(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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

    fn parse_not(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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

    fn parse_and(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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

    fn parse_or(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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
            _ => Some(Err(LexerError::new(
                "Incomplete token error",
                "Expected '||'".to_string(),
                self.src,
                self.buffer.iter().map(|b| *b as char).collect(),
            ))),
        }
    }

    fn parse_lesser(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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

    fn parse_greater(&mut self, start: Position) -> Option<Result<Token, LexerError>> {
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
