use std::fmt::{Display, Formatter, Result as FormatResult};
use std::mem;
use std::vec::IntoIter;

use crate::interpreter::{Execute, ExecutionContext, Interpret, InterpreterResult};
use crate::lexer::{Location, Token, TokenType};

pub enum ParseError {
    SyntaxError(IllegalSyntaxError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            ParseError::SyntaxError(err) => err.fmt(f),
        }
    }
}

pub struct IllegalSyntaxError {
    pub name: String,
    pub details: String,
    pub source: String,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl IllegalSyntaxError {
    fn new(name: &str, details: &str, location: Location, source: String) -> IllegalSyntaxError {
        IllegalSyntaxError {
            name: name.to_string(),
            details: details.to_string(),
            source: source,
            line: location.start.line,
            start: location.start.column,
            end: location.end.column,
        }
    }

    fn new_invalid_syntax(details: &str, location: Location, source: String) -> IllegalSyntaxError {
        Self::new(&"Illegal Syntax", details, location, source)
    }
}

impl Display for IllegalSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        let line_header = format!("line {line}: ", line = self.line + 1);

        let underline = (0..self.start + line_header.len())
            .map(|_| ' ')
            .chain((self.start..=self.end).map(|_| '^'))
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

#[derive(Debug)]
pub struct VariableNode {
    pub token: Token,
    pub assign: bool,
    pub pos: (usize, usize)
}

#[derive(Debug)]
pub struct FactorNode {
    pub token: Token,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct UnaryNode {
    pub op_token: Token,
    pub node: Box<SyntaxNode>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct TermNode {
    pub op_token: Token,
    pub left_node: Box<SyntaxNode>,
    pub right_node: Box<SyntaxNode>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub enum SyntaxNode {
    Variable(VariableNode),
    Factor(FactorNode),
    UnaryFactor(UnaryNode),
    Term(TermNode),
}

impl SyntaxNode {
    fn get_pos(&self) -> (usize, usize) {
        match self {
            Self::Variable(node) => node.pos,
            Self::Factor(node) => node.pos,
            Self::UnaryFactor(node) => node.pos,
            Self::Term(node) => node.pos,
        }
    }

    fn set_pos(&mut self, pos: (usize, usize)) {
        match self {
            Self::Variable(node) => node.pos = pos,
            Self::Factor(node) => node.pos = pos,
            Self::UnaryFactor(node) => node.pos = pos,
            Self::Term(node) => node.pos = pos,
        }
    }
}

pub struct AbstractSyntaxTree {
    inner: SyntaxNode,
    source_text: String,
}

impl Execute for AbstractSyntaxTree {
    fn execute(&self) -> InterpreterResult {
        let mut context = ExecutionContext::new(self.source_text.clone());
        self.inner.interpret(&mut context)
    }
}

struct DebugItem {
    value: String,
    location: Location,
}

pub struct Parser {
    tokens: IntoIter<Token>,
    current_token: Option<Token>,
    debug_line: Vec<DebugItem>,
}

pub type ParseResult = Result<AbstractSyntaxTree, ParseError>;
type InternalParseResult = Result<SyntaxNode, ParseError>;

static EXPRESSION_OPS: [TokenType; 2] = [TokenType::Operation('+'), TokenType::Operation('-')];
static TERM_OPS: [TokenType; 2] = [TokenType::Operation('*'), TokenType::Operation('/')];

impl<'a> Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let mut parser = Parser {
            tokens: tokens.into_iter(),
            current_token: None,
            debug_line: Vec::new(),
        };
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        let next = self.tokens.next();

        match &next {
            Some(token) if !matches!(token.value, TokenType::EOF) => {
                self.debug_line.push(DebugItem {
                    value: token.value.to_string(),
                    location: token.source,
                });
            }
            _ => {}
        }

        self.current_token = next;
    }

    fn load_debug_line(&mut self) {
        let current_line = match &self.current_token {
            Some(token) => token.source.start.line,
            None => 0,
        };

        while self.current_token.is_some()
            && !matches!(self.current_token.as_ref().unwrap().value, TokenType::EOF)
            && current_line == self.current_token.as_ref().unwrap().source.start.line
        {
            self.advance();
        }
    }

    /**
     * helper functions for parsing grammar nodes into the stacks
     */

    /// factor = atom
    ///        = (PLUS|MINUS) factor
    fn factor(&mut self) -> InternalParseResult {
        let current_token = mem::replace(&mut self.current_token, None).unwrap();

        match current_token.value {
            TokenType::Operation('+') | TokenType::Operation('-') => {
                self.advance();
                let factor_result = self.factor();
                if let Err(err) = factor_result {
                    return Err(err);
                }

                let factor = unsafe { factor_result.unwrap_unchecked() };
                let (_, end) = factor.get_pos();
                let start = current_token.source.start.column;

                Ok(SyntaxNode::UnaryFactor(UnaryNode {
                    op_token: current_token,
                    node: Box::new(factor),
                    pos: (start, end),
                }))
            }
            TokenType::Int(_) | TokenType::Float(_) => {
                self.advance();
                let pos = (
                    current_token.source.start.column,
                    current_token.source.end.column,
                );
                Ok(SyntaxNode::Factor(FactorNode {
                    token: current_token,
                    pos: pos,
                }))
            }
            TokenType::LParen('(') => {
                let start = current_token.source.start.column;
                self.advance();

                let result = self.expression();
                if let Err(err) = result {
                    return Err(err);
                }
                let mut expression = unsafe { result.unwrap_unchecked() };

                let token_ref = self.current_token.as_ref().unwrap();
                let token_type = &token_ref.value;
                let location = token_ref.source;

                if token_type == &TokenType::RParen(')') {
                    let end = location.end.column;
                    expression.set_pos((start, end));
                    self.advance();
                    return Ok(expression);
                } else {
                    self.load_debug_line();
                    let source = self.regenerate_source();
                    return Err(ParseError::SyntaxError(
                        IllegalSyntaxError::new_invalid_syntax("Expected ')'", location, source),
                    ));
                }
            }
            _ => {
                let location = current_token.source;
                // reset the parsers current token since, it's not a valid factor token
                self.current_token = Some(current_token);
                self.load_debug_line();
                return Err(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "Expected an int or float",
                        location,
                        self.regenerate_source(),
                    ),
                ));
            }
        }
    }

    /// atom = INT|FLOAT|IDENTIFIER
    ///      = LParen expression RParen
    fn atom(&mut self) -> InternalParseResult {
        todo!("not implemented");
    }

    /// term = factor (MUL|DIV factor)*
    fn term(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.factor();
        self.bin_op(func, &TERM_OPS)
    }

    /// expression = KW:LET IDENTIFIER EQ expression 
    ///            = term (PLUS|MINUS term)*
    fn expression(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.term();
        self.bin_op(func, &EXPRESSION_OPS)
    }

    fn bin_op(
        &mut self,
        func: fn(&mut Parser) -> InternalParseResult,
        ops: &[TokenType],
    ) -> InternalParseResult {
        let left_result = func(self);
        if let Err(err) = left_result {
            return Err(err);
        }

        let mut left = unsafe { left_result.unwrap_unchecked() };
        while let Some(token) = &self.current_token {
            if !ops.iter().any(|op| token.value == *op) {
                break;
            }

            let op_token = mem::replace(&mut self.current_token, None).unwrap();
            self.advance();

            let right_result = func(self);
            if let Err(err) = right_result {
                return Err(err);
            }
            let right = unsafe { right_result.unwrap_unchecked() };

            let (start, _) = left.get_pos();
            let (_, end) = right.get_pos();

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                op_token: op_token,
                right_node: Box::new(right),
                pos: (start, end),
            });
        }

        Ok(left)
    }

    pub fn generate_syntax_tree(&mut self) -> ParseResult {
        let result = self.expression();
        let current_token = self.current_token.as_ref().unwrap();
        let token_type = &current_token.value;
        let token_location = current_token.source;

        match result {
            Ok(node) if matches!(token_type, &TokenType::EOF) => Ok(AbstractSyntaxTree {
                inner: node,
                source_text: self.regenerate_source(),
            }),
            Err(err) => Err(err),
            _ => {
                self.load_debug_line();
                return Err(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "Expected '+', '-', '*', '/'",
                        token_location,
                        self.regenerate_source(),
                    ),
                ));
            }
        }
    }

    fn regenerate_source(&self) -> String {
        let mut debug: Vec<String> = Vec::new();
        let mut end = 0usize;
        self.debug_line.iter().for_each(|next| {
            let start = next.location.start.column;
            debug.push((end + 1..start).map(|_| ' ').collect::<String>());
            debug.push(next.value.to_string());
            end = next.location.end.column;
        });

        debug.into_iter().collect::<String>()
    }
}
