use std::fmt::{Display, Formatter, Result as FormatResult};
use std::mem;
use std::vec::IntoIter;

use crate::lexer::{Location, Token, TokenType};

pub enum ParseError {
    SyntaxError(IllegalSyntaxError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            ParseError::SyntaxError(err) => write!(f, "{}", err),
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
pub struct FactorNode {
    token: Token,
}

#[derive(Debug)]
pub struct TermNode {
    op_token: Token,
    left_node: Box<SyntaxNode>,
    right_node: Box<SyntaxNode>,
}

#[derive(Debug)]
pub enum SyntaxNode {
    Factor(FactorNode),
    Term(TermNode),
}

pub struct AbstractSyntaxTree {
    inner: SyntaxNode,
}

pub enum EvaluationResult {
    Float(f64),
    Int(u64),
}

impl Display for EvaluationResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            EvaluationResult::Int(int) => f.write_fmt(format_args!("{}", int)),
            EvaluationResult::Float(float) => f.write_fmt(format_args!("{}", float)),
        }
    }
}

pub trait Evaluate {
    fn evaluate(&self) -> EvaluationResult;
}

impl Evaluate for AbstractSyntaxTree {
    fn evaluate(&self) -> EvaluationResult {
        self.inner.evaluate()
    }
}
// use an enum for evalutation results
// I can use this to my advantage to avoid unknown typing issues in rust

impl Evaluate for SyntaxNode {
    fn evaluate(&self) -> EvaluationResult {
        match self {
            SyntaxNode::Factor(node) => node.evaluate(),
            SyntaxNode::Term(node) => node.evaluate(),
        }
    }
}

impl Evaluate for FactorNode {
    fn evaluate(&self) -> EvaluationResult {
        match self.token.value {
            TokenType::Int(int) => EvaluationResult::Int(int),
            TokenType::Float(float) => EvaluationResult::Float(float),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

fn add(left: EvaluationResult, right: EvaluationResult) -> EvaluationResult {
    if let EvaluationResult::Int(left_int) = left {
        if let EvaluationResult::Int(right_int) = right {
            let result = left_int + right_int;
            return EvaluationResult::Int(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float + right_float;
            return EvaluationResult::Float(result);
        }
    }
    if let EvaluationResult::Float(left_float) = left {
        if let EvaluationResult::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float + right_float;
            return EvaluationResult::Float(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let result = left_float + right_float;
            return EvaluationResult::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot add a non u64 or f64");
}

fn subtract(left: EvaluationResult, right: EvaluationResult) -> EvaluationResult {
    if let EvaluationResult::Int(left_int) = left {
        if let EvaluationResult::Int(right_int) = right {
            let result = left_int - right_int;
            return EvaluationResult::Int(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float - right_float;
            return EvaluationResult::Float(result);
        }
    }
    if let EvaluationResult::Float(left_float) = left {
        if let EvaluationResult::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float - right_float;
            return EvaluationResult::Float(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let result = left_float - right_float;
            return EvaluationResult::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot subtract a non u64 or f64");
}

fn multiply(left: EvaluationResult, right: EvaluationResult) -> EvaluationResult {
    if let EvaluationResult::Int(left_int) = left {
        if let EvaluationResult::Int(right_int) = right {
            let result = left_int * right_int;
            return EvaluationResult::Int(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float * right_float;
            return EvaluationResult::Float(result);
        }
    }
    if let EvaluationResult::Float(left_float) = left {
        if let EvaluationResult::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float * right_float;
            return EvaluationResult::Float(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let result = left_float * right_float;
            return EvaluationResult::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot multiply a non u64 or f64");
}

fn divide(left: EvaluationResult, right: EvaluationResult) -> EvaluationResult {
    if let EvaluationResult::Int(left_int) = left {
        if let EvaluationResult::Int(right_int) = right {
            let result = left_int / right_int;
            return EvaluationResult::Int(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float / right_float;
            return EvaluationResult::Float(result);
        }
    }
    if let EvaluationResult::Float(left_float) = left {
        if let EvaluationResult::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float / right_float;
            return EvaluationResult::Float(result);
        }
        if let EvaluationResult::Float(right_float) = right {
            let result = left_float / right_float;
            return EvaluationResult::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot divide a non u64 or f64");
}

impl Evaluate for TermNode {
    fn evaluate(&self) -> EvaluationResult {
        let left_result = self.left_node.evaluate();
        let right_result = self.right_node.evaluate();

        match self.op_token.value {
            TokenType::Operation('+') => add(left_result, right_result),
            TokenType::Operation('-') => subtract(left_result, right_result),
            TokenType::Operation('*') => multiply(left_result, right_result),
            TokenType::Operation('/') => divide(left_result, right_result),
            _ => panic!("Only +,-,*,/ are allowed\nop {:?}", self.op_token),
        }
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

    fn factor(&mut self) -> InternalParseResult {
        let current_token = mem::replace(&mut self.current_token, None).unwrap();

        match current_token.value {
            TokenType::Int(_) | TokenType::Float(_) => {
                self.advance();
                return Ok(SyntaxNode::Factor(FactorNode {
                    token: current_token,
                }));
            }
            TokenType::LParen('(') => {
                self.advance();
                let result = self.expression();
                if let Err(err) = result {
                    return Err(err);
                }
                let expression = unsafe { result.unwrap_unchecked() };

                let token_ref = self.current_token.as_ref().unwrap();
                let token_type = token_ref.value;
                let location = token_ref.source;

                if token_type == TokenType::RParen(')') {
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

    fn term(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.factor();
        self.bin_op(func, &TERM_OPS)
    }

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

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                op_token: op_token,
                right_node: Box::new(right),
            });
        }

        Ok(left)
    }

    pub fn generate_syntax_tree(&mut self) -> ParseResult {
        let result = self.expression();
        let current_token = self.current_token.as_ref().unwrap();
        let token_type = current_token.value;
        let token_location = current_token.source;

        match result {
            Ok(node) if matches!(token_type, TokenType::EOF) => {
                Ok(AbstractSyntaxTree { inner: node })
            }
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
