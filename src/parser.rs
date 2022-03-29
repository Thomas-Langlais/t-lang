use std::fmt::{Display, Formatter, Result as FormatResult};
use std::mem;
use std::vec::IntoIter;

use crate::lexer::{Token, TokenType};

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

pub struct Parser {
    tokens: IntoIter<Token>,
    current_token: Option<Token>,
}

pub type ParseResult = Result<AbstractSyntaxTree, String>;
type InternalParseResult = Result<SyntaxNode, String>;

static EXPRESSION_OPS: [TokenType; 2] = [TokenType::Operation('+'), TokenType::Operation('-')];
static TERM_OPS: [TokenType; 2] = [TokenType::Operation('*'), TokenType::Operation('/')];

impl<'a> Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let mut parser = Parser {
            tokens: tokens.into_iter(),
            current_token: None,
        };
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        self.current_token = self.tokens.next();
    }

    /**
     * helper functions for parsing grammar nodes into the stacks
     */

    fn factor(&mut self) -> InternalParseResult {
        if let Some(token) = self.current_token.as_ref() {
            match token.value {
                TokenType::Int(_) | TokenType::Float(_) => {
                    let current_token = mem::replace(&mut self.current_token, None).unwrap();
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
                    
                    let expression = result.unwrap();
                    match &self.current_token {
                        Some(token) => {
                            if token.value == TokenType::RParen(')') {
                                self.advance();
                                return Ok(expression);
                            } else {
                                return Err("Expected ')'".to_string());
                            }
                        },
                        None => {
                            return Err("Expected ')'".to_string());
                        }
                    }
                }
                _ => {}
            }
        }
        Err("Expected an int or float".to_string())
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

        let mut left = left_result.unwrap();
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
            let right = right_result.unwrap();

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                op_token: op_token,
                right_node: Box::new(right),
            });
        }

        Ok(left)
    }

    pub fn generate_syntax_tree(mut self) -> ParseResult {
        let result = self.expression();
        match result {
            Ok(node) => Ok(AbstractSyntaxTree { inner: node }),
            Err(err) => Err(err),
        }
    }
}
