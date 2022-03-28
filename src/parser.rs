use phf::phf_map;
use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::{Token, TokenType};

static OPERATOR_PRECEDENCE: phf::Map<char, u8> = phf_map! {
    '*' => 3,
    '/' => 3,
    '+' => 2,
    '-' => 2,
    '(' => 1
};

fn precedence(token_type: &TokenType) -> u8 {
    let ref op_char = match token_type {
        TokenType::Operation(op) => op,
        TokenType::LParen(lp) => lp,
        _ => panic!("Parser should only have operations and left parentheses"),
    };

    OPERATOR_PRECEDENCE[op_char]
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
    Int(u64)
}

impl Display for EvaluationResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            EvaluationResult::Int(int) => f.write_fmt(format_args!("{}", int)),
            EvaluationResult::Float(float) => f.write_fmt(format_args!("{}", float))
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
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Option<Token>,
}

impl<'a> Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            token_index: 0,
            current_token: None,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if matches!(self.current_token, Some(_)) {
            self.token_index += 1;
        }

        if self.token_index < self.tokens.len() {
            self.current_token = Some(self.tokens[self.token_index]);
        } else {
            self.current_token = None;
        }

        self.current_token
    }

    pub fn generate_syntax_tree(mut self) -> Result<Option<AbstractSyntaxTree>, String> {
        // create the postfix ordered tokens
        let postfix_tokens = {
            let mut result: Vec<Token> = Vec::new();
            let mut operation_stack: Vec<Token> = Vec::new();
            while let Some(token) = self.advance() {
                match token.value {
                    TokenType::Int(_) | TokenType::Float(_) => {
                        if self.token_index > 0
                            && !matches!(
                                self.tokens[self.token_index - 1].value,
                                TokenType::Operation(_) | TokenType::LParen(_)
                            )
                        {
                            return Err(format!("A number can only come after an opening parenthesis or operator but got {:?}", self.tokens[self.token_index - 1]));
                        }
                        result.push(token);
                    }
                    TokenType::Operation(op) => {
                        if self.token_index > 0
                            && !matches!(
                                self.tokens[self.token_index - 1].value,
                                TokenType::RParen(_) | TokenType::Int(_) | TokenType::Float(_)
                            )
                        {
                            return Err(format!("An operator can only come after a closing parenthesis or operand but got {:?}", self.tokens[self.token_index - 1]));
                        }
                        if self.token_index == 0 {
                            return Err("An expression cannot start with an operator".to_string());
                        }
                        while operation_stack.len() > 0
                            && precedence(&operation_stack.last().unwrap().value)
                                >= OPERATOR_PRECEDENCE[&op]
                        {
                            result.push(operation_stack.pop().unwrap());
                        }
                        operation_stack.push(token);
                    }
                    TokenType::LParen('(') => {
                        if self.token_index > 0
                            && !matches!(
                                self.tokens[self.token_index - 1].value,
                                TokenType::LParen('(') | TokenType::Operation(_)
                            )
                        {
                            return Err("An ( can only come after a (, or operator".to_string());
                        }
                        operation_stack.push(token);
                    }
                    TokenType::RParen(')') => {
                        if self.token_index > 0
                            && !matches!(
                                self.tokens[self.token_index - 1].value,
                                TokenType::RParen(')')
                                    | TokenType::LParen('(') // empty parenthesis
                                    | TokenType::Int(_)
                                    | TokenType::Float(_)
                            )
                        {
                            return Err(format!("A ) can only come after a ), int, or float"));
                        }
                        if self.token_index == 0 {
                            return Err(format!("An expression cannot start with a )"));
                        }
                        let mut op_option = operation_stack.pop();
                        let found = loop {
                            match op_option {
                                None => break false,
                                Some(Token {
                                    value: TokenType::LParen('('),
                                    ..
                                }) => break true,
                                Some(op_token) => {
                                    result.push(op_token);
                                    op_option = operation_stack.pop();
                                }
                            }
                        };

                        if !found {
                            return Err(format!(
                                "Parser could not find the closing '(' {:?}",
                                token
                            ));
                        }
                    }
                    _ => {
                        panic!("Parser cannot understand the token {:?}", token.value);
                    }
                };
            }
            while operation_stack.len() > 0 {
                let token = operation_stack.pop().unwrap();

                if matches!(token.value, TokenType::LParen(_)) {
                    return Err(format!("Unmatched closing parentheses {:?}", token));
                }
                result.push(token);
            }

            result
        };

        let mut token_iter = postfix_tokens.iter();

        // parse the ordered tokens into a syntax tree
        let mut operand_stack: Vec<SyntaxNode> = Vec::new();
        while let Some(token) = token_iter.next() {
            match token.value {
                TokenType::Float(_) | TokenType::Int(_) => {
                    operand_stack.push(SyntaxNode::Factor(FactorNode { token: *token }));
                }
                TokenType::Operation(_) => {
                    assert!(operand_stack.len() >= 2, "Unfinished expression/term");

                    let right_node = Box::new(operand_stack.pop().unwrap());
                    let left_node = Box::new(operand_stack.pop().unwrap());

                    operand_stack.push(SyntaxNode::Term(TermNode {
                        op_token: *token,
                        left_node: left_node,
                        right_node: right_node,
                    }));
                }
                _ => {
                    panic!("Unknown token ({:?}) in postfix ordered tokens", token)
                }
            }
        }

        if operand_stack.len() > 1 {
            return Err(format!("unknown {0:#?} {1:#?}", operand_stack, self.tokens));
        }

        Ok(match operand_stack.pop() {
            Some(node) => Some(AbstractSyntaxTree { inner: node }),
            None => None,
        })
    }
}
