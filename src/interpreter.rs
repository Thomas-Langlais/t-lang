use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::TokenType;
use crate::parser::{FactorNode, SyntaxNode, TermNode, UnaryNode};

pub enum InterpretedType {
    Float(f64),
    Int(i64),
}

pub struct DivideByZeroError {
    name: String,
    details: String,
    source: String,
    line: usize,
    start: usize,
    end: usize,
}

pub enum InterpreterError {
    DivideByZero(DivideByZeroError),
}

impl DivideByZeroError {
    fn new(details: &str, start: usize, end: usize, line: usize) -> DivideByZeroError {
        DivideByZeroError {
            name: String::from("Divide by zero"),
            details: String::from(details),
            source: String::new(),
            line,
            start,
            end,
        }
    }
}

pub type InterpreterResult = Result<InterpretedType, InterpreterError>;

impl Display for InterpretedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpretedType::Int(int) => write!(f, "{}", int),
            InterpretedType::Float(float) => write!(f, "{}", float),
        }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpreterError::DivideByZero(err) => err.fmt(f),
        }
    }
}

impl Display for DivideByZeroError {
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

pub trait Interpret {
    fn interpret(&self) -> InterpreterResult;
}

impl Interpret for SyntaxNode {
    fn interpret(&self) -> InterpreterResult {
        match self {
            SyntaxNode::Factor(node) => node.interpret(),
            SyntaxNode::UnaryFactor(node) => node.interpret(),
            SyntaxNode::Term(node) => node.interpret(),
        }
    }
}

impl Interpret for FactorNode {
    fn interpret(&self) -> InterpreterResult {
        match self.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Int(int)),
            TokenType::Float(float) => Ok(InterpretedType::Float(float)),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl Interpret for UnaryNode {
    fn interpret(&self) -> InterpreterResult {
        if matches!(self.op_token.value, TokenType::Operation('+')) {
            return self.node.interpret();
        }
        if matches!(self.op_token.value, TokenType::Operation('-')) {
            let result = self.node.interpret();
            if let Err(err) = result {
                return Err(err);
            }

            let rhs = unsafe { result.unwrap_unchecked() };
            return subtract(InterpretedType::Int(0), rhs);
        }

        panic!("A unary operator can only be -/+");
    }
}

impl Interpret for TermNode {
    fn interpret(&self) -> InterpreterResult {
        let left_result = self.left_node.interpret();
        if let Err(err) = left_result {
            return Err(err);
        }
        let right_result = self.right_node.interpret();
        if let Err(err) = right_result {
            return Err(err);
        }

        let (lhs, rhs) = unsafe {
            (
                left_result.unwrap_unchecked(),
                right_result.unwrap_unchecked(),
            )
        };

        match self.op_token.value {
            TokenType::Operation('+') => add(lhs, rhs),
            TokenType::Operation('-') => subtract(lhs, rhs),
            TokenType::Operation('*') => multiply(lhs, rhs),
            TokenType::Operation('/') => divide(lhs, rhs),
            _ => panic!("Only +,-,*,/ are allowed\nop {:?}", self.op_token),
        }
    }
}

fn add(left: InterpretedType, right: InterpretedType) -> InterpreterResult {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int + right_int;
            return Ok(InterpretedType::Int(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float + right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float + right_float;
            return Ok(InterpretedType::Float(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float + right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    // It should NEVER reach here
    panic!("Cannot add a non i64 or f64");
}

fn subtract(left: InterpretedType, right: InterpretedType) -> InterpreterResult {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int - right_int;
            return Ok(InterpretedType::Int(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float - right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float - right_float;
            return Ok(InterpretedType::Float(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float - right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    // It should NEVER reach here
    panic!("Cannot subtract a non i64 or f64");
}

fn multiply(left: InterpretedType, right: InterpretedType) -> InterpreterResult {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int * right_int;
            return Ok(InterpretedType::Int(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float * right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float * right_float;
            return Ok(InterpretedType::Float(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float * right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    // It should NEVER reach here
    panic!("Cannot multiply a non i64 or f64");
}

fn divide(left: InterpretedType, right: InterpretedType) -> InterpreterResult {
    if matches!(right, InterpretedType::Int(int) if int == 0)
        | matches!(right, InterpretedType::Float(float) if float == 0.0)
    {
        return Err(InterpreterError::DivideByZero(DivideByZeroError::new(
            "The right hand side of the division expression is 0",
            0,
            0,
            0,
        )));
    }
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int / right_int;
            return Ok(InterpretedType::Int(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float / right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float / right_float;
            return Ok(InterpretedType::Float(result));
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float / right_float;
            return Ok(InterpretedType::Float(result));
        }
    }
    // It should NEVER reach here
    panic!("Cannot divide a non i64 or f64");
}
