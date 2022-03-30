use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::TokenType;
use crate::parser::{SyntaxNode, FactorNode, UnaryNode, TermNode};

pub enum InterpretedType {
    Float(f64),
    Int(i64),
}

impl Display for InterpretedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpretedType::Int(int) => f.write_fmt(format_args!("{}", int)),
            InterpretedType::Float(float) => f.write_fmt(format_args!("{}", float)),
        }
    }
}

pub trait Interpret {
    fn interpret(&self) -> InterpretedType;
}

impl Interpret for SyntaxNode {
    fn interpret(&self) -> InterpretedType {
        match self {
            SyntaxNode::Factor(node) => node.interpret(),
            SyntaxNode::UnaryFactor(node) => node.interpret(),
            SyntaxNode::Term(node) => node.interpret(),
        }
    }
}

impl Interpret for FactorNode {
    fn interpret(&self) -> InterpretedType {
        match self.token.value {
            TokenType::Int(int) => InterpretedType::Int(int),
            TokenType::Float(float) => InterpretedType::Float(float),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl Interpret for UnaryNode {
    fn interpret(&self) -> InterpretedType {
        if matches!(self.op_token.value, TokenType::Operation('+')) {
            return self.node.interpret();
        }
        if matches!(self.op_token.value, TokenType::Operation('-')) {
            return subtract(InterpretedType::Int(0), self.node.interpret());
        }

        panic!("A unary operator can only be -/+");
    }
}

impl Interpret for TermNode {
    fn interpret(&self) -> InterpretedType {
        let left_result = self.left_node.interpret();
        let right_result = self.right_node.interpret();

        match self.op_token.value {
            TokenType::Operation('+') => add(left_result, right_result),
            TokenType::Operation('-') => subtract(left_result, right_result),
            TokenType::Operation('*') => multiply(left_result, right_result),
            TokenType::Operation('/') => divide(left_result, right_result),
            _ => panic!("Only +,-,*,/ are allowed\nop {:?}", self.op_token),
        }
    }
}

fn add(left: InterpretedType, right: InterpretedType) -> InterpretedType {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int + right_int;
            return InterpretedType::Int(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float + right_float;
            return InterpretedType::Float(result);
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float + right_float;
            return InterpretedType::Float(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float + right_float;
            return InterpretedType::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot add a non i64 or f64");
}

fn subtract(left: InterpretedType, right: InterpretedType) -> InterpretedType {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int - right_int;
            return InterpretedType::Int(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float - right_float;
            return InterpretedType::Float(result);
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float - right_float;
            return InterpretedType::Float(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float - right_float;
            return InterpretedType::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot subtract a non i64 or f64");
}

fn multiply(left: InterpretedType, right: InterpretedType) -> InterpretedType {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int * right_int;
            return InterpretedType::Int(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float * right_float;
            return InterpretedType::Float(result);
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float * right_float;
            return InterpretedType::Float(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float * right_float;
            return InterpretedType::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot multiply a non i64 or f64");
}

fn divide(left: InterpretedType, right: InterpretedType) -> InterpretedType {
    if let InterpretedType::Int(left_int) = left {
        if let InterpretedType::Int(right_int) = right {
            let result = left_int / right_int;
            return InterpretedType::Int(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let left_float = left_int as f64;
            let result = left_float / right_float;
            return InterpretedType::Float(result);
        }
    }
    if let InterpretedType::Float(left_float) = left {
        if let InterpretedType::Int(right_int) = right {
            let right_float = right_int as f64;
            let result = left_float / right_float;
            return InterpretedType::Float(result);
        }
        if let InterpretedType::Float(right_float) = right {
            let result = left_float / right_float;
            return InterpretedType::Float(result);
        }
    }
    // It should NEVER reach here
    panic!("Cannot divide a non i64 or f64");
}