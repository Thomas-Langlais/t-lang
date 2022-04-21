use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::TokenType;
use crate::parser::{FactorNode, SyntaxNode, TermNode, UnaryNode, VariableNode};

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
pub struct SymbolNotFoundError {
    name: String,
    details: String,
    source: String,
    line: usize,
    start: usize,
    end: usize,
}
pub enum InterpreterError {
    DivideByZero(DivideByZeroError),
    SymbolNotFound(SymbolNotFoundError),
}

impl DivideByZeroError {
    fn new(
        details: &str,
        source: String,
        start: usize,
        end: usize,
        line: usize,
    ) -> DivideByZeroError {
        DivideByZeroError {
            name: String::from("Divide by zero"),
            details: String::from(details),
            source: source,
            line,
            start,
            end,
        }
    }
}
impl SymbolNotFoundError {
    fn new(
        details: &str,
        source: String,
        start: usize,
        end: usize,
        line: usize,
    ) -> SymbolNotFoundError {
        SymbolNotFoundError {
            name: String::from("Symbol not found"),
            details: String::from(details),
            source: source,
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
            InterpreterError::SymbolNotFound(err) => err.fmt(f),
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
impl Display for SymbolNotFoundError {
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
pub enum SymbolValue {
    Int(i64),
    Float(f64),
}

pub struct SymbolTable {
    pub symbols: HashMap<String, SymbolValue>,
    parent_context: Option<Box<ExecutionContext>>,
}

pub struct ExecutionContext {
    source_text: String,
    current_pos: (usize, usize),
    pub symbol_table: Box<SymbolTable>,
    parent_context: Option<Box<ExecutionContext>>,
}

impl ExecutionContext {
    pub fn new(source_text: String, symbol_table: Box<SymbolTable>) -> ExecutionContext {
        ExecutionContext {
            source_text,
            symbol_table,
            parent_context: None,
            current_pos: (0, 0),
        }
    }
}

impl SymbolTable {
    pub fn new(symbols: HashMap<String, SymbolValue>) -> Self {
        SymbolTable {
            symbols,
            parent_context: None,
        }
    }

    pub fn get(&self, identifier: &str) -> Option<&SymbolValue> {
        if let Some(value) = self.symbols.get(identifier) {
            return Some(value);
        }

        let mut parent_context = &self.parent_context;
        while let Some(parent) = parent_context {
            if let Some(value) = parent.symbol_table.symbols.get(identifier) {
                return Some(value);
            }
            parent_context = &parent.parent_context;
        }

        None
    }

    pub fn set(&mut self, identifier: &str, value: SymbolValue) {
        self.symbols.insert(identifier.to_string(), value);
    }

    pub fn remove(&mut self, identifier: &str) {
        self.symbols.remove(identifier);
    }
}

pub trait Interpret {
    // I should try to add some trait methods that "visits" the children nodes
    // and use those instead.
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult;
}

impl Interpret for SyntaxNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        match self {
            Self::Variable(node) => node.interpret(context),
            Self::Factor(node) => node.interpret(context),
            Self::UnaryFactor(node) => node.interpret(context),
            Self::Term(node) => node.interpret(context),
        }
    }
}

impl Interpret for VariableNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        match &self.identifier_token.value {
            TokenType::Identifier(identifier) => {
                if self.assign {
                    let expression = unsafe { self.expression.as_ref().unwrap_unchecked() };
                    let expression_result = expression.interpret(context);
                    if let Err(err) = expression_result {
                        return Err(err);
                    }

                    let result = unsafe { expression_result.unwrap_unchecked() };
                    context.current_pos = self.pos;

                    context.symbol_table.set(
                        identifier,
                        match result {
                            InterpretedType::Int(int) => SymbolValue::Int(int),
                            InterpretedType::Float(float) => SymbolValue::Float(float),
                        },
                    );

                    Ok(result)
                } else {
                    context.current_pos = self.pos;
                    let value_result = context.symbol_table.get(identifier);
                    if value_result.is_none() {
                        let (start, end) = context.current_pos;
                        return Err(InterpreterError::SymbolNotFound(SymbolNotFoundError::new(
                            "The variable does not exist in the current context",
                            context.source_text.clone(),
                            start,
                            end,
                            0,
                        )));
                    }

                    let value = unsafe { value_result.unwrap_unchecked() };
                    match value {
                        SymbolValue::Int(int) => Ok(InterpretedType::Int(*int)),
                        SymbolValue::Float(float) => Ok(InterpretedType::Float(*float)),
                    }
                    // todo!("implement variable get interpret");
                }
            }
            _ => panic!("A variable node can only have an identifier token"),
        }
    }
}

impl Interpret for FactorNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        context.current_pos = self.pos;

        match self.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Int(int)),
            TokenType::Float(float) => Ok(InterpretedType::Float(float)),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl Interpret for UnaryNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        context.current_pos = self.pos;

        if matches!(self.op_token.value, TokenType::Operation('+')) {
            return self.node.interpret(context);
        }
        if matches!(self.op_token.value, TokenType::Operation('-')) {
            let result = self.node.interpret(context);
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
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        let left_result = self.left_node.interpret(context);
        if let Err(err) = left_result {
            return Err(err);
        }
        let right_result = self.right_node.interpret(context);
        if let Err(err) = right_result {
            return Err(err);
        }

        // set the term position after the children have finished
        context.current_pos = self.pos;
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
            TokenType::Operation('/') => divide(lhs, rhs, context),
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

fn divide(
    left: InterpretedType,
    right: InterpretedType,
    context: &ExecutionContext,
) -> InterpreterResult {
    if matches!(right, InterpretedType::Int(int) if int == 0)
        | matches!(right, InterpretedType::Float(float) if float == 0.0)
    {
        let (start, end) = context.current_pos;
        return Err(InterpreterError::DivideByZero(DivideByZeroError::new(
            "The right hand side of the division expression is 0",
            context.source_text.clone(),
            start,
            end,
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
