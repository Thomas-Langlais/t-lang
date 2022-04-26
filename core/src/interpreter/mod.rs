use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::{OperationTokenType, TokenType};
use crate::parser::{FactorNode, SyntaxNode, TermNode, UnaryNode, VariableNode};

mod operations;
pub mod symbol_table;
pub use symbol_table::{SymbolEntry, SymbolTable, SymbolValue};

pub enum InterpretedType {
    Float(f64),
    Int(i64),
    Bool(bool),
}

#[derive(Debug)]
pub struct RTError {
    name: &'static str,
    details: &'static str,
}

#[derive(Debug)]
pub struct InterpreterError {
    pub name: &'static str,
    pub details: &'static str,
    pub source: String,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl<'a> RTError {
    pub fn into(self, context: &ExecutionContext) -> InterpreterError {
        let (start, end) = context.current_pos;
        InterpreterError {
            name: self.name,
            details: self.details,
            source: context.source_text.clone(),
            start,
            end,
            line: 0,
        }
    }
}

pub type InterpreterResult = Result<InterpretedType, InterpreterError>;

impl Display for InterpretedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpretedType::Int(int) => write!(f, "{}", int),
            InterpretedType::Float(float) => write!(f, "{}", float),
            InterpretedType::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        let line_header = format!("line {line}: ", line = self.line + 1);

        let underline = (0..self.start + line_header.len())
            .map(|_| ' ')
            .chain((self.start..=self.end).map(|_| '^'))
            .collect::<String>();

        let source = &self.source.lines().next().unwrap();

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

pub struct ExecutionContext<'a> {
    source_text: String,
    current_pos: (usize, usize),
    symbol_table: &'a SymbolTable<'a>,
    parent_context: Option<&'a ExecutionContext<'a>>,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(source_text: String, symbol_table: &'a SymbolTable<'a>) -> ExecutionContext<'a> {
        ExecutionContext {
            source_text,
            symbol_table,
            parent_context: None,
            current_pos: (0, 0),
        }
    }
}

impl From<InterpretedType> for bool {
    fn from(value: InterpretedType) -> Self {
        match value {
            InterpretedType::Float(float) => float != 0.0,
            InterpretedType::Int(int) => int != 0,
            InterpretedType::Bool(b) => b,
        }
    }
}

impl<'a> From<&InterpretedType> for SymbolValue {
    fn from(val: &InterpretedType) -> Self {
        match val {
            InterpretedType::Int(n) => SymbolValue::Int(*n),
            InterpretedType::Float(n) => SymbolValue::Float(*n),
            InterpretedType::Bool(n) => SymbolValue::Bool(*n),
        }
    }
}
impl From<SymbolValue> for InterpretedType {
    fn from(val: SymbolValue) -> Self {
        match val {
            SymbolValue::Int(n) => InterpretedType::Int(n),
            SymbolValue::Float(n) => InterpretedType::Float(n),
            SymbolValue::Bool(n) => InterpretedType::Bool(n),
        }
    }
}

pub trait Interpret<'a> {
    // I should try to add some trait methods that "visits" the children nodes
    // and use those instead.
    fn interpret(&self, context: &'a mut ExecutionContext) -> InterpreterResult;
}

impl<'a> Interpret<'a> for SyntaxNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        match self {
            Self::Variable(node) => node.interpret(context),
            Self::Factor(node) => node.interpret(context),
            Self::Unary(node) => node.interpret(context),
            Self::Term(node) => node.interpret(context),
        }
    }
}

impl<'a> Interpret<'a> for VariableNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        match &self.identifier_token.value {
            TokenType::Identifier(identifier) => {
                if self.assign {
                    let expression = unsafe { self.expression.as_ref().unwrap_unchecked() };
                    let result = expression.interpret(context)?;

                    context.current_pos = self.pos;
                    if let Err(err) = context
                        .symbol_table
                        .set(identifier, SymbolValue::from(&result))
                    {
                        return Err(err.into(context));
                    }

                    Ok(result)
                } else {
                    context.current_pos = self.pos;
                    let value_result = context.symbol_table.get(identifier);
                    if value_result.is_none() {
                        let (start, end) = context.current_pos;
                        return Err(InterpreterError {
                            name: "Symbol not found",
                            details: "The variable does not exist in the current context",
                            source: context.source_text.clone(),
                            start,
                            end,
                            line: 0,
                        });
                    }

                    let value = unsafe { value_result.unwrap_unchecked() };
                    Ok(InterpretedType::from(value))
                }
            }
            _ => panic!("A variable node can only have an identifier token"),
        }
    }
}

impl<'a> Interpret<'a> for FactorNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        context.current_pos = self.pos;

        match self.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Int(int)),
            TokenType::Float(float) => Ok(InterpretedType::Float(float)),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl<'a> Interpret<'a> for UnaryNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        context.current_pos = self.pos;

        match self.op_token.value {
            TokenType::Operation(OperationTokenType::Arithmetic(arith_type)) => {
                self.arith_op(arith_type, context)
            }
            TokenType::Operation(OperationTokenType::Logic(lgc_type)) => {
                self.logic_op(lgc_type, context)
            }
            _ => unreachable!("A unary operator can only be -/+ or !"),
        }
    }
}

impl<'a> Interpret<'a> for TermNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        let lhs = self.left_node.interpret(context)?;
        let rhs = self.right_node.interpret(context)?;

        // set the term position after the children have finished
        context.current_pos = self.pos;

        match self.op_token.value {
            TokenType::Operation(OperationTokenType::Arithmetic(arith_type)) => {
                self.arith_op(arith_type, lhs, rhs, context)
            }
            TokenType::Operation(OperationTokenType::Comparison(cmp_type)) => {
                self.comp_op(cmp_type, lhs, rhs)
            }
            TokenType::Operation(OperationTokenType::Logic(lgc_type)) => {
                self.logic_op(lgc_type, bool::from(lhs), bool::from(rhs))
            }
            _ => unreachable!("Only +,-,*,/ are allowed\nop {:?}", self.op_token),
        }
    }
}