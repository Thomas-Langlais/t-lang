use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::{CompType, LogicType, OperationTokenType, TokenType};
use crate::parser::{FactorNode, SyntaxNode, TermNode, UnaryNode, VariableNode};

mod operations;

// #[derive(PartialEq, PartialOrd)]
pub enum InterpretedType {
    Float(f64),
    Int(i64),
}

pub struct DivideByZeroError {
    name: &'static str,
    details: &'static str,
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

#[derive(Debug, Clone, Copy)]
pub enum SymbolValue {
    Int(i64),
    Float(f64),
}

pub struct SymbolTable<'a> {
    pub symbols: RefCell<HashMap<String, SymbolValue>>,
    parent_context: Option<&'a ExecutionContext<'a>>,
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

unsafe impl<'a> Sync for SymbolTable<'a> {}

impl<'a> SymbolTable<'a> {
    pub fn new(symbols: HashMap<String, SymbolValue>) -> Self {
        SymbolTable {
            symbols: RefCell::new(symbols),
            parent_context: None,
        }
    }

    pub fn get(&self, identifier: &str) -> Option<SymbolValue> {
        let symbols = self.symbols.borrow();
        if let Some(value) = symbols.get(identifier) {
            return Some(*value);
        }

        let mut parent_context = &self.parent_context;
        while let Some(parent) = parent_context {
            if let Some(value) = parent.symbol_table.symbols.borrow().get(identifier) {
                return Some(*value);
            }
            parent_context = &parent.parent_context;
        }

        None
    }

    pub fn set(&self, identifier: &str, value: SymbolValue) {
        self.symbols
            .borrow_mut()
            .insert(identifier.to_string(), value);
    }

    pub fn remove(&self, identifier: &str) {
        self.symbols.borrow_mut().remove(identifier);
    }
}

impl From<InterpretedType> for bool {
    fn from(value: InterpretedType) -> Self {
        match value {
            InterpretedType::Float(float) => float != 0.0,
            InterpretedType::Int(int) => int != 0,
        }
    }
}

impl TermNode {
    fn arith_op(
        &self,
        arith_type: char,
        lhs: InterpretedType,
        rhs: InterpretedType,
        context: &ExecutionContext,
    ) -> InterpreterResult {
        match arith_type {
            '+' => Ok(lhs + rhs),
            '-' => Ok(lhs - rhs),
            '*' => Ok(lhs * rhs),
            '/' => match lhs / rhs {
                Ok(res) => Ok(res),
                Err(err) => Err(err.into(context)),
            },
            _ => unreachable!("unknown arithmetic operation"),
        }
    }

    fn comp_op(
        &self,
        cmp_type: CompType,
        lhs: InterpretedType,
        rhs: InterpretedType,
        context: &ExecutionContext,
    ) -> InterpreterResult {
        match cmp_type {
            CompType::EE => Ok(InterpretedType::Int(i64::from(lhs == rhs))),
            CompType::NE => Ok(InterpretedType::Int(i64::from(lhs != rhs))),
            CompType::GT => Ok(InterpretedType::Int(i64::from(lhs > rhs))),
            CompType::GTE => Ok(InterpretedType::Int(i64::from(lhs >= rhs))),
            CompType::LT => Ok(InterpretedType::Int(i64::from(lhs < rhs))),
            CompType::LTE => Ok(InterpretedType::Int(i64::from(lhs <= rhs))),
        }
    }

    fn logic_op(
        &self,
        lgc_type: LogicType,
        lhs: bool,
        rhs: bool,
        context: &ExecutionContext,
    ) -> InterpreterResult {
        match lgc_type {
            LogicType::AND => {
                Ok(InterpretedType::Int(i64::from(lhs && rhs)))
            },
            LogicType::OR => {
                Ok(InterpretedType::Int(i64::from(lhs || rhs)))
            },
            _ => unreachable!("Cannot interpret ! operation for a term")
        }
    }
}

impl UnaryNode {
    fn arith_op(&self, arith_type: char, context: &mut ExecutionContext) -> InterpreterResult {
        match arith_type {
            '+' => self.node.interpret(context),
            '-' => {
                let rhs = self.node.interpret(context)?;
                context.current_pos = self.pos;
                Ok(InterpretedType::Int(-1) * rhs)
            }
            _ => unreachable!("A unary arithmetic operation can only be -/+"),
        }
    }

    fn logic_op(&self, lgc_type: LogicType, context: &mut ExecutionContext) -> InterpreterResult {
        match lgc_type {
            LogicType::NOT => {
                let rhs = self.node.interpret(context)?;
                context.current_pos = self.pos;
                Ok(InterpretedType::Int(i64::from(!bool::from(rhs))))
            }
            _ => unreachable!("A unary operator can only be !"),
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
                        SymbolValue::Int(int) => Ok(InterpretedType::Int(int)),
                        SymbolValue::Float(float) => Ok(InterpretedType::Float(float)),
                    }
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
                self.comp_op(cmp_type, lhs, rhs, context)
            }
            TokenType::Operation(OperationTokenType::Logic(lgc_type)) => {
                self.logic_op(lgc_type, bool::from(lhs), bool::from(rhs), context)
            }
            _ => unreachable!("Only +,-,*,/ are allowed\nop {:?}", self.op_token),
        }
    }
}
