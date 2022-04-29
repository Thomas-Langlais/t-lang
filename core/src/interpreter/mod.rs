use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::{OperationTokenType, TokenType};
use crate::parser::{
    FactorNode, StatementNode, StatementListNode, SyntaxNode, TermNode, UnaryNode, VariableNode, IfNode,
};

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
        let line = context.line;
        InterpreterError {
            name: self.name,
            details: self.details,
            source: context.source_text.clone(),
            start,
            end,
            line,
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
        let line_header = format!("line {line}: ", line = self.line);

        let underline = (1..self.start + line_header.len())
            .map(|_| ' ')
            .chain((self.start..=self.end).map(|_| '^'))
            .collect::<String>();

        let source = self
            .source
            .lines()
            .enumerate()
            .skip_while(|(i, _)| i + 1 != self.line)
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

pub struct ExecutionContext<'a> {
    source_text: String,
    current_pos: (usize, usize),
    line: usize,
    symbol_table: &'a SymbolTable<'a>,
    parent_context: Option<&'a ExecutionContext<'a>>,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(source_text: String, symbol_table: &'a SymbolTable<'a>) -> ExecutionContext<'a> {
        ExecutionContext {
            source_text,
            current_pos: (0, 0),
            line: 0,
            symbol_table,
            parent_context: None,
        }
    }

    fn visit(&mut self, pos: (usize, usize), line: usize) {
        self.current_pos = pos;
        self.line = line;
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
            Self::If(node) => node.interpret(context),
            Self::Statements(node) => node.interpret(context),
            Self::Statement(node) => node.interpret(context),
            Self::For(_) => todo!(),
            Self::While(_) => todo!(),
            Self::Continue(_) => todo!(),
            Self::Break(_) => todo!(),
            Self::Variable(node) => node.interpret(context),
            Self::Factor(node) => node.interpret(context),
            Self::Unary(node) => node.interpret(context),
            Self::Term(node) => node.interpret(context),
        }
    }
}

impl<'a> Interpret<'a> for StatementListNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        // compile complains about last_result being unitialized without the assignment
        // the parser has to output at least one statement, so let's a temp value
        let mut last_result = InterpretedType::Int(0);

        for statement in &self.statements {
            last_result = statement.interpret(context)?;
        }

        Ok(last_result)
    }
}

impl<'a> Interpret<'a> for StatementNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        self.inner.interpret(context)
    }
}

impl<'a> Interpret<'a> for IfNode {
    fn interpret(&self, context: &'a mut ExecutionContext) -> InterpreterResult {
        for case in &self.if_nodes {
            let output = case.condition.interpret(context)?;
            if bool::from(output) {
                return case.statements.interpret(context);
            }
        }

        if let Some(else_case) = &self.else_node {
            return else_case.interpret(context);
        }
        
        unreachable!("An IfNode always has 1 if statement")
    }
}

impl<'a> Interpret<'a> for VariableNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        match &self.identifier_token.value {
            TokenType::Identifier(identifier) => {
                if self.assign {
                    let expression = match &self.expression {
                        Some(expr) => expr,
                        None => unreachable!("There should always be an expression"),
                    };
                    let result = expression.interpret(context)?;

                    context.visit(self.pos, self.line);
                    if let Err(err) = context
                        .symbol_table
                        .set(identifier, SymbolValue::from(&result))
                    {
                        return Err(err.into(context));
                    }

                    Ok(result)
                } else {
                    context.visit(self.pos, self.line);
                    let value_result = context.symbol_table.get(identifier);
                    if value_result.is_none() {
                        let (start, end) = context.current_pos;
                        let line = context.line;
                        return Err(InterpreterError {
                            name: "Symbol not found",
                            details: "The variable does not exist in the current context",
                            source: context.source_text.clone(),
                            start,
                            end,
                            line,
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
        context.visit(self.pos, self.line);

        match self.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Int(int)),
            TokenType::Float(float) => Ok(InterpretedType::Float(float)),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl<'a> Interpret<'a> for UnaryNode {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        context.visit(self.pos, self.line);

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
        context.visit(self.pos, self.line);

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
