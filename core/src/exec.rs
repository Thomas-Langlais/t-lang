use std::fmt::{Display, Formatter, Result as FormatResult};

use crate::lexer::{OperationTokenType, TokenType, Source};
use crate::ast::{
    FactorNode, ForNode, IfNode, StatementListNode, StatementNode, SyntaxNode, TermNode, UnaryNode,
    VariableNode, WhileNode,
};

use crate::symbol_table::{SymbolTable, SymbolValue};

pub enum InterpretedType {
    Float(f64),
    Int(i64),
    Bool(bool),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct RTError {
    pub name: &'static str,
    pub details: &'static str,
}

pub type Result = std::result::Result<InterpretedType, RTError>;

impl Display for InterpretedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpretedType::Int(int) => write!(f, "{}", int),
            InterpretedType::Float(float) => write!(f, "{}", float),
            InterpretedType::Bool(b) => write!(f, "{}", b),
            InterpretedType::Break => write!(f, "brk"),
            InterpretedType::Continue => write!(f, "con"),
        }
    }
}

pub struct ExecutionContext<'a> {
    pub (crate) source: Source,
    pub (crate) symbol_table: &'a SymbolTable<'a>,
    pub (crate) parent_context: Option<&'a ExecutionContext<'a>>,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(symbol_table: &'a SymbolTable<'a>) -> ExecutionContext<'a> {
        ExecutionContext {
            source: Source::default(),
            symbol_table,
            parent_context: None,
        }
    }

    pub fn source(&self) -> Source {
        self.source
    }

    fn visit(&mut self, source: Source) {
        self.source = source;
    }
}

impl From<InterpretedType> for bool {
    fn from(value: InterpretedType) -> Self {
        match value {
            InterpretedType::Float(float) => float != 0.0,
            InterpretedType::Int(int) => int != 0,
            InterpretedType::Bool(b) => b,
            _ => panic!("handle con and brk conditions"),
        }
    }
}

impl<'a> From<&InterpretedType> for SymbolValue {
    fn from(val: &InterpretedType) -> Self {
        match val {
            InterpretedType::Int(n) => SymbolValue::Int(*n),
            InterpretedType::Float(n) => SymbolValue::Float(*n),
            InterpretedType::Bool(n) => SymbolValue::Bool(*n),
            _ => panic!("handle con and brk conditions"),
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
    fn interpret(&self, context: &'a mut ExecutionContext) -> Result;
}

impl<'a> Interpret<'a> for SyntaxNode {
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        match self {
            Self::If(node) => node.interpret(context),
            Self::Statements(node) => node.interpret(context),
            Self::Statement(node) => node.interpret(context),
            Self::For(node) => node.interpret(context),
            Self::While(node) => node.interpret(context),
            Self::Continue(_) => Ok(InterpretedType::Continue),
            Self::Break(_) => Ok(InterpretedType::Break),
            Self::Variable(node) => node.interpret(context),
            Self::Factor(node) => node.interpret(context),
            Self::Unary(node) => node.interpret(context),
            Self::Term(node) => node.interpret(context),
        }
    }
}

impl<'a> Interpret<'a> for StatementListNode {
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        // compile complains about last_result being unitialized without the assignment
        // the parser has to output at least one statement, so let's a temp value
        let mut last_result = InterpretedType::Int(0);
        context.visit(self.source);

        for statement in &self.statements {
            last_result = statement.interpret(context)?;
            if matches!(
                last_result,
                InterpretedType::Continue | InterpretedType::Break
            ) {
                break;
            }
        }

        Ok(last_result)
    }
}

impl<'a> Interpret<'a> for StatementNode {
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        context.visit(self.source);
        self.inner.interpret(context)
    }
}

impl<'a> Interpret<'a> for IfNode {
    fn interpret(&self, context: &'a mut ExecutionContext) -> Result {
        context.visit(self.source);
        
        for case in &self.if_nodes {
            context.visit(case.source);
            let output = case.condition.interpret(context)?;
            if bool::from(output) {
                return case.statements.interpret(context);
            }
        }

        if let Some(else_case) = &self.else_node {
            return else_case.interpret(context);
        }

        Ok(InterpretedType::Int(0))
    }
}

impl<'a> Interpret<'a> for ForNode {
    fn interpret(&self, context: &'a mut ExecutionContext) -> Result {
        context.visit(self.source);

        if let Some(decl) = &self.declaration {
            decl.interpret(context)?;
        }

        loop {
            if let Some(cond) = &self.condition {
                if !bool::from(cond.interpret(context)?) {
                    break;
                };
            }

            let result = self.block.interpret(context)?;
            if let InterpretedType::Break = result {
                break;
            }

            if let Some(incr) = &self.increment {
                incr.interpret(context)?;
            }
        }

        Ok(InterpretedType::Int(0))
    }
}

impl<'a> Interpret<'a> for WhileNode {
    fn interpret(&self, context: &'a mut ExecutionContext) -> Result {
        context.visit(self.source);
        
        loop {
            if !bool::from(self.condition.interpret(context)?) {
                break;
            }

            match self.block.interpret(context)? {
                InterpretedType::Continue => {
                    continue;
                }
                InterpretedType::Break => {
                    break;
                }
                _ => {}
            }
        }

        Ok(InterpretedType::Int(0))
    }
}

impl<'a> Interpret<'a> for VariableNode {
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        context.visit(self.source);
        
        match &self.identifier_token.value {
            TokenType::Identifier(identifier) => {
                if self.assign {
                    let expression = match &self.expression {
                        Some(expr) => expr,
                        None => unreachable!("There should always be an expression"),
                    };
                    let result = expression.interpret(context)?;

                    context.visit(self.source);
                    if let Err(err) = context
                        .symbol_table
                        .set(identifier, SymbolValue::from(&result))
                    {
                        return Err(err);
                    }

                    Ok(result)
                } else {
                    context.visit(self.source);
                    let value_result = context.symbol_table.get(identifier);
                    if value_result.is_none() {
                        return Err(RTError {
                            name: "Symbol not found",
                            details: "The variable does not exist in the current context",
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
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        context.visit(self.source);

        match self.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Int(int)),
            TokenType::Float(float) => Ok(InterpretedType::Float(float)),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl<'a> Interpret<'a> for UnaryNode {
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        context.visit(self.source);

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
    fn interpret(&self, context: &mut ExecutionContext) -> Result {
        let lhs = self.left_node.interpret(context)?;
        let rhs = self.right_node.interpret(context)?;

        // set the term position after the children have finished
        context.visit(self.source);

        match self.op_token.value {
            TokenType::Operation(OperationTokenType::Arithmetic(arith_type)) => {
                self.arith_op(arith_type, lhs, rhs)
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
