use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FormatResult};
use std::io;

use async_recursion::async_recursion;

use crate::ast::{
    FactorNode, ForNode, IfNode, StatementListNode, StatementNode, SyntaxNode, TermNode, UnaryNode,
    VariableNode, WhileNode, Value
};
use crate::lexer::{OperationTokenType, Source, TokenType};
use crate::parser::{Error as ParserError, Parser};
use crate::symbol_table::{SymbolEntry, SymbolTable, Symbol};

pub enum InterpretedType {
    Value(Value),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct RTError {
    pub name: &'static str,
    pub details: &'static str,
}

pub enum Error {
    ParserError(ParserError),
    RuntimeError(RTError, Source),
    StoppedError(u8),
}

pub enum InterpreterError {
    Runtime(RTError),
    Stopped
}

impl From<RTError> for InterpreterError {
    fn from(err: RTError) -> Self {
        Self::Runtime(err)
    }
}

pub(crate) type Result = std::result::Result<InterpretedType, InterpreterError>;
pub type ExecResult<T> = std::result::Result<T, Error>;

impl Display for InterpretedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            InterpretedType::Value(Value::Int(int)) => write!(f, "{}", int),
            InterpretedType::Value(Value::Float(float)) => write!(f, "{}", float),
            InterpretedType::Value(Value::Bool(b)) => write!(f, "{}", b),
            InterpretedType::Break => write!(f, "brk"),
            InterpretedType::Continue => write!(f, "con"),
        }
    }
}

pub(crate) struct ExecutionContext<'a> {
    pub(crate) source: Source,
    pub(crate) symbol_table: &'a SymbolTable<'a>,
    pub(crate) parent_context: Option<&'a ExecutionContext<'a>>,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(symbol_table: &'a SymbolTable<'a>) -> ExecutionContext<'a> {
        ExecutionContext {
            source: Source::default(),
            symbol_table,
            parent_context: None,
        }
    }

    fn visit(&mut self, source: Source) {
        self.source = source;
    }
}

impl From<InterpretedType> for bool {
    fn from(value: InterpretedType) -> Self {
        match value {
            InterpretedType::Value(Value::Float(float)) => float != 0.0,
            InterpretedType::Value(Value::Int(int)) => int != 0,
            InterpretedType::Value(Value::Bool(b)) => b,
            _ => panic!("handle con and brk conditions"),
        }
    }
}

impl<'a> From<&InterpretedType> for Symbol {
    fn from(val: &InterpretedType) -> Self {
        match val {
            &InterpretedType::Value(value) => Symbol::Value(value),
            _ => panic!("handle con and brk conditions"),
        }
    }
}
impl From<Symbol> for InterpretedType {
    fn from(val: Symbol) -> Self {
        match val {
            Symbol::Value(value) => InterpretedType::Value(value),
        }
    }
}

impl From<ParserError> for Error {
    fn from(err: ParserError) -> Self {
        Self::ParserError(err)
    }
}

pub struct Machine<'a> {
    stop_reason: Option<u8>,
    symbols: SymbolTable<'a>,
}

impl<'a> Machine<'a> {
    #[async_recursion(?Send)]
    async fn interpret_node(
        &self,
        node: &SyntaxNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        if self.stop_reason.is_some() {
            return Err(InterpreterError::Stopped);
        }

        match node {
            SyntaxNode::FunctionDeclaration(node) => todo!("implement the function declaration execution"),
            SyntaxNode::FunctionInvocation(node) => todo!("implement the function invocation execution"),
            SyntaxNode::If(node) => self.interpret_if_node(node, context).await,
            SyntaxNode::Statements(node) => self.interpret_statement_list_node(node, context).await,
            SyntaxNode::Statement(node) => self.interpret_statement_node(node, context).await,
            SyntaxNode::For(node) => self.interpret_for_node(node, context).await,
            SyntaxNode::While(node) => self.interpret_while_node(node, context).await,
            SyntaxNode::Continue(_) => Ok(InterpretedType::Continue),
            SyntaxNode::Break(_) => Ok(InterpretedType::Break),
            SyntaxNode::Return(node) => todo!("implement the return statement execution"),
            SyntaxNode::Variable(node) => self.interpret_variable_node(node, context).await,
            SyntaxNode::Factor(node) => self.interpret_factor_node(node, context).await,
            SyntaxNode::Unary(node) => self.interpret_unary_node(node, context).await,
            SyntaxNode::Term(node) => self.interpret_term_node(node, context).await,
        }
    }

    async fn interpret_statement_list_node(
        &self,
        node: &StatementListNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        // compile complains about last_result being unitialized without the assignment
        // the parser has to output at least one statement, so let's a temp value
        let mut last_result = InterpretedType::Value(Value::Int(0));
        context.visit(node.source);

        for statement in &node.statements {
            last_result = self.interpret_node(statement, context).await?;
            if matches!(
                last_result,
                InterpretedType::Continue | InterpretedType::Break
            ) {
                break;
            }
        }

        Ok(last_result)
    }

    async fn interpret_statement_node(
        &self,
        node: &StatementNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);
        self.interpret_node(&node.inner, context).await
    }

    async fn interpret_for_node(
        &self,
        node: &ForNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);

        if let Some(decl) = &node.declaration {
            self.interpret_node(decl, context).await?;
        }

        loop {
            if let Some(cond) = &node.condition {
                if !bool::from(self.interpret_node(cond, context).await?) {
                    break;
                };
            }

            let result = self.interpret_node(&node.block, context).await?;
            if let InterpretedType::Break = result {
                break;
            }

            if let Some(incr) = &node.increment {
                self.interpret_node(incr, context).await?;
            }
        }

        Ok(InterpretedType::Value(Value::Int(0)))
    }

    async fn interpret_while_node(
        &self,
        node: &WhileNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);

        loop {
            if !bool::from(self.interpret_node(&node.condition, context).await?) {
                break;
            }

            match self.interpret_node(&node.block, context).await? {
                InterpretedType::Continue => {
                    continue;
                }
                InterpretedType::Break => {
                    break;
                }
                _ => {}
            }
        }

        Ok(InterpretedType::Value(Value::Int(0)))
    }

    async fn interpret_if_node(&self, node: &IfNode, context: &mut ExecutionContext<'_>) -> Result {
        context.visit(node.source);

        for case in &node.if_nodes {
            context.visit(case.source);
            let output = self.interpret_node(&case.condition, context).await?;
            if bool::from(output) {
                return self.interpret_node(&case.statements, context).await;
            }
        }

        if let Some(else_case) = &node.else_node {
            return self.interpret_node(&else_case, context).await;
        }

        Ok(InterpretedType::Value(Value::Int(0)))
    }

    async fn interpret_variable_node(
        &self,
        node: &VariableNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);

        match &node.identifier_token.value {
            TokenType::Identifier(identifier) => {
                if node.assign {
                    let expression = match &node.expression {
                        Some(expr) => expr,
                        None => unreachable!("There should always be an expression"),
                    };
                    let result = self.interpret_node(expression, context).await?;

                    context.visit(node.source);
                    if let Err(err) = context
                        .symbol_table
                        .set(identifier, Symbol::from(&result))
                    {
                        return Err(err.into());
                    }

                    Ok(result)
                } else {
                    context.visit(node.source);
                    let value_result = context.symbol_table.get(identifier);
                    if value_result.is_none() {
                        return Err(RTError {
                            name: "Symbol not found",
                            details: "The variable does not exist in the current context",
                        }.into());
                    }

                    let value = unsafe { value_result.unwrap_unchecked() };
                    Ok(InterpretedType::from(value))
                }
            }
            _ => panic!("A variable node can only have an identifier token"),
        }
    }

    async fn interpret_unary_node(
        &self,
        node: &UnaryNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);

        match node.op_token.value {
            TokenType::Operation(OperationTokenType::Arithmetic(arith_type)) => {
                let input = self.interpret_node(&node.node, context).await;
                self.unary_arith_op(input, arith_type).await
            }
            TokenType::Operation(OperationTokenType::Logic(lgc_type)) => {
                let input = self.interpret_node(&node.node, context).await;
                self.unary_logic_op(input, lgc_type).await
            }
            _ => unreachable!("A unary operator can only be -/+ or !"),
        }
    }

    async fn interpret_term_node(
        &self,
        node: &TermNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        let lhs = self.interpret_node(&node.left_node, context).await?;
        let rhs = self.interpret_node(&node.right_node, context).await?;

        // set the term position after the children have finished
        context.visit(node.source);

        match node.op_token.value {
            TokenType::Operation(OperationTokenType::Arithmetic(arith_type)) => {
                self.term_arith_op(arith_type, lhs, rhs).await
            }
            TokenType::Operation(OperationTokenType::Comparison(cmp_type)) => {
                self.term_comp_op(cmp_type, lhs, rhs).await
            }
            TokenType::Operation(OperationTokenType::Logic(lgc_type)) => {
                self.term_logic_op(lgc_type, bool::from(lhs), bool::from(rhs))
                    .await
            }
            _ => unreachable!("Only +,-,*,/ are allowed\nop {:?}", node.op_token),
        }
    }

    async fn interpret_factor_node(
        &self,
        node: &FactorNode,
        context: &mut ExecutionContext<'_>,
    ) -> Result {
        context.visit(node.source);

        match node.token.value {
            TokenType::Int(int) => Ok(InterpretedType::Value(Value::Int(int))),
            TokenType::Float(float) => Ok(InterpretedType::Value(Value::Float(float))),
            _ => panic!("A factor should only be a int or a float"),
        }
    }
}

impl Machine<'_> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stop_reason: None,
            symbols: SymbolTable::new(HashMap::from([
                (
                    String::from("true"),
                    SymbolEntry {
                        value: Symbol::Value(Value::Bool(true)),
                        is_constant: true,
                    },
                ),
                (
                    String::from("false"),
                    SymbolEntry {
                        value: Symbol::Value(Value::Bool(false)),
                        is_constant: true,
                    },
                ),
            ])),
        }
    }

    pub fn exit(&mut self, code: u8) {
        self.stop_reason = Some(code);
    }

    pub async fn exec(
        &mut self,
        input: &'_ mut dyn io::Read,
    ) -> ExecResult<Option<InterpretedType>> {
        debug_assert!(self.stop_reason.is_none());

        let mut parser = Parser::from(input);
        let mut last_result = None;

        while self.stop_reason.is_none() {
            match parser.parse_one()? {
                Some(node) => {
                    let mut context = ExecutionContext::new(&self.symbols);
                    last_result = match self.interpret_node(&node, &mut context).await {
                        Ok(result) => Some(result),
                        Err(InterpreterError::Runtime(err)) => return Err(Error::RuntimeError(err, context.source)),
                        // this is a shim to avoid handling stopped errors
                        _ => continue,
                    };
                }
                None => break,
            }
        }

        match self.stop_reason.take() {
            None => Ok(last_result),
            Some(code) => Err(Error::StoppedError(code)),
        }
    }
}
