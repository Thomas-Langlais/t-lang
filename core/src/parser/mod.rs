use std::fmt::{Debug, Display, Formatter, Result as FormatResult};
use std::mem;

use crate::interpreter::{ExecutionContext, Interpret, InterpreterResult};
use crate::lexer::{Position, Source, Token, TokenType};

mod grammar;

pub enum ParseError {
    SyntaxError(IllegalSyntaxError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        match self {
            ParseError::SyntaxError(err) => err.fmt(f),
        }
    }
}

pub struct IllegalSyntaxError {
    pub name: String,
    pub details: String,
    pub source: String,
    pub location: Source,
    reached_eof: bool,
}

impl IllegalSyntaxError {
    fn new(
        name: &str,
        details: &str,
        location: Source,
        source: String,
        reached_eof: bool,
    ) -> IllegalSyntaxError {
        IllegalSyntaxError {
            name: name.to_string(),
            details: details.to_string(),
            source,
            location,
            reached_eof,
        }
    }

    fn new_invalid_syntax(details: &str, location: Source, source: &[u8]) -> IllegalSyntaxError {
        let src: String = source.iter().map(|&b| b as char).collect();
        let reached_eof = location.start == location.end && location.start.index == src.len() - 1;
        Self::new("Illegal Syntax", details, location, src, reached_eof)
    }
}

impl Display for IllegalSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        if self.reached_eof {
            return write!(
                f,
                "{name} - {details}\nEnd of file reached",
                name = self.name,
                details = self.details
            );
        }

        let line_header = format!("line {line}: ", line = self.location.start.line);

        let underline = (1..self.location.start.column + line_header.len())
            .map(|_| ' ')
            .chain((self.location.start.index..=self.location.end.index).map(|_| '^'))
            .collect::<String>();

        let source = self
            .source
            .lines()
            .enumerate()
            .skip_while(|(i, _)| i + 1 != self.location.start.line)
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

#[derive(Debug)]
pub struct VariableNode {
    pub identifier_token: Token,
    pub expression: Option<Box<SyntaxNode>>,
    pub assign: bool,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct FactorNode {
    pub token: Token,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct UnaryNode {
    pub op_token: Token,
    pub node: Box<SyntaxNode>,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct TermNode {
    pub op_token: Token,
    pub left_node: Box<SyntaxNode>,
    pub right_node: Box<SyntaxNode>,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct ConditionNode {
    pub condition: Box<SyntaxNode>,
    pub statements: Box<SyntaxNode>,
}

#[derive(Debug)]
pub struct IfNode {
    pub if_nodes: Vec<ConditionNode>,
    pub else_node: Option<Box<SyntaxNode>>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct StatementNode {
    pub inner: Box<SyntaxNode>,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct StatementListNode {
    pub statements: Vec<SyntaxNode>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct ForNode {
    pub declaration: Option<Box<SyntaxNode>>,
    pub condition: Option<Box<SyntaxNode>>,
    pub increment: Option<Box<SyntaxNode>>,
    pub block: Box<SyntaxNode>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct WhileNode {
    pub condition: Box<SyntaxNode>,
    pub block: Box<SyntaxNode>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub struct ContinueNode(Token);

#[derive(Debug)]
pub struct BreakNode(Token);

#[derive(Debug)]
pub enum SyntaxNode {
    If(IfNode),
    Statements(StatementListNode),
    Statement(StatementNode),
    For(ForNode),
    While(WhileNode),
    Continue(ContinueNode),
    Break(BreakNode),
    Variable(VariableNode),
    Factor(FactorNode),
    Unary(UnaryNode),
    Term(TermNode),
}

impl SyntaxNode {
    fn get_pos(&self) -> (usize, usize) {
        match self {
            Self::If(node) => node.pos,
            Self::Statements(node) => node.pos,
            Self::Statement(node) => node.pos,
            Self::For(node) => node.pos,
            Self::While(node) => node.pos,
            Self::Continue(ContinueNode(node)) => {
                (node.source.start.column, node.source.end.column)
            }
            Self::Break(BreakNode(node)) => (node.source.start.column, node.source.end.column),
            Self::Variable(node) => node.pos,
            Self::Factor(node) => node.pos,
            Self::Unary(node) => node.pos,
            Self::Term(node) => node.pos,
        }
    }

    fn set_pos(&mut self, pos: (usize, usize)) {
        match self {
            Self::If(node) => node.pos = pos,
            Self::Statements(node) => node.pos = pos,
            Self::Statement(node) => node.pos = pos,
            Self::For(node) => node.pos = pos,
            Self::While(node) => node.pos = pos,
            Self::Continue(ContinueNode(node)) => {
                node.source.start.column = pos.0;
                node.source.end.column = pos.1;
            }
            Self::Break(BreakNode(node)) => {
                node.source.start.column = pos.0;
                node.source.end.column = pos.1;
            }
            Self::Variable(node) => node.pos = pos,
            Self::Factor(node) => node.pos = pos,
            Self::Unary(node) => node.pos = pos,
            Self::Term(node) => node.pos = pos,
        }
    }
}

pub struct AbstractSyntaxTree {
    inner: SyntaxNode,
}

impl Debug for AbstractSyntaxTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        write!(f, "{:#?}", self.inner)
    }
}

impl<'a> Interpret<'a> for AbstractSyntaxTree {
    fn interpret(&self, context: &mut ExecutionContext) -> InterpreterResult {
        self.inner.interpret(context)
    }
}

#[derive(Debug)]
pub struct ParserOkRunner(ParseContext, SyntaxNode);
pub struct ParserErrorRunner(ParseContext, ParseError);

impl From<InternalParseResult> for ParserErrorRunner {
    fn from(res: InternalParseResult) -> Self {
        res.expect_err("This is infallible, there should NEVER the Result::Ok variant")
    }
}

pub struct Parser<'a> {
    source: &'a [u8],
    index: usize,
    tokens: Vec<Token>,
    current_token: Option<Token>,
}

#[derive(Clone, Copy, Default, Debug)]
struct ParseContext {
    advances: usize,
    reverse_advances: usize,
}

impl ParseContext {
    fn advance(&mut self) {
        self.advances += 1;
    }

    fn register(&mut self, result: InternalParseResult) -> Result<SyntaxNode, InternalParseResult> {
        self.advances += match &result {
            Ok(ParserOkRunner(ctx, _)) => ctx.advances,
            Err(ParserErrorRunner(ctx, _)) => ctx.advances,
        };

        match result {
            Ok(ParserOkRunner(_, node)) => Ok(node),
            Err(ParserErrorRunner(_, err)) => Err(Err(ParserErrorRunner(*self, err))),
        }
    }

    fn try_register(
        &mut self,
        result: InternalParseResult,
    ) -> Result<SyntaxNode, InternalParseResult> {
        if let Err(ParserErrorRunner(context, _)) = &result {
            self.reverse_advances = context.advances;
        }
        self.register(result)
    }

    fn success(self, node: SyntaxNode) -> InternalParseResult {
        Ok(ParserOkRunner(self, node))
    }

    fn failure(self, node: ParseError) -> InternalParseResult {
        Err(ParserErrorRunner(self, node))
    }
}

type InternalParseResult = Result<ParserOkRunner, ParserErrorRunner>;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a [u8]) -> Parser<'a> {
        let mut parser = Parser {
            source,
            index: 0,
            tokens,
            current_token: None,
        };
        parser.update_token();

        parser
    }

    fn advance(&mut self) {
        self.index += 1;
        self.update_token();
    }

    fn reverse(&mut self, amount: usize) {
        self.index -= amount;
        self.update_token();
    }

    fn update_token(&mut self) {
        if (0..self.tokens.len()).contains(&self.index) {
            self.current_token = Some(self.tokens[self.index].clone())
        } else {
            self.current_token = None
        }
    }

    /**
     * helper functions for parsing grammar nodes into the stacks
     */
    fn bin_op(
        &mut self,
        func: fn(&mut Parser) -> InternalParseResult,
        ops: &[TokenType],
    ) -> InternalParseResult {
        let mut context = ParseContext::default();
        let mut left = context.register(func(self))?;

        while let Some(token) = &self.current_token {
            if !ops.iter().any(|op| token.value == *op) {
                break;
            }

            let op_token = mem::replace(&mut self.current_token, None).unwrap();
            context.advance();
            self.advance();
            let right = context.register(func(self))?;
            let (start, _) = left.get_pos();
            let (_, end) = right.get_pos();
            let pos = (start, end);
            let line = op_token.source.start.line;

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                right_node: Box::new(right),
                op_token,
                pos,
                line,
            });
        }

        context.success(left)
    }

    // TODO: refactor the grammar to use this utility func
    fn expect_and_consume(
        &mut self,
        context: &mut ParseContext,
        token_type: &'static TokenType,
        error: &'static str,
    ) -> Result<(), InternalParseResult> {
        match &self.current_token {
            Some(Token { value, .. }) if value == token_type => {
                self.advance();
                context.advance();
                Ok(())
            }
            _ => Err(context.failure(ParseError::SyntaxError(
                IllegalSyntaxError::new_invalid_syntax(
                    error,
                    self.current_token.as_ref().unwrap().source,
                    self.source,
                ),
            ))),
        }
    }

    fn expect_and_parse<F>(
        &mut self,
        context: &mut ParseContext,
        parse: F,
        token_type: &'static TokenType,
        error: &'static str,
    ) -> Result<SyntaxNode, InternalParseResult>
    where
        F: FnOnce(&mut Self) -> InternalParseResult,
    {
        match &self.current_token {
            Some(Token { value, .. }) if value == token_type => {
                Ok(context.register(parse(self))?)
            }
            _ => Err(context.failure(ParseError::SyntaxError(
                IllegalSyntaxError::new_invalid_syntax(
                    error,
                    self.current_token.as_ref().unwrap().source,
                    self.source,
                ),
            ))),
        }
    }

    fn skip_line_term(&mut self, context: &mut ParseContext) -> (usize, Source) {
        let mut newlines = 0;
        let mut last_source = Source::default();

        while let Some(Token {
            value: TokenType::LineTerm,
            source,
        }) = self.current_token
        {
            last_source = source;
            self.advance();
            context.advance();
            newlines += 1;
        }

        if let Some(token) = &self.current_token {
            last_source = token.source;
        }

        (newlines, last_source)
    }

    pub fn generate_syntax_tree(&mut self) -> Result<AbstractSyntaxTree, ParseError> {
        let result = self.statements();
        let current_token = self.current_token.as_ref().unwrap();
        let token_type = &current_token.value;

        match result {
            Ok(ParserOkRunner(_, node)) if matches!(token_type, &TokenType::EOF) => {
                Ok(AbstractSyntaxTree { inner: node })
            }
            Err(ParserErrorRunner(_, err)) => Err(err),
            _ => {
                let start = current_token.source.start;
                let end = Position {
                    index: self.tokens.last().unwrap().source.end.index - 1,
                    ..self.tokens.last().unwrap().source.end
                };
                let token_location = Source { start, end };
                Err(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "Expected a variable declaration or expression",
                        token_location,
                        self.source,
                    ),
                ))
            }
        }
    }
}
