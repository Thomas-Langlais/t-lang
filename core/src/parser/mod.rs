use std::fmt::{Debug, Display, Formatter, Result as FormatResult};
use std::mem;
use std::vec::IntoIter;

use crate::interpreter::{ExecutionContext, Interpret, InterpreterResult};
use crate::lexer::{CompType, LogicType, OperationTokenType, Position, Source, Token, TokenType};

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
    pub location: Source
}

impl IllegalSyntaxError {
    fn new(name: &str, details: &str, location: Source, source: String) -> IllegalSyntaxError {
        IllegalSyntaxError {
            name: name.to_string(),
            details: details.to_string(),
            source,
            location,
        }
    }

    fn new_invalid_syntax(details: &str, location: Source, source: &[u8]) -> IllegalSyntaxError {
        let src = source.iter().map(|&b| b as char).collect();
        Self::new("Illegal Syntax", details, location, src)
    }
}

impl Display for IllegalSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
        let line_header = format!("line {line}: ", line = self.location.start.line);

        let underline = (1..self.location.start.column + line_header.len())
            .map(|_| ' ')
            .chain((self.location.start.column..=self.location.end.column).map(|_| '^'))
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
pub struct Statement {
    pub inner: Box<SyntaxNode>,
    pub pos: (usize, usize),
    pub line: usize,
}

#[derive(Debug)]
pub struct StatementList {
    pub statements: Vec<Statement>,
    pub pos: (usize, usize),
}

#[derive(Debug)]
pub enum SyntaxNode {
    Statements(StatementList),
    Variable(VariableNode),
    Factor(FactorNode),
    Unary(UnaryNode),
    Term(TermNode),
}

impl SyntaxNode {
    fn get_pos(&self) -> (usize, usize) {
        match self {
            Self::Statements(node) => node.pos,
            Self::Variable(node) => node.pos,
            Self::Factor(node) => node.pos,
            Self::Unary(node) => node.pos,
            Self::Term(node) => node.pos,
        }
    }

    fn set_pos(&mut self, pos: (usize, usize)) {
        match self {
            Self::Statements(node) => node.pos = pos,
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

pub struct Parser<'a> {
    source: &'a [u8],
    tokens: IntoIter<Token>,
    current_token: Option<Token>,
}

pub type ParseResult = Result<AbstractSyntaxTree, ParseError>;
type InternalParseResult = Result<SyntaxNode, ParseError>;

mod grammar;

impl<'a> Parser<'a> {

    pub fn new(tokens: Vec<Token>, source: &'a [u8]) -> Parser<'a> {
        let mut parser = Parser {
            source,
            tokens: tokens.into_iter(),
            current_token: None,
        };
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        let next = self.tokens.next();
        self.current_token = next;
    }

    /**
     * helper functions for parsing grammar nodes into the stacks
     */
    fn bin_op(
        &mut self,
        func: fn(&mut Parser) -> InternalParseResult,
        ops: &[TokenType],
    ) -> InternalParseResult {
        let left_result = func(self);
        if let Err(err) = left_result {
            return Err(err);
        }

        let mut left = unsafe { left_result.unwrap_unchecked() };
        while let Some(token) = &self.current_token {
            if !ops.iter().any(|op| token.value == *op) {
                break;
            }

            let op_token = mem::replace(&mut self.current_token, None).unwrap();
            self.advance();

            let right_result = func(self);
            if let Err(err) = right_result {
                return Err(err);
            }
            let right = unsafe { right_result.unwrap_unchecked() };

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

        Ok(left)
    }

    fn skip_line_term(&mut self) -> (usize, Source) {
        let mut newlines = 0;
        let mut last_source = Source::default();

        while let Some(Token {
            value: TokenType::LineTerm,
            source,
        }) = self.current_token
        {
            last_source = source;
            self.advance();
            newlines += 1;
        }

        if let Some(token) = &self.current_token {
            last_source = token.source;
        }

        (newlines, last_source)
    }

    pub fn generate_syntax_tree(&mut self) -> ParseResult {
        let result = self.statements();
        let current_token = self.current_token.as_ref().unwrap();
        let token_type = &current_token.value;
        let token_location = current_token.source;

        match result {
            Ok(node) if matches!(token_type, &TokenType::EOF) => {
                Ok(AbstractSyntaxTree { inner: node })
            }
            Err(err) => Err(err),
            _ => {
                return Err(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "Expected '+', '-', '*', '/'",
                        token_location,
                        self.source,
                    ),
                ));
            }
        }
    }
}
