use std::cell::{RefCell};
use std::fmt::{Debug, Display, Formatter, Result as FormatResult};
use std::io;
use std::iter::Peekable;
use std::rc::Rc;

// use crate::interpreter::{ExecutionContext, Interpret, InterpreterResult};
use crate::lexer::{Lexer, Source, Token, TokenType};

mod grammar;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(IllegalSyntaxError),
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct FactorNode {
    pub token: Token,
}

#[derive(Debug)]
pub struct UnaryNode {
    pub op_token: Token,
    pub node: Box<SyntaxNode>,
}

#[derive(Debug)]
pub struct TermNode {
    pub op_token: Token,
    pub left_node: Box<SyntaxNode>,
    pub right_node: Box<SyntaxNode>,
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
}

#[derive(Debug)]
pub struct StatementNode {
    pub inner: Box<SyntaxNode>,
}

#[derive(Debug)]
pub struct StatementListNode {
    pub statements: Vec<SyntaxNode>,
}

#[derive(Debug)]
pub struct ForNode {
    pub declaration: Option<Box<SyntaxNode>>,
    pub condition: Option<Box<SyntaxNode>>,
    pub increment: Option<Box<SyntaxNode>>,
    pub block: Box<SyntaxNode>,
}

#[derive(Debug)]
pub struct WhileNode {
    pub condition: Box<SyntaxNode>,
    pub block: Box<SyntaxNode>,
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

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    buf: Rc<RefCell<Vec<char>>>,
}

impl<'a> From<&'a mut dyn io::Read> for Parser<'a> {
    fn from(reader: &'a mut dyn io::Read) -> Self {
        let buf = Rc::new(RefCell::new(vec![]));
        
        let mut lexer = Lexer::from(reader);
        lexer.read_buffer = Rc::clone(&buf);
        
        Parser {
            lexer: lexer.peekable(),
            buf,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Bad(&'static str, Source),
    Io(io::Error),
}

type Result<T> = std::result::Result<T, Error>;

impl<'a> Parser<'a> {
    /**
     * helper functions for parsing grammar nodes into the stacks
     */
    fn bin_op(
        &mut self,
        func: fn(&mut Parser) -> Result<SyntaxNode>,
        ops: &[TokenType],
    ) -> Result<SyntaxNode> {
        let mut left = func(self)?;

        while let Some(Ok(token)) = self.lexer.peek() {
            if !ops.iter().any(|op| token.value == *op) {
                break;
            }

            let op_token = self.lexer.next().unwrap().unwrap();
            if let TokenType::Bad(msg, source) = op_token.value {
                return Err(Error::Bad(msg, source));
            }

            let right = func(self)?;

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                right_node: Box::new(right),
                op_token,
            });
        }

        Ok(left)
    }

    fn skip_passed(&mut self, token_type: TokenType) -> Result<()> {
        loop {
            match self.lexer.peek() {
                Some(Err(_)) => break Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
                Some(Ok(Token { value, .. })) if value == &token_type => {
                    self.lexer.next();
                    break Ok(());
                }
                _ => {
                    self.lexer.next();
                }
            }
        }
    }

    // TODO: refactor the grammar to use this utility func
    fn expect_and_consume(
        &mut self,
        token_type: &'static TokenType,
        error: &'static str,
    ) -> Result<()> {
        match self.lexer.peek() {
            Some(Ok(Token { value, .. })) if value == token_type => {
                self.lexer.next();
                Ok(())
            }
            Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => Err(Error::Bad(
                error,
                self.lexer.peek().as_ref().unwrap().as_ref().unwrap().source,
            )),
        }
    }

    fn expect_and_parse<F>(
        &mut self,
        parse: F,
        token_type: &'static TokenType,
        error: &'static str,
    ) -> Result<SyntaxNode>
    where
        F: FnOnce(&mut Self) -> Result<SyntaxNode>,
    {
        match self.lexer.peek() {
            Some(Ok(Token { value, .. })) if value == token_type => Ok(parse(self)?),
            Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => Err(Error::Bad(
                error,
                self.lexer.peek().as_ref().unwrap().as_ref().unwrap().source,
            )),
        }
    }

    pub fn parse_one(&mut self) -> Result<Option<SyntaxNode>> {
        let res = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::EOF,
                ..
            })) => Ok(None),
            Some(Ok(_)) => match self.statement() {
                Ok(node) => Ok(Some(node)),
                Err(e) => Err(e),
            },
            Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => panic!("lexer always has a some"),
        };

        res
    }

    pub fn source(&self) -> Vec<char> {
        unsafe { (&*self.buf.as_ptr()).clone() }
    }
}
