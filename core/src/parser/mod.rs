use std::io;
use std::iter::Peekable;

use crate::lexer::{Lexer, Source, Token, TokenType};

mod grammar;

#[derive(Debug)]
pub struct VariableNode {
    pub identifier_token: Token,
    pub expression: Option<Box<SyntaxNode>>,
    pub assign: bool,
    pub source: Source,
}

#[derive(Debug)]
pub struct FactorNode {
    pub token: Token,
    pub source: Source,
}

#[derive(Debug)]
pub struct UnaryNode {
    pub op_token: Token,
    pub node: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct TermNode {
    pub op_token: Token,
    pub left_node: Box<SyntaxNode>,
    pub right_node: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct ConditionNode {
    pub condition: Box<SyntaxNode>,
    pub statements: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct IfNode {
    pub if_nodes: Vec<ConditionNode>,
    pub else_node: Option<Box<SyntaxNode>>,
    pub source: Source,
}

#[derive(Debug)]
pub struct StatementNode {
    pub inner: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct StatementListNode {
    pub statements: Vec<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct ForNode {
    pub declaration: Option<Box<SyntaxNode>>,
    pub condition: Option<Box<SyntaxNode>>,
    pub increment: Option<Box<SyntaxNode>>,
    pub block: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct WhileNode {
    pub condition: Box<SyntaxNode>,
    pub block: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct ContinueNode(Token, Source);

#[derive(Debug)]
pub struct BreakNode(Token, Source);

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

fn get_source(node: &SyntaxNode) -> Source {
    match node {
        SyntaxNode::If(node) => node.source,
        SyntaxNode::Statements(node) => node.source,
        SyntaxNode::Statement(node) => node.source,
        SyntaxNode::For(node) => node.source,
        SyntaxNode::While(node) => node.source,
        SyntaxNode::Continue(ContinueNode(_, source)) => *source,
        SyntaxNode::Break(BreakNode(_, source)) => *source,
        SyntaxNode::Variable(node) => node.source,
        SyntaxNode::Factor(node) => node.source,
        SyntaxNode::Unary(node) => node.source,
        SyntaxNode::Term(node) => node.source,
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

#[derive(Debug)]
pub enum Error {
    Bad(&'static str, Source),
    Io(io::Error),
}

type Result<T> = std::result::Result<T, Error>;

impl<'a> Parser<'a> {
    pub fn from(reader: &'a mut dyn io::Read) -> Self {
        Parser {
            lexer: Lexer::from(reader).peekable(),
        }
    }

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

            let start = get_source(&left).start;
            let end = get_source(&right).end;
            let source = Source::new(start, end);

            left = SyntaxNode::Term(TermNode {
                left_node: Box::new(left),
                right_node: Box::new(right),
                op_token,
                source,
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
    ) -> Result<Token> {
        match self.lexer.peek() {
            Some(Ok(Token { value, .. })) if value == token_type => {
                Ok(self.lexer.next().unwrap().unwrap())
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
}
