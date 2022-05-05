use std::io;
use std::iter::Peekable;

use crate::lexer::{CompType, Lexer, LogicType, OperationTokenType, Source, Token, TokenType};
use crate::ast::{get_source, SyntaxNode, StatementListNode, StatementNode, IfNode, FactorNode, TermNode, WhileNode, ConditionNode, ContinueNode, BreakNode, VariableNode, ForNode, UnaryNode};

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

    /// atom = INT|FLOAT|IDENTIFIER
    ///      = LParen expr RParen
    fn atom(&mut self) -> Result<SyntaxNode> {
        match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Int(_) | TokenType::Float(_),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                let source = token.source;
                Ok(SyntaxNode::Factor(FactorNode { token, source }))
            }
            Some(Ok(Token {
                value: TokenType::Identifier(_),
                ..
            })) => {
                let identifier_token = self.lexer.next().unwrap().unwrap();
                let source = identifier_token.source;
                Ok(SyntaxNode::Variable(VariableNode {
                    identifier_token,
                    expression: None,
                    assign: false,
                    source,
                }))
            }
            Some(Ok(Token {
                value: TokenType::LParen('('),
                ..
            })) => {
                self.lexer.next().unwrap().unwrap();
                let expression = self.expr()?;

                match self.lexer.peek() {
                    Some(Ok(Token {
                        value: TokenType::RParen(')'),
                        ..
                    })) => {
                        self.lexer.next();
                        Ok(expression)
                    }
                    Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
                    _ => Err(Error::Bad(
                        "Expected ')'",
                        self.lexer.peek().as_ref().unwrap().as_ref().unwrap().source,
                    )),
                }
            }
            Some(Ok(Token {
                value: TokenType::Bad(msg, source),
                ..
            })) => Err(Error::Bad(*msg, *source)),
            Some(Ok(Token { source, .. })) => Err(Error::Bad(
                "Expected an number, variable, number sign (+/-), or if statement",
                *source,
            )),
            _ => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
        }
    }

    /// factor = atom
    ///        = (PLUS|MINUS) factor
    fn factor(&mut self) -> Result<SyntaxNode> {
        match self.lexer.peek() {
            Some(Ok(Token {
                value:
                    TokenType::Operation(OperationTokenType::Arithmetic('+'))
                    | TokenType::Operation(OperationTokenType::Arithmetic('-')),
                ..
            })) => {
                let op_token = self.lexer.next().unwrap().unwrap();
                let factor = self.factor()?;
                let source = Source::new(op_token.source.start, get_source(&factor).end);

                Ok(SyntaxNode::Unary(UnaryNode {
                    node: Box::new(factor),
                    op_token,
                    source,
                }))
            }
            Some(Ok(_)) => self.atom(),
            _ => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
        }
    }

    /// term = factor (MUL|DIV factor)*
    fn term(&mut self) -> Result<SyntaxNode> {
        let func = |parser: &mut Parser| parser.factor();
        self.bin_op(
            func,
            &[
                TokenType::Operation(OperationTokenType::Arithmetic('*')),
                TokenType::Operation(OperationTokenType::Arithmetic('/')),
            ],
        )
    }

    /// LBlock statement+ RBlock
    fn block(&mut self) -> Result<SyntaxNode> {
        let start = self
            .expect_and_consume(&TokenType::LBlock, "expected |-")?
            .source
            .start;

        let statement = self.statement()?;
        let mut statements = vec![statement];

        while !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                value: TokenType::RBlock,
                ..
            }))
        ) {
            statements.push(self.statement()?);
        }

        let end = self
            .expect_and_consume(&TokenType::RBlock, "expected -|")?
            .source
            .end;
        let source = Source::new(start, end);

        Ok(SyntaxNode::Statements(StatementListNode {
            statements,
            source,
        }))
    }

    /// if_stmt = KW:IF expr block
    ///             (KW:ELSE KW:IF expr block)* | (KW:ELSE block)?
    fn if_stmt(&mut self) -> Result<SyntaxNode> {
        let starting_source = self.lexer.next().unwrap().unwrap().source;
        let mut source = starting_source;

        let mut if_nodes = vec![];
        let mut else_node = None;

        let expr = self.expr()?;
        let block = self.block()?;
        source = Source::new(source.start, get_source(&block).end);
        if_nodes.push(ConditionNode {
            condition: Box::new(expr),
            statements: Box::new(block),
            source,
        });

        while let Some(Ok(Token {
            value: TokenType::Keyword("else"),
            ..
        })) = self.lexer.peek()
        {
            source = self.lexer.next().unwrap().unwrap().source;

            if let Some(Ok(Token {
                value: TokenType::Keyword("if"),
                ..
            })) = self.lexer.peek()
            {
                self.lexer.next();
                let expr = self.expr()?;
                let block = self.block()?;
                let source = Source::new(source.start, get_source(&block).end);

                if_nodes.push(ConditionNode {
                    condition: Box::new(expr),
                    statements: Box::new(block),
                    source,
                });
                continue;
            } else {
                let block = self.block()?;
                source = get_source(&block);
                else_node = Some(Box::new(block));
                break;
            }
        }

        Ok(SyntaxNode::If(IfNode {
            if_nodes,
            else_node,
            source: Source::new(starting_source.start, source.end),
        }))
    }

    /// for_stmt = KW:FOR
    ///                 LParen
    ///                     (decl_stmt)? LINETERM
    ///                     (expr)? LINETERM
    ///                     (decl_stmt)?
    ///                 RParen
    ///            block
    fn for_stmt(&mut self) -> Result<SyntaxNode> {
        let start = self.lexer.next().unwrap().unwrap().source.start;
        self.expect_and_consume(&TokenType::LParen('('), "expected (")?;
        let declaration = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Keyword("let"),
                ..
            })) => Some(Box::new(self.decl_stmt()?)),
            _ => None,
        };
        self.expect_and_consume(&TokenType::LineTerm, "expected ;")?;

        let condition = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::LineTerm,
                ..
            })) => None,
            _ => Some(Box::new(self.expr()?)),
        };
        self.expect_and_consume(&TokenType::LineTerm, "expected ;")?;

        let increment = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Keyword("let"),
                ..
            })) => Some(Box::new(self.decl_stmt()?)),
            _ => None,
        };
        self.expect_and_consume(&TokenType::RParen(')'), "expected )")?;

        let block =
            Box::new(self.expect_and_parse(|p| p.block(), &TokenType::LBlock, "expected |-")?);
        let end = get_source(&block).end;
        let source = Source::new(start, end);

        Ok(SyntaxNode::For(ForNode {
            declaration,
            condition,
            increment,
            block,
            source,
        }))
    }

    /// while_expr = KW:WHILE expr block
    fn while_stmt(&mut self) -> Result<SyntaxNode> {
        let start = self.lexer.next().unwrap().unwrap().source.start;

        let condition = Box::new((self.expr())?);
        let block =
            Box::new(self.expect_and_parse(|p| p.block(), &TokenType::LBlock, "expected |-")?);

        let end = get_source(&block).end;
        let source = Source::new(start, end);

        Ok(SyntaxNode::While(WhileNode {
            condition,
            block,
            source,
        }))
    }

    /// arith_expr = term (PLUS|MINUS term)*
    fn arith_expr(&mut self) -> Result<SyntaxNode> {
        let func = |parser: &mut Parser| parser.term();
        self.bin_op(
            func,
            &[
                TokenType::Operation(OperationTokenType::Arithmetic('+')),
                TokenType::Operation(OperationTokenType::Arithmetic('-')),
            ],
        )
    }

    /// comp_expr = NOT comp_expr
    ///           = arith_expr ((EE|NE|LT|GT|LTE|GTE) arith_expr)*
    fn comp_expr(&mut self) -> Result<SyntaxNode> {
        if let Some(Ok(Token {
            value: TokenType::Operation(OperationTokenType::Logic(LogicType::NOT)),
            ..
        })) = self.lexer.peek()
        {
            let op_token = self.lexer.next().unwrap().unwrap();
            let node = Box::new(self.comp_expr()?);

            let source = Source::new(op_token.source.start, get_source(&node).end);
            Ok(SyntaxNode::Unary(UnaryNode {
                op_token,
                node,
                source,
            }))
        } else {
            let func = |parser: &mut Parser| parser.arith_expr();
            self.bin_op(
                func,
                &[
                    TokenType::Operation(OperationTokenType::Comparison(CompType::EE)),
                    TokenType::Operation(OperationTokenType::Comparison(CompType::NE)),
                    TokenType::Operation(OperationTokenType::Comparison(CompType::LT)),
                    TokenType::Operation(OperationTokenType::Comparison(CompType::LTE)),
                    TokenType::Operation(OperationTokenType::Comparison(CompType::GT)),
                    TokenType::Operation(OperationTokenType::Comparison(CompType::GTE)),
                ],
            )
        }
    }

    /// decl_stmt = KW:LET IDENTIFIER EQ expr
    fn decl_stmt(&mut self) -> Result<SyntaxNode> {
        let start = self.lexer.next().unwrap().unwrap().source.start;
        let identifier_token = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Identifier(_),
                ..
            })) => self.lexer.next().unwrap().unwrap(),
            Some(Err(_)) => return Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            Some(Ok(Token {
                value: TokenType::Bad(msg, source),
                ..
            })) => return Err(Error::Bad(*msg, *source)),
            _ => panic!("there should always be a token in the iterator"),
        };

        match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Operation(OperationTokenType::EQ),
                ..
            })) => {
                self.lexer.next();
            }
            Some(Err(_)) => return Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => {
                return Err(Error::Bad(
                    "Expected a variable name",
                    self.lexer.peek().as_ref().unwrap().as_ref().unwrap().source,
                ));
            }
        };

        let expr = self.expr()?;
        let source = Source::new(start, get_source(&expr).end);
        let expression = Some(Box::new(expr));
        Ok(SyntaxNode::Variable(VariableNode {
            identifier_token,
            expression,
            assign: true,
            source,
        }))
    }

    /// expr = comp_expr ((AND|OR) comp_expr)*
    fn expr(&mut self) -> Result<SyntaxNode> {
        let func = |parser: &mut Parser| parser.comp_expr();
        self.bin_op(
            func,
            &[
                TokenType::Operation(OperationTokenType::Logic(LogicType::AND)),
                TokenType::Operation(OperationTokenType::Logic(LogicType::OR)),
            ],
        )
    }

    /// statement = if_stmt
    ///           = for_stmt
    ///           = while_stmt
    ///           = decl_stmt LINETERM
    ///           = KW:CONTINUE LINETERM
    ///           = KW:BREAK LINETERM
    ///           = expr LINETERM
    fn statement(&mut self) -> Result<SyntaxNode> {
        match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Keyword("if"),
                ..
            })) => {
                let result = self.if_stmt();
                if result.is_err() {
                    self.skip_passed(TokenType::RBlock)?;
                }
                let inner = Box::new(result?);
                let source = get_source(&inner);
                Ok(SyntaxNode::Statement(StatementNode { inner, source }))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("for"),
                ..
            })) => {
                let result = self.for_stmt();
                if result.is_err() {
                    self.skip_passed(TokenType::RBlock)?;
                }
                let inner = Box::new(result?);
                let source = get_source(&inner);
                Ok(SyntaxNode::Statement(StatementNode { inner, source }))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("while"),
                ..
            })) => {
                let result = self.while_stmt();
                if result.is_err() {
                    self.skip_passed(TokenType::RBlock)?;
                }
                let inner = Box::new(result?);
                let source = get_source(&inner);
                Ok(SyntaxNode::Statement(StatementNode { inner, source }))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("let"),
                ..
            })) => {
                let result = self.decl_stmt();
                if result.is_err() {
                    self.skip_passed(TokenType::LineTerm)?;
                } else {
                    self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                }
                let inner = Box::new(result?);
                let source = get_source(&inner);
                Ok(SyntaxNode::Statement(StatementNode { inner, source }))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("brk"),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                let source = token.source;
                self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                Ok(SyntaxNode::Break(BreakNode(token, source)))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("con"),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                let source = token.source;
                self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                Ok(SyntaxNode::Continue(ContinueNode(token, source)))
            }
            Some(Ok(_)) => {
                let result = self.expr();
                if result.is_err() {
                    self.skip_passed(TokenType::LineTerm)?;
                } else {
                    self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                }
                let inner = Box::new(result?);
                let source = get_source(&inner);
                Ok(SyntaxNode::Statement(StatementNode { inner, source }))
            }
            Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => unreachable!("Infallible since EOF is never parsed"),
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
