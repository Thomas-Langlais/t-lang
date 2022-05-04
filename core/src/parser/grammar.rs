use crate::lexer::{CompType, LogicType, OperationTokenType, Token, TokenType};
use crate::parser::{
    ConditionNode, FactorNode, IfNode, Parser, Result, StatementListNode, StatementNode,
    SyntaxNode, UnaryNode, VariableNode,
};

use super::{BreakNode, ContinueNode, Error, ForNode, WhileNode};

impl<'a> Parser<'a> {
    /// atom = INT|FLOAT|IDENTIFIER
    ///      = LParen expression RParen
    fn atom(&mut self) -> Result<SyntaxNode> {
        match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Int(_) | TokenType::Float(_),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                Ok(SyntaxNode::Factor(FactorNode { token }))
            }
            Some(Ok(Token {
                value: TokenType::Identifier(_),
                ..
            })) => {
                let identifier_token = self.lexer.next().unwrap().unwrap();
                Ok(SyntaxNode::Variable(VariableNode {
                    identifier_token,
                    expression: None,
                    assign: false,
                }))
            }
            Some(Ok(Token {
                value: TokenType::LParen('('),
                ..
            })) => {
                self.lexer.next().unwrap().unwrap();
                let expression = self.expression()?;

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
                Ok(SyntaxNode::Unary(UnaryNode {
                    node: Box::new(factor),
                    op_token,
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
        self.expect_and_consume(&TokenType::LBlock, "expected |-")?;
        // let statements = self.statements()?;

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

        self.expect_and_consume(&TokenType::RBlock, "expected -|")?;
        Ok(SyntaxNode::Statements(StatementListNode { statements }))
    }

    /// if_stmt = KW:IF expr block
    ///             (KW:ELSE KW:IF expr block)* | (KW:ELSE block)?
    fn if_stmt(&mut self) -> Result<SyntaxNode> {
        self.expect_and_consume(&TokenType::Keyword("if"), "expected an 'if' keyword token")?;

        let mut node = IfNode {
            if_nodes: vec![ConditionNode {
                condition: Box::new(self.expr()?),
                statements: Box::new(self.block()?),
            }],
            else_node: None,
        };

        while let Some(Ok(Token {
            value: TokenType::Keyword("else"),
            ..
        })) = self.lexer.peek()
        {
            self.lexer.next();

            if let Some(Ok(Token {
                value: TokenType::Keyword("if"),
                ..
            })) = self.lexer.peek()
            {
                self.lexer.next();
                node.if_nodes.push(ConditionNode {
                    condition: Box::new(self.expr()?),
                    statements: Box::new(self.block()?),
                });

                continue;
            } else {
                node.else_node = Some(Box::new(self.block()?));
                break;
            }
        }

        Ok(SyntaxNode::If(node))
    }

    /// for_stmt = KW:FOR
    ///                 LParen
    ///                     (decl_expr)? LINETERM
    ///                     (expr)? LINETERM
    ///                     (decl_expr)?
    ///                 RParen
    ///            block
    fn for_stmt(&mut self) -> Result<SyntaxNode> {
        self.lexer.next();
        self.expect_and_consume(&TokenType::LParen('('), "expected (")?;
        let declaration = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Keyword("let"),
                ..
            })) => Some(Box::new(self.decl_expr()?)),
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
            })) => Some(Box::new(self.decl_expr()?)),
            _ => None,
        };
        self.expect_and_consume(&TokenType::RParen(')'), "expected )")?;

        let block =
            Box::new(self.expect_and_parse(|p| p.block(), &TokenType::LBlock, "expected |-")?);

        Ok(SyntaxNode::For(ForNode {
            declaration,
            condition,
            increment,
            block,
        }))
    }

    /// while_expr = KW:WHILE expr block
    fn while_stmt(&mut self) -> Result<SyntaxNode> {
        self.lexer.next();
        let condition = Box::new((self.expr())?);
        let block =
            Box::new(self.expect_and_parse(|p| p.block(), &TokenType::LBlock, "expected |-")?);

        Ok(SyntaxNode::While(WhileNode { condition, block }))
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
            Ok(SyntaxNode::Unary(UnaryNode { op_token, node }))
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

    /// decl_expr = KW:LET IDENTIFIER EQ expr
    fn decl_expr(&mut self) -> Result<SyntaxNode> {
        self.lexer.next().unwrap().unwrap();
        let identifier_token = match self.lexer.peek() {
            Some(Ok(Token {
                value: TokenType::Identifier(_),
                ..
            })) => self.lexer.next().unwrap().unwrap(),
            Some(Err(_)) => return Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            Some(Ok(Token {
                value: TokenType::Bad(msg, source),
                ..
            })) => {
                let err = Err(Error::Bad(
                    *msg,
                    *source,
                ));
                self.skip_passed(TokenType::LineTerm)?;
                return err;
            }
            _ => panic!("there should always be a token in the iterator")
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
        let expression = Some(Box::new(expr));
        Ok(SyntaxNode::Variable(VariableNode {
            identifier_token,
            expression,
            assign: true,
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

    /// expression = decl_expr
    ///            = expr
    fn expression(&mut self) -> Result<SyntaxNode> {
        if let Some(Ok(Token {
            value: TokenType::Keyword("let"),
            ..
        })) = self.lexer.peek()
        {
            self.decl_expr()
        } else {
            self.expr()
        }
    }

    /// statement = if_stmt
    ///           = for_stmt
    ///           = while_stmt
    ///           = KW:CONTINUE LINETERM
    ///           = KW:BREAK LINETERM
    ///           = expression LINETERM
    pub fn statement(&mut self) -> Result<SyntaxNode> {
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
                Ok(SyntaxNode::Statement(StatementNode { inner }))
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
                Ok(SyntaxNode::Statement(StatementNode { inner }))
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
                Ok(SyntaxNode::Statement(StatementNode { inner }))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("brk"),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                Ok(SyntaxNode::Break(BreakNode(token)))
            }
            Some(Ok(Token {
                value: TokenType::Keyword("con"),
                ..
            })) => {
                let token = self.lexer.next().unwrap().unwrap();
                self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                Ok(SyntaxNode::Continue(ContinueNode(token)))
            }
            Some(Ok(_)) => {
                let inner = Box::new(self.expression()?);
                self.expect_and_consume(&TokenType::LineTerm, "Expected ';'")?;
                Ok(SyntaxNode::Statement(StatementNode { inner }))
            }
            Some(Err(_)) => Err(Error::Io(self.lexer.next().unwrap().unwrap_err())),
            _ => unreachable!("Infallible since EOF is never parsed"),
        }
    }
}
