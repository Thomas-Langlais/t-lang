use std::mem;

use crate::lexer::{CompType, LogicType, OperationTokenType, Token, TokenType};
use crate::parser::{
    ConditionNode, FactorNode, IfNode, IllegalSyntaxError, InternalParseResult, ParseContext,
    ParseError, Parser, Statement, StatementList, SyntaxNode, UnaryNode, VariableNode,
};

impl<'a> Parser<'a> {
    /// atom = INT|FLOAT|IDENTIFIER
    ///      = LParen expression RParen
    ///      = if_expr
    fn atom(&mut self) -> InternalParseResult {
        let mut context = ParseContext::default();

        match self.current_token {
            Some(Token {
                value: TokenType::Int(_) | TokenType::Float(_),
                source,
            }) => {
                let token = mem::replace(&mut self.current_token, None).unwrap();
                context.advance();
                self.advance();
                let pos = (source.start.column, source.end.column);
                let line = source.start.line;
                context.success(SyntaxNode::Factor(FactorNode { token, pos, line }))
            }
            Some(Token {
                value: TokenType::Identifier(_),
                source,
            }) => {
                let identifier_token = mem::replace(&mut self.current_token, None).unwrap();
                context.advance();
                self.advance();
                let pos = (source.start.column, source.end.column);
                let line = source.start.line;
                context.success(SyntaxNode::Variable(VariableNode {
                    identifier_token,
                    expression: None,
                    assign: false,
                    pos,
                    line,
                }))
            }
            Some(Token {
                value: TokenType::LParen('('),
                source,
            }) => {
                let start = source.start.column;
                context.advance();
                self.advance();

                let mut expression = context.register(self.expression())?;
                let token_ref = self.current_token.as_ref().unwrap();
                let token_type = &token_ref.value;
                let location = token_ref.source;

                if token_type == &TokenType::RParen(')') {
                    let end = location.end.column;
                    expression.set_pos((start, end));
                    context.advance();
                    self.advance();
                    context.success(expression)
                } else {
                    let source = self.source;
                    context.failure(ParseError::SyntaxError(
                        IllegalSyntaxError::new_invalid_syntax("Expected ')'", location, source),
                    ))
                }
            }
            Some(Token {
                value: TokenType::Keyword("if"),
                ..
            }) => context.success(context.register(self.if_expr())?),
            Some(Token { source, .. }) => context.failure(ParseError::SyntaxError(
                IllegalSyntaxError::new_invalid_syntax(
                    "Expected an number, variable, number sign (+/-), or if statement",
                    source,
                    self.source,
                ),
            )),
            None => unreachable!(
                "should not ever reach the end, EOF is always last and is never parser"
            ),
        }
    }

    /// factor = atom
    ///        = (PLUS|MINUS) factor
    fn factor(&mut self) -> InternalParseResult {
        match self.current_token.as_ref().unwrap().value {
            TokenType::Operation(OperationTokenType::Arithmetic('+'))
            | TokenType::Operation(OperationTokenType::Arithmetic('-')) => {
                let mut context = ParseContext::default();

                let op_token = mem::replace(&mut self.current_token, None).unwrap();
                context.advance();
                self.advance();

                let factor = context.register(self.factor())?;
                let (_, end) = factor.get_pos();
                let start = op_token.source.start.column;
                let pos = (start, end);
                let line = op_token.source.start.line;
                context.success(SyntaxNode::Unary(UnaryNode {
                    node: Box::new(factor),
                    op_token,
                    pos,
                    line,
                }))
            }
            _ => self.atom(),
        }
    }

    /// term = factor (MUL|DIV factor)*
    fn term(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.factor();
        self.bin_op(
            func,
            &[
                TokenType::Operation(OperationTokenType::Arithmetic('*')),
                TokenType::Operation(OperationTokenType::Arithmetic('/')),
            ],
        )
    }

    /// LBlock statements RBlock
    fn block(&mut self) -> InternalParseResult {
        let mut context = ParseContext::default();

        match self.current_token {
            Some(Token {
                value: TokenType::LBlock,
                ..
            }) => {}
            Some(Token { source, .. }) => {
                return context.failure(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax("expected |-", source, self.source),
                ));
            }
            _ => unreachable!(
                "should not ever reach the end, EOF is always last and is never parser"
            ),
        };
        self.advance();
        context.advance();

        let statements = context.register(self.statements())?;

        match self.current_token {
            Some(Token {
                value: TokenType::RBlock,
                ..
            }) => {}
            Some(Token { source, .. }) => {
                return context.failure(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax("expected -|", source, self.source),
                ));
            }
            _ => unreachable!(
                "should not ever reach the end, EOF is always last and is never parser"
            ),
        };
        self.advance();
        context.advance();

        context.success(statements)
    }

    /// if-expr = KW:IF expr block
    ///             (KW:ELSE KW:IF expr block)* | (KW:ELSE block)?
    fn if_expr(&mut self) -> InternalParseResult {
        let mut context = ParseContext::default();

        match self.current_token {
            Some(Token {
                value: TokenType::Keyword("if"),
                ..
            }) => {
                self.advance();
                context.advance();
            }
            Some(Token { source, .. }) => {
                return context.failure(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "expected if token",
                        source,
                        self.source,
                    ),
                ));
            }
            _ => unreachable!(
                "should not ever reach the end, EOF is always last and is never parser"
            ),
        };

        let mut node = IfNode {
            if_nodes: vec![ConditionNode {
                condition: Box::new(context.register(self.expr())?),
                statements: Box::new(context.register(self.block())?),
            }],
            else_node: None,
            pos: (0, 0),
        };

        while let Some(Token {
            value: TokenType::Keyword("else"),
            ..
        }) = self.current_token
        {
            self.advance();
            context.advance();

            if let Some(Token {
                value: TokenType::Keyword("if"),
                ..
            }) = self.current_token
            {
                self.advance();
                context.advance();
                node.if_nodes.push(ConditionNode {
                    condition: Box::new(context.register(self.expr())?),
                    statements: Box::new(context.register(self.block())?),
                });

                continue;
            } else {
                node.else_node = Some(Box::new(context.register(self.block())?));
                break;
            }
        }

        context.success(SyntaxNode::If(node))
    }

    /// arith_expr = term (PLUS|MINUS term)*
    fn arith_expr(&mut self) -> InternalParseResult {
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
    fn comp_expr(&mut self) -> InternalParseResult {
        if let Some(Token {
            value: TokenType::Operation(OperationTokenType::Logic(LogicType::NOT)),
            ..
        }) = self.current_token
        {
            let mut context = ParseContext::default();

            let op_token = mem::replace(&mut self.current_token, None).unwrap();
            context.advance();
            self.advance();

            let node = Box::new(context.register(self.comp_expr())?);
            let pos = (op_token.source.start.column, node.get_pos().1);
            let line = op_token.source.start.line;
            context.success(SyntaxNode::Unary(UnaryNode {
                op_token,
                node,
                pos,
                line,
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

    /// expr = comp_expr ((AND|OR) comp_expr)*
    fn expr(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.comp_expr();
        self.bin_op(
            func,
            &[
                TokenType::Operation(OperationTokenType::Logic(LogicType::AND)),
                TokenType::Operation(OperationTokenType::Logic(LogicType::OR)),
            ],
        )
    }

    /// expression = KW:LET IDENTIFIER EQ expr
    ///            = expr
    fn expression(&mut self) -> InternalParseResult {
        let mut context = ParseContext::default();

        if let Some(Token {
            value: TokenType::Keyword("let"),
            ..
        }) = self.current_token
        {
            let let_token = mem::replace(&mut self.current_token, None).unwrap();
            context.advance();
            self.advance();

            let identifier_token = match self.current_token {
                Some(Token {
                    value: TokenType::Identifier(_),
                    ..
                }) => {
                    let token = mem::replace(&mut self.current_token, None).unwrap();
                    context.advance();
                    self.advance();
                    token
                }
                _ => {
                    let location = match &self.current_token {
                        Some(token) => token.source,
                        None => let_token.source,
                    };
                    return context.failure(ParseError::SyntaxError(
                        IllegalSyntaxError::new_invalid_syntax(
                            "Expected a variable name",
                            location,
                            self.source,
                        ),
                    ));
                }
            };

            match self.current_token {
                Some(Token {
                    value: TokenType::Operation(OperationTokenType::EQ),
                    ..
                }) => {
                    context.advance();
                    self.advance();
                }
                _ => {
                    let location = match &self.current_token {
                        Some(token) => token.source,
                        None => identifier_token.source,
                    };
                    return context.failure(ParseError::SyntaxError(
                        IllegalSyntaxError::new_invalid_syntax(
                            "Expected a variable name",
                            location,
                            self.source,
                        ),
                    ));
                }
            };

            let expr = context.register(self.expr())?;
            let pos = (let_token.source.start.column, expr.get_pos().1);
            let line = let_token.source.start.line;
            let expression = Some(Box::new(expr));
            context.success(SyntaxNode::Variable(VariableNode {
                identifier_token,
                expression,
                assign: true,
                pos,
                line,
            }))
        } else {
            self.expr()
        }
    }

    /// statement = expression
    fn statement(&mut self) -> InternalParseResult {
        self.expression()
    }

    /// statements = LINETERM* statement
    ///             (LINETERM+ statement)*
    ///              LINETERM*
    pub fn statements(&mut self) -> InternalParseResult {
        let mut context = ParseContext::default();

        // LINETERM* statement
        self.skip_line_term(&mut context);
        let line = (self.current_token.as_ref().unwrap()).source.start.line;
        let stmt = context.register(self.statement())?;
        let pos = stmt.get_pos();
        let statement = Statement {
            inner: Box::new(stmt),
            pos,
            line, // impl the line transfer
        };
        let mut statements = vec![statement];

        statements.append(&mut {
            let mut vec: Vec<Statement> = Vec::new();

            // (LINETERM+ statement)*
            loop {
                let (newlines, _) = self.skip_line_term(&mut context);
                if newlines == 0 {
                    break;
                }

                let line = (self.current_token.as_ref().unwrap()).source.start.line;
                let stmt = match context.try_register(self.statement()) {
                    Ok(s) => s,
                    Err(Err(_)) => {
                        // because the LINETERM+ is not met, it moves to the next state
                        self.reverse(context.reverse_advances);
                        break;
                    }
                    _ => unreachable!(
                        "Infallible, context.try_register should only wrap errors in an error"
                    ),
                };
                let pos = stmt.get_pos();
                let statement = Statement {
                    inner: Box::new(stmt),
                    pos,
                    line,
                };

                vec.push(statement);
            }

            vec
        });

        // LINETERM*
        self.skip_line_term(&mut context);
        let (start, _) = pos;
        let list_pos = (start, statements.last().unwrap().pos.1);
        context.success(SyntaxNode::Statements(StatementList {
            statements,
            pos: list_pos,
        }))
    }
}
