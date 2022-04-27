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
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl IllegalSyntaxError {
    fn new(name: &str, details: &str, location: Source, source: String) -> IllegalSyntaxError {
        IllegalSyntaxError {
            name: name.to_string(),
            details: details.to_string(),
            source: source,
            line: location.start.line,
            start: location.start.column,
            end: location.end.column,
        }
    }

    fn new_invalid_syntax(details: &str, location: Source, source: &[u8]) -> IllegalSyntaxError {
        let src = source.iter().map(|&b| b as char).collect();
        Self::new("Illegal Syntax", details, location, src)
    }
}

impl Display for IllegalSyntaxError {
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

static EXPRESSION_OPS: [TokenType; 2] = [
    TokenType::Operation(OperationTokenType::Arithmetic('+')),
    TokenType::Operation(OperationTokenType::Arithmetic('-')),
];
static TERM_OPS: [TokenType; 2] = [
    TokenType::Operation(OperationTokenType::Arithmetic('*')),
    TokenType::Operation(OperationTokenType::Arithmetic('/')),
];

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

    /// atom = INT|FLOAT|IDENTIFIER
    ///      = LParen expression RParen
    fn atom(&mut self) -> InternalParseResult {
        let current_token = mem::replace(&mut self.current_token, None).unwrap();

        match current_token.value {
            TokenType::Int(_) | TokenType::Float(_) => {
                self.advance();
                let pos = (
                    current_token.source.start.column,
                    current_token.source.end.column,
                );
                let line = current_token.source.start.line;
                Ok(SyntaxNode::Factor(FactorNode {
                    token: current_token,
                    pos,
                    line,
                }))
            }
            TokenType::Identifier(_) => {
                self.advance();
                let pos = (
                    current_token.source.start.column,
                    current_token.source.end.column,
                );
                let line = current_token.source.start.line;
                Ok(SyntaxNode::Variable(VariableNode {
                    identifier_token: current_token,
                    expression: None,
                    assign: false,
                    pos,
                    line,
                }))
            }
            TokenType::LParen('(') => {
                let start = current_token.source.start.column;
                self.advance();

                let result = self.expression();
                if let Err(err) = result {
                    return Err(err);
                }
                let mut expression = unsafe { result.unwrap_unchecked() };

                let token_ref = self.current_token.as_ref().unwrap();
                let token_type = &token_ref.value;
                let location = token_ref.source;

                if token_type == &TokenType::RParen(')') {
                    let end = location.end.column;
                    expression.set_pos((start, end));
                    self.advance();
                    Ok(expression)
                } else {
                    let source = self.source;
                    Err(ParseError::SyntaxError(
                        IllegalSyntaxError::new_invalid_syntax("Expected ')'", location, source),
                    ))
                }
            }
            _ => {
                let location = current_token.source;
                // reset the parsers current token since, it's not a valid factor token
                self.current_token = Some(current_token);
                Err(ParseError::SyntaxError(
                    IllegalSyntaxError::new_invalid_syntax(
                        "Expected an number, variable, or expression",
                        location,
                        self.source,
                    ),
                ))
            }
        }
    }

    /// factor = atom
    ///        = (PLUS|MINUS) factor
    fn factor(&mut self) -> InternalParseResult {
        match self.current_token.as_ref().unwrap().value {
            TokenType::Operation(OperationTokenType::Arithmetic('+'))
            | TokenType::Operation(OperationTokenType::Arithmetic('-')) => {
                let op_token = mem::replace(&mut self.current_token, None).unwrap();
                self.advance();

                let factor_result = self.factor();
                if let Err(err) = factor_result {
                    return Err(err);
                }

                let factor = unsafe { factor_result.unwrap_unchecked() };
                let (_, end) = factor.get_pos();
                let start = op_token.source.start.column;
                let pos = (start, end);
                let line = op_token.source.start.line;
                Ok(SyntaxNode::Unary(UnaryNode {
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
        self.bin_op(func, &TERM_OPS)
    }

    /// arith_expr = term (PLUS|MINUS term)*
    fn arith_expr(&mut self) -> InternalParseResult {
        let func = |parser: &mut Parser| parser.term();
        self.bin_op(func, &EXPRESSION_OPS)
    }

    /// comp_expr = NOT comp_expr
    ///           = arith_expr ((EE|NE|LT|GT|LTE|GTE) arith_expr)*
    fn comp_expr(&mut self) -> InternalParseResult {
        if let Some(Token {
            value: TokenType::Operation(OperationTokenType::Logic(LogicType::NOT)),
            ..
        }) = self.current_token
        {
            let op_token = mem::replace(&mut self.current_token, None).unwrap();
            self.advance();

            let cmp_expr = self.comp_expr();
            if let Err(err) = cmp_expr {
                return Err(err);
            }

            let node = Box::new(unsafe { cmp_expr.unwrap_unchecked() });
            let pos = (op_token.source.start.column, node.get_pos().1);
            let line = op_token.source.start.line;
            Ok(SyntaxNode::Unary(UnaryNode {
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
        {
            if let Some(Token {
                value: TokenType::Keyword("let"),
                ..
            }) = self.current_token
            {
                let let_token = mem::replace(&mut self.current_token, None).unwrap();
                self.advance();

                let identifier_token = match self.current_token {
                    Some(Token {
                        value: TokenType::Identifier(_),
                        ..
                    }) => {
                        let token = mem::replace(&mut self.current_token, None).unwrap();
                        self.advance();
                        token
                    }
                    _ => {
                        let location = match &self.current_token {
                            Some(token) => token.source,
                            None => let_token.source,
                        };
                        return Err(ParseError::SyntaxError(
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
                        self.advance();
                    }
                    _ => {
                        let location = match &self.current_token {
                            Some(token) => token.source,
                            None => identifier_token.source,
                        };
                        return Err(ParseError::SyntaxError(
                            IllegalSyntaxError::new_invalid_syntax(
                                "Expected a variable name",
                                location,
                                self.source,
                            ),
                        ));
                    }
                };

                let expression_result = self.expr();
                if let Err(err) = expression_result {
                    return Err(err);
                }

                let expr = unsafe { expression_result.unwrap_unchecked() };
                let pos = (let_token.source.start.column, expr.get_pos().1);
                let line = let_token.source.start.line;
                let expression = Some(Box::new(expr));
                Ok(SyntaxNode::Variable(VariableNode {
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
    }

    /// statement = expression
    fn statement(&mut self) -> InternalParseResult {
        self.expression()
    }

    /// statements = LINETERM* statement
    ///             (LINETERM+ statement)*
    ///              LINETERM*
    fn statements(&mut self) -> InternalParseResult {
        // LINETERM* statement
        self.skip_line_term();
        let stmt = self.statement()?;
        let pos = stmt.get_pos();
        let statement = Statement {
            inner: Box::new(stmt),
            pos,
            line: 0, // impl the line transfer
        };
        let mut statements = vec![statement];

        statements.append(&mut {
            let mut vec: Vec<Statement> = Vec::new();
            let mut more_statements = true;
            
            // (LINETERM+ statement)*
            loop {
                let (newlines, _) = self.skip_line_term();
                if newlines == 0 {
                    more_statements = false;
                }
                
                if !more_statements {
                    break
                }
                let stmt = match self.statement() {
                    Ok(s) => s,
                    Err(e) => {
                        // because the LINETERM+ is not met, it moves to the next state
                        todo!()
                    }
                };
                let pos = stmt.get_pos();
                let statement = Statement {
                    inner: Box::new(stmt),
                    pos,
                    line: 0, // impl the line transfer
                };

                vec.push(statement);
            }

            vec
        });

        // LINETERM*
        self.skip_line_term();
        let (start, _) = pos;
        let list_pos = (start, statements.last().unwrap().pos.1);
        Ok(SyntaxNode::Statements(StatementList {
            statements,
            pos: list_pos,
        }))
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
