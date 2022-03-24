use crate::lexer::Token;

pub struct NumberNode {
    token: Token
}

pub struct BinaryOpNode {
    op_token: Token,
    left_node: Option<Box<BinaryOpNode>>,
    right_node: Option<Box<BinaryOpNode>>
}

pub enum NodeType {
    Number(NumberNode),
    Operation(BinaryOpNode)
}

pub struct Parser<'p> {
    tokens: &'p Vec<Token>,
    token_index: usize,
    current_token: Option<&'p Token>
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p Vec<Token>) -> Parser<'p> {
        Parser {
            tokens: tokens,
            token_index: 0,
            current_token: None
        }
    }

    fn advance(&mut self) -> Option<&'p Token> {
        if matches!(self.current_token, Some(_)) {
            self.token_index += 1;
        }

        if self.token_index < self.tokens.len() {
            self.current_token = Some(&self.tokens[self.token_index]);
        } else {
            self.current_token = None;
        }

        self.current_token
    }

    pub fn generate_syntax_tree(&mut self) {
        while let Some(token) = self.advance() {
            println!("{}", token);
        }
    }
}