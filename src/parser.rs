use crate::lexer::{Token, TokenType};
use phf::phf_map;

static OPERATOR_PRECEDENCE: phf::Map<char, u8> = phf_map! {
    '*' => 3,
    '/' => 3,
    '+' => 2,
    '-' => 2,
    '(' => 1
};

fn precedence(token_type: &TokenType) -> u8 {
    let ref op_char = match token_type {
        TokenType::Operation(op) => op,
        TokenType::LParen(lp) => lp,
        _ => panic!("Parser should only have operations and left parentheses"),
    };

    OPERATOR_PRECEDENCE[op_char]
}

#[derive(Debug)]
pub struct FactorNode<'p> {
    token: &'p Token,
}

#[derive(Debug)]
pub struct TermNode<'p> {
    op_token: &'p Token,
    left_node: Box<SyntaxNode<'p>>,
    right_node: Box<SyntaxNode<'p>>,
}

#[derive(Debug)]
pub enum SyntaxNode<'p> {
    Factor(FactorNode<'p>),
    Term(TermNode<'p>),
}

pub struct Parser<'p> {
    tokens: &'p Vec<Token>,
    token_index: usize,
    current_token: Option<&'p Token>,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p Vec<Token>) -> Parser<'p> {
        Parser {
            tokens: tokens,
            token_index: 0,
            current_token: None,
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

    pub fn generate_syntax_tree(&mut self) -> SyntaxNode {
        // create the postfix ordered tokens
        let postfix_tokens = {
            let mut result: Vec<&Token> = Vec::new();
            let mut operation_stack: Vec<&Token> = Vec::new();
            
            while let Some(token) = self.advance() {
                match token.value {
                    TokenType::Int(_) | TokenType::Float(_) => {
                        result.push(token);
                    }
                    TokenType::Operation(op) => {
                        while operation_stack.len() > 0
                            && precedence(&operation_stack.last().unwrap().value)
                                >= OPERATOR_PRECEDENCE[&op]
                        {
                            result.push(operation_stack.pop().unwrap());
                        }
                        operation_stack.push(token);
                    }
                    TokenType::LParen('(') => {
                        operation_stack.push(token);
                    }
                    TokenType::RParen(')') => {
                        let mut op_option = operation_stack.pop();
                        let found = loop {
                            match op_option {
                                None => {
                                    break false
                                },
                                Some(Token { value: TokenType::LParen('('), .. }) => {
                                    break true
                                },
                                Some(op_token) => {
                                    result.push(op_token);
                                    op_option = operation_stack.pop();
                                }
                            }
                        };
                        assert!(found, "Parser could not find the closing '(' {:?}", token);
                    }
                    _ => {
                        panic!("Parser cannot understand the grammar {:?}", token.value);
                    }
                };
            }
    
            while operation_stack.len() > 0 {
                let ref token = operation_stack.pop().unwrap();
                assert!(!matches!(token.value, TokenType::LParen(_)), "Unmatched closing parentheses {:?}", token);
                result.push(token);
            }

            result
        };
        let mut token_iter = postfix_tokens.iter();

        // parse the ordered tokens into a syntax tree
        let mut operand_stack: Vec<SyntaxNode> = Vec::new();
        while let Some(ref token) = token_iter.next() {
            match token.value {
                TokenType::Float(_) | TokenType::Int(_) => {
                    operand_stack.push(SyntaxNode::Factor(FactorNode {
                        token
                    }));
                },
                TokenType::Operation(_) => {
                    assert!(operand_stack.len() >= 2, "Unfinished expression/term");

                    let right_node = Box::new(operand_stack.pop().unwrap());
                    let left_node = Box::new(operand_stack.pop().unwrap());

                    operand_stack.push(SyntaxNode::Term(TermNode {
                        op_token: token,
                        left_node: left_node,
                        right_node: right_node
                    }));
                },
                _ => {
                    panic!("Unknown token ({:?}) in postfix ordered tokens", token)
                }
            }
        }
        assert_eq!(operand_stack.len(), 1, "unknown {0:#?} {1:#?}", operand_stack, self.tokens);

        operand_stack.pop().unwrap()
    }
}
