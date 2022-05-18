use crate::lexer::{Token, Source};

#[derive(Debug)]
pub struct VariableNode {
    pub identifier_token: Token,
    pub expression: Option<Box<SyntaxNode>>,
    pub assign: bool,
    pub source: Source,
}

#[derive(Debug)]
pub struct FunctionDeclarationNode {
    pub identifier_token: Token,
    pub arguments: Vec<SyntaxNode>,
    pub block: Box<SyntaxNode>,
    pub source: Source,
}

#[derive(Debug)]
pub struct FunctionInvocationNode {
    pub identifier_token: Token,
    pub arguments: Vec<SyntaxNode>,
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
pub struct ContinueNode(pub Token, pub Source);

#[derive(Debug)]
pub struct BreakNode(pub Token, pub Source);

#[derive(Debug)]
pub struct ReturnNode {
    pub token: Token,
    pub return_expression: Option<Box<SyntaxNode>>,
    pub source: Source,
}

#[derive(Debug)]
pub enum SyntaxNode {
    FunctionDeclaration(FunctionDeclarationNode),
    FunctionInvocation(FunctionInvocationNode),
    If(IfNode),
    Statements(StatementListNode),
    Statement(StatementNode),
    For(ForNode),
    While(WhileNode),
    Continue(ContinueNode),
    Break(BreakNode),
    Return(ReturnNode),
    Variable(VariableNode),
    Factor(FactorNode),
    Unary(UnaryNode),
    Term(TermNode),
}

pub (crate) fn get_source(node: &SyntaxNode) -> Source {
    match node {
        SyntaxNode::FunctionDeclaration(node) => node.source,
        SyntaxNode::FunctionInvocation(node) => node.source,
        SyntaxNode::If(node) => node.source,
        SyntaxNode::Statements(node) => node.source,
        SyntaxNode::Statement(node) => node.source,
        SyntaxNode::For(node) => node.source,
        SyntaxNode::While(node) => node.source,
        SyntaxNode::Continue(ContinueNode(_, source)) => *source,
        SyntaxNode::Break(BreakNode(_, source)) => *source,
        SyntaxNode::Return(node) => node.source,
        SyntaxNode::Variable(node) => node.source,
        SyntaxNode::Factor(node) => node.source,
        SyntaxNode::Unary(node) => node.source,
        SyntaxNode::Term(node) => node.source,
    }
}