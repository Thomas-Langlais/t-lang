use super::{TokenType, OperationTokenType, CompType, LogicType};

impl ToString for TokenType {
    fn to_string(&self) -> String {
        match self {
            TokenType::Keyword(word) => word.to_string(),
            TokenType::Identifier(identifier) => identifier.to_string(),
            TokenType::LParen(lp) => lp.to_string(),
            TokenType::RParen(rp) => rp.to_string(),
            TokenType::LBlock => "|-".to_string(),
            TokenType::RBlock => "-|".to_string(),
            TokenType::Operation(op) => op.to_string(),
            TokenType::Int(int) => int.to_string(),
            TokenType::Float(float) => float.to_string(),
            TokenType::LineTerm => ";".to_string(),
            TokenType::EOF => "\\n".to_string(),
        }
    }
}

impl ToString for OperationTokenType {
    fn to_string(&self) -> String {
        match self {
            Self::EQ => "=".to_string(),
            Self::Arithmetic(op) => op.to_string(),
            Self::Comparison(cmp) => cmp.to_string(),
            Self::Logic(lgc) => lgc.to_string(),
        }
    }
}

impl ToString for CompType {
    fn to_string(&self) -> String {
        match self {
            Self::EE => "==".to_string(),
            Self::NE => "!=".to_string(),
            Self::LT => "<".to_string(),
            Self::GT => ">".to_string(),
            Self::LTE => "<=".to_string(),
            Self::GTE => ">=".to_string(),
        }
    }
}

impl ToString for LogicType {
    fn to_string(&self) -> String {
        match self {
            Self::AND => "&&".to_string(),
            Self::OR => "||".to_string(),
            Self::NOT => "!".to_string(),
        }
    }
}