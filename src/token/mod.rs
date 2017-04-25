#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Integer(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Equal,
    NotEqual,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equal,
    LtGt,
    Sum,
    Product,
    Prefix,
    Call
}

pub fn lookup<S: Into<String>>(identifier: S) -> Token {
    let id = identifier.into();
    match id.as_ref() {
        "=" => Token::Assign,
        "+" => Token::Plus,
        "-" => Token::Minus,
        "!" => Token::Bang,
        "*" => Token::Asterisk,
        "/" => Token::Slash,

        "<" => Token::Lt,
        ">" => Token::Gt,

        "==" => Token::Equal,
        "!=" => Token::NotEqual,

        "," => Token::Comma,
        ";" => Token::Semicolon,

        "(" => Token::LParen,
        ")" => Token::RParen,
        "{" => Token::LBrace,
        "}" => Token::RBrace,
        
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,

        _ => Token::Identifier(id)
    }
}

pub fn precedence(token: &Token) -> Precedence {
    match *token {
        Token::LParen                  => Precedence::Call,
        Token::Slash | Token::Asterisk => Precedence::Product,
        Token::Plus | Token::Minus     => Precedence::Sum,
        Token::Lt | Token::Gt          => Precedence::LtGt,
        Token::Equal | Token::NotEqual => Precedence::Equal,
        _                              => Precedence::Lowest
    }
}
