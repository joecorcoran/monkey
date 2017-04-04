use token::Token;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression, expression: Expression },
    Return { expression: Expression },
    Expression { expression: Expression }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(usize),
    Boolean(bool),
    Prefix { operator: Token, right: Box<Expression> },
    Infix { left: Box<Expression>, operator: Token, right: Box<Expression> }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    pub fn new() -> Program {
	Program {
	    statements: vec![]
	}
    }

    pub fn add(&mut self, s: Statement) {
	self.statements.push(s);
    }
}
