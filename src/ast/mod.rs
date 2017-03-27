use token::Token;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression },
    Return
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String)
}

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
