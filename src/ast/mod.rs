use token::Token;

pub type Parameters = Option<Vec<Box<Expression>>>;
pub type Statements = Vec<Box<Statement>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression, expression: Expression },
    Return { expression: Expression },
    Expression { expression: Expression },
    Block { statements: Statements }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(usize),
    Boolean(bool),
    Prefix { operator: Token, right: Box<Expression> },
    Infix { left: Box<Expression>, operator: Token, right: Box<Expression> },
    If { condition: Box<Expression>, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    Function { parameters: Parameters, body: Box<Statement> }
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
