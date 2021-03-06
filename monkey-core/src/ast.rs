use token::Token;

pub type Arguments = Option<Vec<Box<Expression>>>;
pub type ArrayElements = Option<Vec<Box<Expression>>>;
pub type Parameters = Option<Vec<Box<Expression>>>;
pub type Statements = Option<Vec<Box<Statement>>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Let { identifier: Expression, expression: Expression },
    Return { expression: Expression },
    Expression { expression: Expression },
    Block { statements: Statements }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
    Str(String),
    Integer(usize),
    Boolean(bool),
    Prefix { operator: Token, right: Box<Expression> },
    Infix { left: Box<Expression>, operator: Token, right: Box<Expression> },
    Index { left: Box<Expression>, index: Box<Expression> },
    If { condition: Box<Expression>, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    Function { parameters: Parameters, body: Box<Statement> },
    Call { function: Box<Expression>, arguments: Arguments },
    Array { elements: ArrayElements }
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
