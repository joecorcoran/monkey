use ast::Statement;
use engine::Env;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Function(Box<Function>),
    Integer(i32),
    Null,
    Return(Box<Object>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub env: Env,
    pub parameters: Option<Vec<String>>,
    pub body: Box<Statement>
}
