use ast::Statement;
use engine::EnvRef;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Function(Box<Function>),
    Integer(i32),
    Null,
    Return(Box<Object>)
}

impl Object {
    pub fn ret(self) -> Object {
	match self {
	    Object::Return(o) => *o,
	    _ => self
	}
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match *self {
	    Object::Boolean(ref b) => write!(f, "{}", b),
	    Object::Function(ref ff) => ff.fmt(f),
	    Object::Integer(ref i) => write!(f, "{}", i),
	    Object::Null => write!(f, "null"),
	    Object::Return(ref o) => o.fmt(f)
	}
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub env: EnvRef,
    pub parameters: Option<Vec<String>>,
    pub body: Box<Statement>
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	if let Some(ref ps) = self.parameters {
	    write!(f, "fn({})", ps.join(", "))
	} else {
	    write!(f, "fn()")
	}
    }
}
