use ast::{Program, Statement, Expression};
use object::Object;
use token::Token;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug, PartialEq)]
pub enum Error {
    Error,
    IllegalOperator,
    NoInput,
    NotImplemented
}

type EvalResult = Result<Object, Error>;

pub trait Eval {
    fn eval(&self) -> EvalResult;
}

impl Eval for Program {
    fn eval(&self) -> EvalResult {
	let mut result = Ok(NULL);
	for s in &self.statements {
	    result = s.eval();
	    if result.is_err() { break }
	}
	result
    }
}

impl Eval for Statement {
    fn eval(&self) -> EvalResult {
	match *self {
	    Statement::Expression { expression: ref e } => e.eval(),
	    Statement::Block { statements: ref ss } => {
		match *ss {
		    Some(ref ss) => {
			let mut result = Ok(NULL);
			for s in ss {
			    result = s.eval();
			    if result.is_err() { break }
			}
			result
		    },
		    None => Ok(NULL)
		}
	    },
	    _ => Err(Error::NotImplemented)
	}
    }
}

impl Eval for Expression {
    fn eval(&self) -> EvalResult {
	match *self {
	    Expression::Integer(i) => Ok(Object::Integer(i as i32)),
	    Expression::Boolean(b) => Ok(if b { TRUE } else { FALSE }),
	    Expression::Prefix { operator: ref o, right: ref r } => eval_prefix(o, r),
	    Expression::Infix { left: ref l, operator: ref o, right: ref r } => eval_infix(l, o, r),
	    Expression::If { condition: ref c, consequence: ref cq, alternative: ref a } => eval_if(c, cq, a),
	    _ => Err(Error::NotImplemented)
	}
    }
}

fn eval_prefix(operator: &Token, right: &Box<Expression>) -> EvalResult {
    let value = right.eval();
    if value.is_err() { return value }
    match *operator {
	Token::Bang => eval_bang_prefix(value.unwrap()),
	Token::Minus => eval_minus_prefix(value.unwrap()),
	_ => Ok(NULL)
    }
}

fn eval_bang_prefix(object: Object) -> EvalResult {
    match object {
	TRUE => Ok(FALSE),
	FALSE => Ok(TRUE),
	NULL => Ok(TRUE),
	_ => Ok(FALSE)
    }
}

fn eval_minus_prefix(object: Object) -> EvalResult {
    match object {
	Object::Integer(i) => Ok(Object::Integer(-i as i32)),
	_ => Err(Error::IllegalOperator)
    }
}

fn eval_infix(left: &Box<Expression>, operator: &Token, right: &Box<Expression>) -> EvalResult {
    let left_value = left.eval();
    if left_value.is_err() { return left_value }
    let right_value = right.eval();
    if right_value.is_err() { return right_value }

    match (left_value.unwrap(), right_value.unwrap()) {
	(Object::Integer(l), Object::Integer(r)) => eval_integer_infix(l, operator, r),
	(Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix(l, operator, r),
	_ => Err(Error::NotImplemented)
    }
}

fn eval_integer_infix(left: i32, operator: &Token, right: i32) -> EvalResult {
    match *operator {
	Token::Plus     => Ok(Object::Integer(left + right)),
	Token::Minus    => Ok(Object::Integer(left - right)),
	Token::Asterisk => Ok(Object::Integer(left * right)),
	Token::Slash    => Ok(Object::Integer(left / right)),
	Token::Equal    => Ok(boolean(left == right)),
	Token::NotEqual => Ok(boolean(left != right)),
	Token::Gt       => Ok(boolean(left > right)),
	Token::Lt       => Ok(boolean(left < right)),
	_               => Err(Error::IllegalOperator)
    }
}

fn eval_boolean_infix(left: bool, operator: &Token, right: bool) -> EvalResult {
    match *operator {
	Token::Equal    => Ok(boolean(left == right)),
	Token::NotEqual => Ok(boolean(left != right)),
	_               => Err(Error::IllegalOperator)
    }
}

fn eval_if(condition: &Box<Expression>, consequence: &Box<Statement>, alternative: &Option<Box<Statement>>) -> EvalResult {
    let condition_value = condition.eval();
    if condition_value.is_err() { return condition_value }
    if truthy(condition_value.unwrap()) {
	consequence.eval()
    } else {
	match *alternative {
	    Some(ref a) => a.eval(),
	    None => Ok(NULL)
	}
    }
}

fn boolean(b: bool) -> Object {
    match b {
	true => TRUE,
	false => FALSE
    }
}

fn truthy(object: Object) -> bool {
    match object {
	FALSE | NULL => false,
	_ => true
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn integer() {
	let program = Program {
	    statements: vec![
		Statement::Expression { expression: Expression::Integer(1) }
	    ]
	};
	let expected = Ok(Object::Integer(1));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn boolean() {
	let program = Program {
	    statements: vec![
		Statement::Expression { expression: Expression::Boolean(false) }
	    ]
	};
	let expected = Ok(Object::Boolean(false));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn bang_prefix() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Prefix {
			operator: Token::Bang,
			right: Box::new(Expression::Boolean(true))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Boolean(false));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn minus_prefix() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Prefix {
			operator: Token::Minus,
			right: Box::new(Expression::Integer(2))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(-2));
	assert_eq!(expected, program.eval());

	let program_err = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Prefix {
			operator: Token::Minus,
			right: Box::new(Expression::Boolean(true))
		    }
		}
	    ]
	};
	let expected_err = Err(Error::IllegalOperator);
	assert_eq!(expected_err, program_err.eval());
    }

    #[test]
    fn integer_infix() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Infix {
			left: Box::new(Expression::Integer(5)),
			operator: Token::Minus,
			right: Box::new(Expression::Integer(2))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(3));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn boolean_infix() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Infix {
			left: Box::new(Expression::Boolean(true)),
			operator: Token::Equal,
			right: Box::new(Expression::Boolean(false))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Boolean(false));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn conditional_if() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::If {
			condition: Box::new(Expression::Boolean(true)),
			consequence: Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Expression { expression: Expression::Integer(10) })
			    ])
			}),
			alternative: None
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(10));
	assert_eq!(expected, program.eval());
    }

    #[test]
    fn conditional_if_else() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::If {
			condition: Box::new(Expression::Boolean(false)),
			consequence: Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Expression { expression: Expression::Integer(10) })
			    ])
			}),
			alternative: Some(Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Expression { expression: Expression::Integer(20) })
			    ])
			}))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(20));
	assert_eq!(expected, program.eval());
    }
}
