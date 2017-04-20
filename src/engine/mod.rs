use ast::{Program, Statement, Expression};
use object::Object;
use token::Token;

mod env;
pub use self::env::Env;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    IdentifierNotFound(String),
    NotImplemented,
    TypeMismatch(Object, Object),
    UnknownOperator
}

type EvalResult = Result<Object, Error>;

pub trait Eval {
    fn eval(&self, env: &mut Env) -> EvalResult;
}

impl Eval for Program {
    fn eval(&self, env: &mut Env) -> EvalResult {
	let mut result = Ok(NULL);
	for s in &self.statements {
	    result = s.eval(env);
	    match result {
		Err(_) => break,
		Ok(Object::Return(v)) => {
		    result = Ok(*v);
		    break
		},
		_ => continue
	    }
	}
	result
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Env) -> EvalResult {
	match *self {
	    Statement::Expression { expression: ref e } => e.eval(env),
	    Statement::Block { statements: ref ss } => {
		match *ss {
		    Some(ref ss) => {
			let mut result = Ok(NULL);
			for s in ss {
			    result = s.eval(env);
			    match result {
				Ok(Object::Return(_)) | Err(_) => break,
				_ => continue
			    }
			}
			result
		    },
		    None => Ok(NULL)
		}
	    },
	    Statement::Return { expression: ref e } => {
		match e.eval(env) {
		    Ok(v) => Ok(Object::Return(Box::new(v))),
		    err => err
		}
	    },
	    Statement::Let { identifier: ref id, expression: ref e } => {
		if let &Expression::Identifier(ref key) = id {
		    let value = e.eval(env);
		    match value {
			Ok(object) => Ok(env.set(key.to_owned(), object)),
			Err(error) => Err(error)
		    }
		} else {
		    unreachable!()
		}
	    }
	}
    }
}

impl Eval for Expression {
    fn eval(&self, env: &mut Env) -> EvalResult {
	match *self {
	    Expression::Identifier(ref i) => eval_identifier(env, i),
	    Expression::Integer(i) => Ok(Object::Integer(i as i32)),
	    Expression::Boolean(b) => Ok(if b { TRUE } else { FALSE }),
	    Expression::Prefix { operator: ref o, right: ref r } => eval_prefix(env, o, r),
	    Expression::Infix { left: ref l, operator: ref o, right: ref r } => eval_infix(env, l, o, r),
	    Expression::If { condition: ref c, consequence: ref cq, alternative: ref a } => eval_if(env, c, cq, a),
	    _ => Err(Error::NotImplemented)
	}
    }
}

fn eval_identifier(env: &mut Env, name: &String) -> EvalResult {
    if let Some(object) = env.get(name) {
	Ok(object.to_owned())
    } else {
	Err(Error::IdentifierNotFound(name.to_owned()))
    }
}

fn eval_prefix(env: &mut Env, operator: &Token, right: &Box<Expression>) -> EvalResult {
    let value = right.eval(env);
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
	_ => Err(Error::UnknownOperator)
    }
}

fn eval_infix(env: &mut Env, left: &Box<Expression>, operator: &Token, right: &Box<Expression>) -> EvalResult {
    let left_value = left.eval(env);
    if left_value.is_err() { return left_value }
    let right_value = right.eval(env);
    if right_value.is_err() { return right_value }

    match (left_value.unwrap(), right_value.unwrap()) {
	(Object::Integer(l), Object::Integer(r)) => eval_integer_infix(l, operator, r),
	(Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix(l, operator, r),
	(l, r) => Err(Error::TypeMismatch(l, r))
    }
}

fn eval_integer_infix(left: i32, operator: &Token, right: i32) -> EvalResult {
    match *operator {
	Token::Plus     => Ok(Object::Integer(left + right)),
	Token::Minus    => Ok(Object::Integer(left - right)),
	Token::Asterisk => Ok(Object::Integer(left * right)),
	Token::Slash    => {
	    match right {
		0 => Err(Error::DivisionByZero),
		_ => Ok(Object::Integer(left / right))
	    }
	},
	Token::Equal    => Ok(boolean(left == right)),
	Token::NotEqual => Ok(boolean(left != right)),
	Token::Gt       => Ok(boolean(left > right)),
	Token::Lt       => Ok(boolean(left < right)),
	_               => Err(Error::UnknownOperator)
    }
}

fn eval_boolean_infix(left: bool, operator: &Token, right: bool) -> EvalResult {
    match *operator {
	Token::Equal    => Ok(boolean(left == right)),
	Token::NotEqual => Ok(boolean(left != right)),
	_               => Err(Error::UnknownOperator)
    }
}

fn eval_if(env: &mut Env, condition: &Box<Expression>, consequence: &Box<Statement>, alternative: &Option<Box<Statement>>) -> EvalResult {
    let condition_value = condition.eval(env);
    if condition_value.is_err() { return condition_value }
    if truthy(condition_value.unwrap()) {
	consequence.eval(env)
    } else {
	match *alternative {
	    Some(ref a) => a.eval(env),
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
    }

    #[test]
    fn boolean() {
	let program = Program {
	    statements: vec![
		Statement::Expression { expression: Expression::Boolean(false) }
	    ]
	};
	let expected = Ok(Object::Boolean(false));
	assert_eq!(expected, program.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));

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
	let expected_err = Err(Error::UnknownOperator);
	assert_eq!(expected_err, program_err.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
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
	assert_eq!(expected, program.eval(&mut Env::new(None)));
    }

    #[test]
    fn return_statement() {
	let program = Program {
	    statements: vec![
		Statement::Return { expression: Expression::Integer(10) },
		Statement::Expression { expression: Expression::Integer(9) }
	    ]
	};
	let expected = Ok(Object::Integer(10));
	assert_eq!(expected, program.eval(&mut Env::new(None)));
    }

    #[test]
    fn return_statement_bubbling() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::If {
			condition: Box::new(Expression::Boolean(true)),
			consequence: Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Return { expression: Expression::Integer(10) }),
				Box::new(Statement::Expression { expression: Expression::Integer(9) })
			    ])
			}),
			alternative: None
		    }
		},
		Statement::Return { expression: Expression::Integer(8) }
	    ]
	};
	let expected = Ok(Object::Integer(10));
	assert_eq!(expected, program.eval(&mut Env::new(None)));
    }

    #[test]
    fn let_binding() {
	let program = Program {
	    statements: vec![
		Statement::Let {
		    identifier: Expression::Identifier("foo".to_string()),
		    expression: Expression::Integer(10)
		},
		Statement::Expression { expression: Expression::Identifier("foo".to_string()) }
	    ]
	};
	let expected = Ok(Object::Integer(10));
	assert_eq!(expected, program.eval(&mut Env::new(None)));
    }
}
