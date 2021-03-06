use ast::{ArrayElements, Arguments, Expression, Parameters, Program, Statement};
use object::{Object, Function};
use token::Token;

mod builtins;

mod env;
pub use self::env::{Env, EnvRef};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug, PartialEq)]
pub enum Error {
    ArgumentsCount(usize, usize),
    DivisionByZero,
    IdentifierNotFound(String),
    IndexNotAllowed,
    IndexNotAnInteger,
    NotAFunction,
    TypeMismatch(Object, Object),
    UnknownOperator
}

type EvalResult = Result<Object, Error>;

pub trait Eval {
    fn eval(&self, env: EnvRef) -> EvalResult;
}

impl Eval for Program {
    fn eval(&self, env: EnvRef) -> EvalResult {
	let mut result = Ok(NULL);
	for s in &self.statements {
	    result = s.eval(env.clone());
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
    fn eval(&self, env: EnvRef) -> EvalResult {
	match *self {
	    Statement::Expression { expression: ref e } => e.eval(env.clone()),
	    Statement::Block { statements: ref ss } => {
		match *ss {
		    Some(ref ss) => {
			let mut result = Ok(NULL);
			for s in ss {
			    result = s.eval(env.clone());
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
		match e.eval(env.clone()) {
		    Ok(v) => Ok(Object::Return(Box::new(v))),
		    err => err
		}
	    },
	    Statement::Let { identifier: ref id, expression: ref e } => {
		if let &Expression::Identifier(ref key) = id {
		    match e.eval(env.clone()) {
			Ok(value) => {
			    Ok(env.borrow_mut().set(key.to_owned(), value))
			},
			Err(e) => Err(e)
		    }
		} else {
		    unreachable!()
		}
	    }
	}
    }
}

impl Eval for Expression {
    fn eval(&self, env: EnvRef) -> EvalResult {
	match *self {
	    Expression::Identifier(ref i) => eval_identifier(env, i),
	    Expression::Str(ref s) => Ok(Object::Str(s.to_owned())),
	    Expression::Integer(i) => Ok(Object::Integer(i as i32)),
	    Expression::Boolean(b) => Ok(if b { TRUE } else { FALSE }),
	    Expression::Prefix { operator: ref o, right: ref r } => eval_prefix(env, o, r),
	    Expression::Infix { left: ref l, operator: ref o, right: ref r } => eval_infix(env, l, o, r),
	    Expression::Index { left: ref l, index: ref i } => eval_index(env, l, i),
	    Expression::If { condition: ref c, consequence: ref cq, alternative: ref a } => eval_if(env, c, cq, a),
	    Expression::Function { parameters: ref p, body: ref b } => eval_function(env, p, b),
	    Expression::Call { function: ref f, arguments: ref a } => eval_call(env, f, a),
	    Expression::Array { elements: ref e } => eval_array(env, e)
	}
    }
}

fn eval_call(env: EnvRef, function_name: &Box<Expression>, arguments: &Arguments) -> EvalResult {
    let args = arguments
		.to_owned()
		.unwrap_or(vec![])
		.iter()
		.map(|a| a.eval(env.clone())
		.unwrap())
		.collect::<Vec<Object>>();

    if let Ok(Object::Function(function)) = function_name.eval(env.clone()) {
	apply_function(&function, &args)
    } else if let Ok(Object::Builtin(name, arity)) = function_name.eval(env.clone()) {
	apply_builtin(&name, arity, &args)
    } else {
	Err(Error::NotAFunction)
    }
}

fn eval_array(env: EnvRef, elements: &ArrayElements) -> EvalResult {
    match elements {
	Some(es) => Ok(Object::Array(es.iter().map(|e| Box::new(e.eval(env.clone()).unwrap())).collect())),
	None => Ok(Object::Array(Vec::new()))
    }
}

fn eval_index(env: EnvRef, left: &Box<Expression>, index: &Box<Expression>) -> EvalResult {
    let id = left.eval(env.clone());
    if id.is_err() { return id; }
    let ix = index.eval(env.clone());
    if ix.is_err() { return ix; }

    match id.unwrap() {
	Object::Array(elements) => {
	    match ix.unwrap() {
		Object::Integer(i) => {
		    if let Some(e) = elements.get(i as usize) {
			Ok((**e).clone())
		    } else {
			Ok(Object::Null)
		    }
		}
		_ => Err(Error::IndexNotAnInteger)
	    }
	},
	_ => Err(Error::IndexNotAllowed)
    }
}

fn apply_builtin(name: &String, arity: usize, arguments: &Vec<Object>) -> EvalResult {
    if arguments.len() != arity {
	return Err(Error::ArgumentsCount(arguments.len(), arity))
    }

    let mut truncated_args = arguments.to_owned();
    truncated_args.truncate(arity);

    match builtins::apply(name, truncated_args) {
	Some(object) => Ok(object),
	None => Err(Error::NotAFunction)
    }
}

fn apply_function(function: &Function, arguments: &Vec<Object>) -> EvalResult {
    match function.parameters {
	Some(ref p) => {
	    if p.len() != arguments.len() {
		return Err(Error::ArgumentsCount(arguments.len(), p.len()));
	    }

	    let mut args = arguments.iter().enumerate();
	    while let Some((i, obj)) = args.next() {
		let key = p.get(i).unwrap().to_owned();
		function.env.borrow_mut().set(key, obj.to_owned());
	    }
	},
	None => {
	    if arguments.len() > 0 { return Err(Error::ArgumentsCount(arguments.len(), 0)); }
	}
    }
    function.body.eval(function.env.clone())
}

fn eval_function(env: EnvRef, parameters: &Parameters, body: &Box<Statement>) -> EvalResult {
    let p = match *parameters {
	Some(ref ps) => {
	    if ps.is_empty() {
		None
	    } else {
		let mut result = vec![];
		let mut iter = ps.iter();
		while let Some(expression) = iter.next() {
		    if let Expression::Identifier(ref i) = **expression {
			result.push(i.to_owned());
		    }
		}
		Some(result)
	    }
	},
	None => None
    };

    Ok(Object::Function(Box::new(Function {
	env: Env::env_ref(Some(env.clone())),
	parameters: p,
	body: body.clone()
    })))
}

fn eval_identifier(env: EnvRef, name: &String) -> EvalResult {
    if let Some(object) = env.borrow().get(name) {
	Ok(object.clone())
    } else if let Some(builtin) = builtins::find(name) {
	Ok(builtin)
    } else {
	Err(Error::IdentifierNotFound(name.to_owned()))
    }
}

fn eval_prefix(env: EnvRef, operator: &Token, right: &Box<Expression>) -> EvalResult {
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

fn eval_infix(env: EnvRef, left: &Box<Expression>, operator: &Token, right: &Box<Expression>) -> EvalResult {
    let left_value = left.eval(env.clone());
    if left_value.is_err() { return left_value }

    let right_value = right.eval(env.clone());
    if right_value.is_err() { return right_value }

    match (left_value.unwrap().ret(), right_value.unwrap().ret()) {
	(Object::Str(l), Object::Str(r)) => eval_string_infix(l, operator, r),
	(Object::Integer(l), Object::Integer(r)) => eval_integer_infix(l, operator, r),
	(Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix(l, operator, r),
	(l, r) => Err(Error::TypeMismatch(l, r))
    }
}

fn eval_string_infix(left: String, operator: &Token, right: String) -> EvalResult {
    match *operator {
	Token::Plus => Ok(Object::Str(format!("{}{}", left, right))),
	_ => Err(Error::UnknownOperator)
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

fn eval_if(env: EnvRef, condition: &Box<Expression>, consequence: &Box<Statement>, alternative: &Option<Box<Statement>>) -> EvalResult {
    let condition_value = condition.eval(env.clone());
    if condition_value.is_err() { return condition_value }
    if truthy(condition_value.unwrap()) {
	consequence.eval(env.clone())
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn boolean() {
	let program = Program {
	    statements: vec![
		Statement::Expression { expression: Expression::Boolean(false) }
	    ]
	};
	let expected = Ok(Object::Boolean(false));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));

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
	assert_eq!(expected_err, program_err.eval(Env::env_ref(None)));
    }

    #[test]
    fn string_infix() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Infix {
			left: Box::new(Expression::Str("hello".to_string())),
			operator: Token::Plus,
			right: Box::new(Expression::Str("!".to_string()))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Str("hello!".to_string()));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn array_literal() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Array {
			elements: Some(vec![
			    Box::new(Expression::Integer(1)),
			    Box::new(Expression::Integer(2))
			])
		    }
		}
	    ]
	};
	let expected = Ok(Object::Array(vec![Box::new(Object::Integer(1)), Box::new(Object::Integer(2))]));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn array_index() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Index {
			left: Box::new(Expression::Array {
			    elements: Some(vec![
				Box::new(Expression::Str("hello".to_string())),
				Box::new(Expression::Str("world".to_string()))
			    ])
			}),
			index: Box::new(Expression::Integer(1))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Str("world".to_string()));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn array_index_oob() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Index {
			left: Box::new(Expression::Array {
			    elements: Some(vec![
				Box::new(Expression::Str("hello".to_string())),
				Box::new(Expression::Str("world".to_string()))
			    ])
			}),
			index: Box::new(Expression::Integer(2))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Null);
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn array_index_left_err() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Index {
			left: Box::new(Expression::Integer(1)),
			index: Box::new(Expression::Integer(2))
		    }
		}
	    ]
	};
	let expected = Err(Error::IndexNotAllowed);
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn array_index_err() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Index {
			left: Box::new(Expression::Array {
			    elements: Some(vec![
				Box::new(Expression::Str("hello".to_string())),
				Box::new(Expression::Str("world".to_string()))
			    ])
			}),
			index: Box::new(Expression::Str("hmm".to_string()))
		    }
		}
	    ]
	};
	let expected = Err(Error::IndexNotAnInteger);
	assert_eq!(expected, program.eval(Env::env_ref(None)));
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
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn function_primitive() {
	let root = Env::env_ref(None);
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Function {
			parameters: None,
			body: Box::new(Statement::Block { statements: None })
		    }
		}
	    ]
	};
	let expected = Ok(Object::Function(Box::new(Function {
	    env: Env::env_ref(Some(root)),
	    parameters: None,
	    body: Box::new(Statement::Block { statements: None })
	})));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn function_call() {
	let program = Program {
	    statements: vec![
		Statement::Let {
		    identifier: Expression::Identifier("foo".to_string()),
		    expression: Expression::Function {
			parameters: Some(vec![
			    Box::new(Expression::Identifier("x".to_string()))
			]),
			body: Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Expression { expression: Expression::Identifier("x".to_string()) })
			    ])
			})
		    }
		},
		Statement::Expression {
		    expression: Expression::Call {
			function: Box::new(Expression::Identifier("foo".to_string())),
			arguments: Some(vec![
			    Box::new(Expression::Integer(10))
			])
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(10));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn builtin_function_call() {
	let program = Program {
	    statements: vec![
		Statement::Expression {
		    expression: Expression::Call {
			function: Box::new(Expression::Identifier("len".to_string())),
			arguments: Some(vec![
			    Box::new(Expression::Str("häää?".to_string()))
			])
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(5));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }

    #[test]
    fn infix_with_function_call() {
	let program = Program {
	    statements: vec![
		Statement::Let {
		    identifier: Expression::Identifier("foo".to_string()),
		    expression: Expression::Function {
			parameters: Some(vec![
			    Box::new(Expression::Identifier("x".to_string()))
			]),
			body: Box::new(Statement::Block {
			    statements: Some(vec![
				Box::new(Statement::Return { expression: Expression::Identifier("x".to_string()) })
			    ])
			})
		    }
		},
		Statement::Expression {
		    expression: Expression::Infix {
			left: Box::new(Expression::Call {
			    function: Box::new(Expression::Identifier("foo".to_string())),
			    arguments: Some(vec![
				Box::new(Expression::Integer(10))
			    ])
			}),
			operator: Token::Minus,
			right: Box::new(Expression::Integer(2))
		    }
		}
	    ]
	};
	let expected = Ok(Object::Integer(8));
	assert_eq!(expected, program.eval(Env::env_ref(None)));
    }
}
