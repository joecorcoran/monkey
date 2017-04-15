use ast::*;
use lexer::Lexer;
use token::Token;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equal,
    LtGt,
    Sum,
    Product,
    Prefix,
    Call
}

pub struct Parser<'a, 'b: 'a> {
    lexer: &'a mut Lexer<'b>,
    errors: Vec<String>
}

impl<'a, 'b> Iterator for Parser<'a, 'b> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
	self.lexer.next_token()
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(lexer: &'a mut Lexer<'b>) -> Parser<'a, 'b> {
        Parser { lexer: lexer, errors: vec![] }
    }

    fn token_precedence(token: &Token) -> Precedence {
	match *token {
	    Token::LParen                  => Precedence::Call,
	    Token::Slash | Token::Asterisk => Precedence::Product,
	    Token::Plus | Token::Minus     => Precedence::Sum,
	    Token::Lt | Token::Gt          => Precedence::LtGt,
	    Token::Equal | Token::NotEqual => Precedence::Equal,
	    _                              => Precedence::Lowest
	}
    }

    fn peek(&mut self) -> Option<Token> {
	self.lexer.peek_token()
    }

    fn peek_error(&mut self, expected: Token) {
	self.errors.push(format!("Expected to find {:?}", expected));
    }

    fn parse_error(&mut self) {
	self.errors.push("Could not parse expression".to_string());
    }

    fn skip_syntax(&mut self, expected: Token) {
	if let Some(token) = self.peek() {
	    if token == expected {
		self.next();
	    } else {
		self.peek_error(expected);
	    }
	}
    }

    fn end_statement(&mut self) {
	if Some(Token::Semicolon) == self.peek() {
	    self.next();
	}
    }

    fn parse(&mut self) -> Result<Program, String> {
	let mut program = Program::new();
	while let Some(statement) = self.parse_statement() {
	    program.add(statement);
	}
	if self.errors.len() > 0 {
	    Err(self.errors.join(", "))
	} else {
	    Ok(program)
	}
    }

    fn parse_statement(&mut self) -> Option<Statement> {
	match self.peek() {
	    Some(Token::Let) => self.parse_statement_let(),
	    Some(Token::Return) => self.parse_statement_return(),
	    Some(_) => self.parse_statement_expression(),
	    None => None
	}
    }

    fn parse_statement_let(&mut self) -> Option<Statement> {
	self.skip_syntax(Token::Let);
	
	if let Some(id) = self.parse_identifier() {
	    self.skip_syntax(Token::Assign);

	    if let Some(expression) = self.parse_expression(Precedence::Lowest) {
		self.end_statement();
		Some(Statement::Let {
		    identifier: id,
		    expression: expression
		})
	    } else {
		self.parse_error();
		None
	    }
	} else {
	    self.peek_error(Token::Identifier("*".to_string()));
	    None
	}

    }

    fn parse_statement_return(&mut self) -> Option<Statement> {
	self.skip_syntax(Token::Return);

	if let Some(expression) = self.parse_expression(Precedence::Lowest) {
	    self.end_statement();
	    Some(Statement::Return { expression: expression })
	} else {
	    self.parse_error();
	    None
	}
    }

    fn parse_statement_expression(&mut self) -> Option<Statement> {
	if let Some(expression) = self.parse_expression(Precedence::Lowest) {
	    self.end_statement();
	    Some(Statement::Expression { expression: expression })
	} else {
	    None
	}
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
	if let Some(mut left) = self.parse_prefix_expression() {
	    while let Some(t) = self.peek() {
		if t == Token::Semicolon { break }
		if precedence >= Self::token_precedence(&t) { break }
		if let Some(infix) = self.parse_infix_expression(Box::new(left.clone())) {
		    left = infix;
		}
	    }
	    Some(left)
	} else {
	    None
	}
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
	match self.peek() {
	    Some(Token::Identifier(_)) => self.parse_identifier(),
	    Some(Token::Integer(_)) => self.parse_integer(),
	    Some(Token::True) | Some(Token::False) => self.parse_boolean(),
	    Some(Token::LParen) => self.parse_group(),
	    Some(Token::If) => self.parse_if(),
	    Some(Token::Function) => self.parse_function(),
	    Some(Token::Bang) | Some(Token::Minus) => {
		let operator = self.next().unwrap();
		if let Some(right) = self.parse_expression(Precedence::Prefix) {
		    Some(Expression::Prefix {
			operator: operator,
			right: Box::new(right)
		    })
		} else {
		    None
		}
	    },
	    _ => None
	}
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Option<Expression> {
	match self.peek() {
	    Some(Token::Plus) | Some(Token::Minus) | Some(Token::Slash) | Some(Token::Asterisk) |
	    Some(Token::Equal) | Some(Token::NotEqual) | Some(Token::Lt) | Some(Token::Gt) => {
		let operator = self.next().unwrap();
		let precedence = Self::token_precedence(&operator);
		if let Some(right) = self.parse_expression(precedence) {
		    Some(Expression::Infix {
			left: left,
			operator: operator,
			right: Box::new(right)
		    })
		} else {
		    None
		}
	    },
	    Some(Token::LParen) => self.parse_call(left),
	    _ => None
	}
    }

    fn parse_call(&mut self, function: Box<Expression>) -> Option<Expression> {
	self.skip_syntax(Token::LParen);

	let mut args: Vec<Box<Expression>> = vec![];
	while let Some(token) = self.peek() {
	    match token {
		Token::RParen => break,
		Token::Comma => {
		    self.next();
		    continue
		},
		_ => {
		    if let Some(arg) = self.parse_expression(Precedence::Lowest) {
			args.push(Box::new(arg));
		    } else {
			self.peek_error(Token::Identifier("*".to_string()));
			return None
		    }
		}
	    }
	}

	self.skip_syntax(Token::RParen);

	Some(Expression::Call {
	    function: function,
	    arguments: if args.is_empty() { None } else { Some(args) }
	})
    }

    fn parse_function(&mut self) -> Option<Expression> {
	self.skip_syntax(Token::Function);

	if Some(Token::LParen) == self.peek() {
	    let parameters = self.parse_parameters();
	    if let Some(block) = self.parse_block() {
		Some(Expression::Function {
		    parameters: parameters,
		    body: Box::new(block)
		})
	    } else {
		None
	    }
	} else {
	    None
	}
    }

    fn parse_parameters(&mut self) -> Parameters {
	self.skip_syntax(Token::LParen);

	let mut parameters: Vec<Box<Expression>> = vec![];
	while let Some(token) = self.peek() {
	    match token {
		Token::RParen => break,
		Token::Comma => {
		    self.next();
		    continue
		},
		_ => {
		    if let Some(parameter) = self.parse_identifier() {
			parameters.push(Box::new(parameter));
		    } else {
			self.peek_error(Token::Identifier("*".to_string()));
			return None
		    }
		}
	    }
	}

	self.skip_syntax(Token::RParen);
	if parameters.is_empty() { None } else { Some(parameters) }
    }

    fn parse_block(&mut self) -> Option<Statement> {
	if Some(Token::LBrace) == self.next() {
	    let mut statements: Vec<Box<Statement>> = vec![];
	    while let Some(token) = self.peek() {
		match token {
		    Token::RBrace => break,
		    _ => {
			if let Some(statement) = self.parse_statement() {
			    statements.push(Box::new(statement));
			}
		    }
		}
	    }
	    self.skip_syntax(Token::RBrace);
	    Some(Statement::Block {
		statements: (if statements.is_empty() { None } else { Some(statements) })
	    })
	} else {
	    None
	}
    }

    fn parse_if(&mut self) -> Option<Expression> {
	self.skip_syntax(Token::If);
	self.skip_syntax(Token::LParen);

	if let Some(condition) = self.parse_expression(Precedence::Lowest) {
	    self.skip_syntax(Token::RParen);    

	    if let Some(consequence) = self.parse_block() {
		self.skip_syntax(Token::Else);
		let alternative = self.parse_block().and_then(|b| Some(Box::new(b)));
		Some(Expression::If {
		    condition: Box::new(condition),
		    consequence: Box::new(consequence),
		    alternative: alternative
		})
	    } else {
		self.parse_error();
		None
	    }
	} else {
	    self.parse_error();
	    None
	}
    }

    fn parse_group(&mut self) -> Option<Expression> {
	self.skip_syntax(Token::LParen);

	if let Some(expression) = self.parse_expression(Precedence::Lowest) {
	    self.skip_syntax(Token::RParen);
	    Some(expression)
	} else {
	    None
	}
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
	if let Some(Token::Identifier(i)) = self.next() {
	    Some(Expression::Identifier(i))
	} else {
	    None
	}
    }

    fn parse_integer(&mut self) -> Option<Expression> {
	if let Some(Token::Integer(i)) = self.next() {
	    Some(Expression::Integer(i.parse().unwrap()))
	} else {
	    None
	}
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
	match self.next() {
	    Some(Token::True) =>  Some(Expression::Boolean(true)),
	    Some(Token::False) => Some(Expression::Boolean(false)),
	    _ => None
	}
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse(input: &str) -> Program {
	let mut lexer = Lexer::new(input);
	let mut parser = Parser::new(&mut lexer);
	let program = parser.parse();
	match program {
	    Ok(p) => p,
	    Err(e) => panic!("Parse error(s): {}", e)
	}
    }

    fn assert_first_statement(program: Program, statement: Statement) {
	let actual = program.statements.get(0);
	if let Some(s) = actual {
	    assert_eq!(statement, *s);
	} else {
	    panic!("Statements empty");
	}
    }

    #[test]
    fn parse_let() {
	let program = parse("let foo = 1;");
	let statement = Statement::Let {
	    identifier: Expression::Identifier("foo".to_string()),
	    expression: Expression::Integer(1)
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn parse_return() {
	let program = parse("return 1;");
	let statement = Statement::Return { expression: Expression::Integer(1) };
	assert_first_statement(program, statement);
    }

    #[test]
    fn parse_identifier() {
	let program = parse("foo;");
	assert_first_statement(program, Statement::Expression { expression: Expression::Identifier("foo".to_string()) });
    }

    #[test]
    fn parse_boolean() {
	let program = parse("true;");
	assert_first_statement(program, Statement::Expression { expression: Expression::Boolean(true) });
    }

    #[test]
    fn prefix_expression() {
	let program = parse("!5;");
	let statement = Statement::Expression {
	    expression: Expression::Prefix { operator: Token::Bang, right: Box::new(Expression::Integer(5)) }
	};
	assert_first_statement(program, statement);

	let program = parse("-5;");
	let statement = Statement::Expression {
	    expression: Expression::Prefix { operator: Token::Minus, right: Box::new(Expression::Integer(5)) }
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn infix_expression() {
	let eq_program = parse("true == false;");
	let eq_statement = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Boolean(true)),
		operator: Token::Equal,
		right: Box::new(Expression::Boolean(false))
	    }
	};
	assert_first_statement(eq_program, eq_statement);
    }

    #[test]
    fn implicit_precedence() {
	let prec_program = parse("5 + 5 / 5;");
	let prec_statement = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Integer(5)),
		operator: Token::Plus,
		right: Box::new(Expression::Infix {
		    left: Box::new(Expression::Integer(5)),
		    operator: Token::Slash,
		    right: Box::new(Expression::Integer(5))
		})
	    }
	};
	assert_first_statement(prec_program, prec_statement);

	let prec_program_2 = parse("5 + 5 + 5;");
	let prec_statement_2 = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Infix {
		    left: Box::new(Expression::Integer(5)),
		    operator: Token::Plus,
		    right: Box::new(Expression::Integer(5))
		}),
		operator: Token::Plus,
		right: Box::new(Expression::Integer(5)) 
	    }
	};
	assert_first_statement(prec_program_2, prec_statement_2);
    }


    #[test]
    fn explicit_precedence() {
	let program = parse("5 + (5 + 5);");
	let statement = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Integer(5)),
		operator: Token::Plus,
		right: Box::new(Expression::Infix {
		    left: Box::new(Expression::Integer(5)),
		    operator: Token::Plus,
		    right: Box::new(Expression::Integer(5))
		}) 
	    }
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn if_expression() {
	let program = parse("if (x > y) { x }");
	let statement = Statement::Expression {
	    expression: Expression::If {
		condition: Box::new(Expression::Infix {
		    left: Box::new(Expression::Identifier("x".to_string())),
		    operator: Token::Gt,
		    right: Box::new(Expression::Identifier("y".to_string()))
		}),
		consequence: Box::new(Statement::Block {
		    statements: Some(vec![
			Box::new(Statement::Expression { expression: Expression::Identifier("x".to_string()) })
		    ])
		}),
		alternative: None
	    }
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn if_else_expression() {
	let program = parse("if (x > y) { x } else { y }");
	let statement = Statement::Expression {
	    expression: Expression::If {
		condition: Box::new(Expression::Infix {
		    left: Box::new(Expression::Identifier("x".to_string())),
		    operator: Token::Gt,
		    right: Box::new(Expression::Identifier("y".to_string()))
		}),
		consequence: Box::new(Statement::Block {
		    statements: Some(vec![
			Box::new(Statement::Expression { expression: Expression::Identifier("x".to_string()) })
		    ])
		}),
		alternative: Some(Box::new(Statement::Block {
		    statements: Some(vec![
			Box::new(Statement::Expression { expression: Expression::Identifier("y".to_string()) })
		    ])
		}))
	    }
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn function_definition() {
	let program = parse("fn(a, b) { a + b; }");
	let statement = Statement::Expression {
	    expression: Expression::Function {
		parameters: Some(vec![
		    Box::new(Expression::Identifier("a".to_string())),
		    Box::new(Expression::Identifier("b".to_string()))
		]),
		body: Box::new(Statement::Block {
		    statements: Some(vec![
			Box::new(Statement::Expression {
			   expression: Expression::Infix {
				left: Box::new(Expression::Identifier("a".to_string())),
				operator: Token::Plus,
				right: Box::new(Expression::Identifier("b".to_string()))
			   }
			})
		    ])
		})
	    }
	};
	assert_first_statement(program, statement);

	let program_no_params = parse("fn() { a + b; }");
	let statement_no_params = Statement::Expression {
	    expression: Expression::Function {
		parameters: None,
		body: Box::new(Statement::Block {
		    statements: Some(vec![
			Box::new(Statement::Expression {
			   expression: Expression::Infix {
				left: Box::new(Expression::Identifier("a".to_string())),
				operator: Token::Plus,
				right: Box::new(Expression::Identifier("b".to_string()))
			   }
			})
		    ])
		})
	    }
	};
	assert_first_statement(program_no_params, statement_no_params);

	let program_no_body = parse("fn() {}");
	let statement_no_body = Statement::Expression {
	    expression: Expression::Function {
		parameters: None,
		body: Box::new(Statement::Block { statements: None })
	    }
	};
	assert_first_statement(program_no_body, statement_no_body);
    }

    #[test]
    fn call() {
	let program = parse("myfunc(foo, 1)");
	let statement = Statement::Expression {
	    expression: Expression::Call {
		function: Box::new(Expression::Identifier("myfunc".to_string())),
		arguments: Some(vec![
		    Box::new(Expression::Identifier("foo".to_string())),
		    Box::new(Expression::Integer(1))
		])
	    }
	};
	assert_first_statement(program, statement);

	let program_no_args = parse("myfunc()");
	let statement_no_args = Statement::Expression {
	    expression: Expression::Call {
		function: Box::new(Expression::Identifier("myfunc".to_string())),
		arguments: None
	    }
	};
	assert_first_statement(program_no_args, statement_no_args);
    }
}

