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

    fn peek(&mut self) -> Option<Token> {
	self.lexer.peek_token()
    }

    fn peek_error(&mut self, expected: &str, actual: Token) {
	self.errors.push(format!("[peek error] expected {:?}, got {:?}", expected, actual));
    }

    fn parse_statement(&mut self, program: &mut Program) {
	let token = self.peek();
	match token {
	    Some(Token::Let) => self.parse_statement_let(program),
	    Some(Token::Return) => self.parse_statement_return(program),
	    Some(_) => self.parse_statement_expression(program),
	    None => {}
	}
    }

    fn parse_statement_let(&mut self, program: &mut Program) {
	self.next(); // Skip keyword
	
	// TODO refactor by de-nesting
	if let Some(Token::Identifier(id)) = self.next() {
	    if let Some(Token::Assign) = self.peek() {
		self.next(); // Skip assign
		if let Some(expression) = self.parse_expression(Precedence::Lowest) {
		    program.add(Statement::Let {
			identifier: Expression::Identifier(id),
			expression: expression
		    }); 
		} else {
		    let token = self.next().unwrap();
		    self.peek_error("expression", token);
		}
	    } else {
		let token = self.next().unwrap();
		self.peek_error("assign", token);
	    }
	} else {
	    let token = self.next().unwrap();
	    self.peek_error("identifier", token);
	}
	// TODO take until semicolon
    }

    fn parse_statement_return(&mut self, program: &mut Program) {
	self.next(); // Skip keyword
	if let Some(expression) = self.parse_expression(Precedence::Lowest) {
	    program.add(Statement::Return { expression: expression });
	} else {
	    self.peek_error("expression", Token::EOF);
	}
	// TODO take until semicolon
    }

    fn parse_statement_expression(&mut self, program: &mut Program) {
	if let Some(e) = self.parse_expression(Precedence::Lowest) {
	    program.add(Statement::Expression { expression: e });
	}
	if self.peek() == Some(Token::Semicolon) { self.next(); }
    }

    fn token_precedence(token: &Token) -> Precedence {
	match *token {
	    Token::Equal | Token::NotEqual => Precedence::Equal,
	    Token::Lt | Token::Gt          => Precedence::LtGt,
	    Token::Plus | Token::Minus     => Precedence::Sum,
	    Token::Slash | Token::Asterisk => Precedence::Product,
	    Token::LParen                  => Precedence::Call,
	    _                              => Precedence::Lowest
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
	// ident int bang minus true false lparen if fn
	match self.peek() {
	    Some(Token::Identifier(_)) => self.parse_identifier(),
	    Some(Token::Integer(_)) => self.parse_integer(),
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
	// plus minus slash asterisk eq noteq lt gt lparen
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
	    _ => None
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
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_statement(input: &str) -> Program {
	let mut lexer = Lexer::new(input);
	let mut parser = Parser::new(&mut lexer);
	let mut program = Program::new();
	parser.parse_statement(&mut program);
	if parser.errors.len() > 0 {
	    panic!("Parser error: {:?}", parser.errors);
	}
	program
    }

    fn assert_first_statement(program: Program, statement: Statement) {
	let actual = program.statements.get(0).unwrap();
	assert_eq!(statement, *actual);	
    }

    #[test]
    fn parse_let() {
	let program = parse_statement("let foo = 1;");
	let statement = Statement::Let {
	    identifier: Expression::Identifier("foo".to_string()),
	    expression: Expression::Integer(1)
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn parse_return() {
	let program = parse_statement("return 1;");
	let statement = Statement::Return { expression: Expression::Integer(1) };
	assert_first_statement(program, statement);
    }

    #[test]
    fn parse_identifier() {
	let program = parse_statement("foo;");
	assert_first_statement(program, Statement::Expression { expression: Expression::Identifier("foo".to_string()) });
    }

    #[test]
    fn prefix_expression() {
	let program = parse_statement("!5;");
	let statement = Statement::Expression {
	    expression: Expression::Prefix { operator: Token::Bang, right: Box::new(Expression::Integer(5)) }
	};
	assert_first_statement(program, statement);

	let program = parse_statement("-5;");
	let statement = Statement::Expression {
	    expression: Expression::Prefix { operator: Token::Minus, right: Box::new(Expression::Integer(5)) }
	};
	assert_first_statement(program, statement);
    }

    #[test]
    fn infix_expression() {
	let plus_program = parse_statement("5 + 5;");
	let plus_statement = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Integer(5)),
		operator: Token::Plus,
		right: Box::new(Expression::Integer(5))
	    }
	};
	assert_first_statement(plus_program, plus_statement);

	let eq_program = parse_statement("5 == 5;");
	let eq_statement = Statement::Expression {
	    expression: Expression::Infix {
		left: Box::new(Expression::Integer(5)),
		operator: Token::Equal,
		right: Box::new(Expression::Integer(5))
	    }
	};
	assert_first_statement(eq_program, eq_statement);

	let prec_program = parse_statement("5 + 5 / 5;");
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
    }
}

