use ast::*;
use lexer::Lexer;
use token::Token;

pub struct Parser<'a, 'b: 'a> {
    lexer: &'a mut Lexer<'b>
}

impl<'a, 'b> Iterator for Parser<'a, 'b> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
	self.lexer.next_token()
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(lexer: &'a mut Lexer<'b>) -> Parser<'a, 'b> {
        Parser {
            lexer: lexer
        }
    }

    fn parse_let(&mut self, program: &mut Program) {
	if let Some(Token::Identifier(id)) = self.next() {
	    // parse until Token::Semicolon
	    program.add(Statement::Let { identifier: Expression::Identifier(id) }); 
	} else {
	    panic!("Could not parse let statement");
	}
    }

    fn parse_return(&mut self, program: &mut Program) {
	program.add(Statement::Return);
    }

    fn parse(&mut self, program: &mut Program) {
	let token = self.next();
	match token {
		Some(Token::Let) => self.parse_let(program),
		Some(Token::Return) => self.parse_return(program),
		_ => {}
	}
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_let() {
	let mut lexer = Lexer::new("let foo = 1;");
	let mut parser = Parser::new(&mut lexer);

	let mut program = Program::new();
	parser.parse(&mut program);

	let l = program.statements.get(0).unwrap();
	assert_eq!(Statement::Let { identifier: Expression::Identifier("foo".to_string()) }, *l);
    }

    #[test]
    fn test_parse_return() {
	let mut lexer = Lexer::new("return 1;");
	let mut parser = Parser::new(&mut lexer);

	let mut program = Program::new();
	parser.parse(&mut program);

	let l = program.statements.get(0).unwrap();
	assert_eq!(Statement::Return, *l);
    }
}

