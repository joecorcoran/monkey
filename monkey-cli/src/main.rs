extern crate rustyline;
extern crate monkey_core;

use monkey_core::{Env, Eval, Lexer, Parser};

fn main() {
    // TODO switch on argv between running file/stdin and repl
    repl()
}

fn repl() {
    let mut rl = rustyline::Editor::<()>::new();
    let env = Env::env_ref(None);
    loop {
	let readline = rl.readline(">> ");
	match readline {
	    Ok(line) => {
		if line == "exit" { break; }
		let mut lexer = Lexer::new(&line);
		let mut parser = Parser::new(&mut lexer);
		match parser.parse() {
		    Ok(ast) => {
			let result = ast.eval(env.clone());
			match result {
			    Ok(object) => println!("{}", object),
			    Err(error) => println!("Error: {:?}", error)
			}
		    },
		    Err(error) => println!("Error: {}", error)
		}
	    },
	    Err(_) => println!("No input"),
	}
    }
}
