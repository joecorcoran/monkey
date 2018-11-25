extern crate rustyline;
extern crate unicode_segmentation;

mod ast;
mod engine;
mod lexer;
mod object;
mod parser;
mod token;

use lexer::Lexer;
use parser::Parser;
use engine::{Env, Eval};

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
