extern crate rustyline;
extern crate monkey_core;

use monkey_core::{Env, Eval, Lexer, Parser};
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let argv: Vec<String> = env::args().collect();
    if let Some(path) = argv.get(1) {
	eval_file(path)
    } else {
	repl()
    }
}

fn eval_file(path: &str) -> std::io::Result<()> {
    let mut file = File::open(path)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    let env = Env::env_ref(None);
    let mut lexer = Lexer::new(&code);
    let mut parser = Parser::new(&mut lexer);

    match parser.parse() {
	Ok(ast) => {
	    let result = ast.eval(env.clone());
	    match result {
		Ok(object) => println!("=> {}", object),
		Err(error) => println!("Error: {:?}", error)
	    }
	},
	Err(error) => println!("Error: {}", error)
    }
    Ok(())
}

fn repl() -> std::io::Result<()> {
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
			    Ok(object) => println!("=> {}", object),
			    Err(error) => println!("Error: {:?}", error)
			}
		    },
		    Err(error) => println!("Error: {}", error)
		}
	    },
	    Err(_) => println!("No input"),
	}
    }
    Ok(())
}
