#![feature(unicode)]

extern crate rustyline;
extern crate std_unicode;
extern crate unicode_segmentation;

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod token;

use lexer::Lexer;
use parser::Parser;
use eval::Eval;

fn main() {
    let mut rl = rustyline::Editor::<()>::new();
    loop {
	let readline = rl.readline(">> ");
	match readline {
	    Ok(line) => {
		if line == "exit" { break; }
		let mut lexer = Lexer::new(&line);
		let mut parser = Parser::new(&mut lexer);
		match parser.parse() {
		    Ok(program) => println!("{:?}", program.eval()),
		    Err(error) => println!("Error: {}", error)
		}
	    },
	    Err(_)   => println!("No input"),
	}
    }
}
