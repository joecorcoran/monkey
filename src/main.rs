#![feature(unicode)]

extern crate rustyline;
extern crate std_unicode;
extern crate unicode_segmentation;

mod ast;
mod lexer;
mod parser;
mod token;

use lexer::Lexer;
use parser::Parser;

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
		    Ok(program) => println!("{:?}", program),
		    Err(error) => println!("Error: {}", error)
		}
	    },
	    Err(_)   => println!("No input"),
	}
    }
}
