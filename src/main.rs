#![feature(unicode)]

extern crate rustyline;
extern crate std_unicode;
extern crate unicode_segmentation;

mod ast;
mod lexer;
mod parser;
mod token;

use lexer::Lexer;

fn main() {
    let mut rl = rustyline::Editor::<()>::new();
    loop {
	let readline = rl.readline(">> ");
	match readline {
	    Ok(line) => {
		if line == "exit" { break; }
		let mut lexer = Lexer::new(&line);
		println!("{:?}", lexer.tokenize());
	    },
	    Err(_)   => println!("No input"),
	}
    }
}
