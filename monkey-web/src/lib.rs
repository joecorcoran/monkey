extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

extern crate monkey_core;
use monkey_core::{Env, Eval, Lexer, Object, Parser};

pub trait ToJs {
    fn to_js(&self) -> Result<JsValue, JsValue>;
}

impl ToJs for Object {
    fn to_js(&self) -> Result<JsValue, JsValue> {
	match self {
	    Object::Boolean(b) if b == &true => Ok(JsValue::TRUE),
	    Object::Boolean(b) if b == &false => Ok(JsValue::FALSE),
	    Object::Integer(i) => Ok(JsValue::from(*i)),
	    Object::Str(s) => Ok(JsValue::from(s)),
	    Object::Null => Ok(JsValue::NULL),
	    _ => Err(JsValue::from_str("Cannot convert Monkey object"))
	}
    }
}

#[wasm_bindgen]
pub fn evaluate(input: &str) -> Result<JsValue, JsValue> {
    let env = Env::env_ref(None);
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    match parser.parse() {
	Ok(ast) => {
	    let result = ast.eval(env.clone());
	    match result {
		Ok(object) => object.to_js(),
		Err(error) => Err(JsValue::from_str(&format!("{:?}", error)))
	    }
	},
	Err(error) => Err(JsValue::from_str(&format!("{:?}", error)))
    }
}
