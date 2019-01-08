extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

extern crate js_sys;

extern crate monkey_core;
use monkey_core::{Env, Eval, Lexer, Object, Parser};

trait TryIntoJs {
    fn try_into_js(&self) -> Result<JsValue, JsValue>;
}

impl TryIntoJs for Object {
    fn try_into_js(&self) -> Result<JsValue, JsValue> {
	let value = match self {
	    Object::Array(elements) => {
		let arr = js_sys::Array::new();
		for e in elements {
		    if let Ok(v) = &e.try_into_js() {
			arr.push(&v);
		    } else {
			return Err("Cannot convert Monkey array member into JavaScript value".into());
		    }
		}
		arr.into()
	    },
	    Object::Boolean(b) => if *b { JsValue::TRUE } else { JsValue::FALSE },
	    Object::Integer(i) => (*i).into(),
	    Object::Str(s) => s.into(),
	    Object::Null => JsValue::NULL,
	    _ => {
		return Err("Cannot convert Monkey object into JavaScript value".into());
	    }
	};
	Ok(value)
    }
}

fn transfer(value: JsValue) -> js_sys::Object {
    let data = js_sys::Object::new();
    js_sys::Reflect::set(&data, &"writable".into(), &false.into()).unwrap();
    js_sys::Reflect::set(&data, &"value".into(), &js_sys::Object::from(value)).unwrap();
    let obj = js_sys::Object::new();
    js_sys::Reflect::set(&obj, &"result".into(), &data).unwrap();
    data
}

#[wasm_bindgen]
pub fn evaluate(input: &str) -> Result<js_sys::Object, JsValue> {
    let env = Env::env_ref(None);
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    match parser.parse() {
	Ok(ast) => {
	    let result = ast.eval(env.clone());
	    match result {
		Ok(object) => object.try_into_js().and_then(|value| Ok(transfer(value))),
		Err(error) => Err(JsValue::from_str(&format!("{:?}", error)))
	    }
	},
	Err(error) => Err(JsValue::from_str(&format!("{:?}", error)))
    }
}
