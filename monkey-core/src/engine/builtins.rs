use unicode_segmentation::UnicodeSegmentation as US;
use object::Object;

const BUILTINS: &[(&str, usize)] = &[
    ("len", 1),
    ("first", 1),
    ("last", 1),
    ("rest", 1),
    ("push", 2)
];

pub fn find(name: &String) -> Option<Object> {
    if let Some((name, arity)) = BUILTINS.iter().find(|(n, _)| *n == name.as_str()) {
	Some(Object::Builtin(name.to_string(), *arity))
    } else {
	None
    }
}

pub fn apply(name: &String, args: Vec<Object>) -> Option<Object> {
    match name.as_str() {
	"len"   => Some(len(args.first().unwrap())),
	"first" => Some(first(args.first().unwrap())),
	"last"  => Some(last(args.first().unwrap())),
	"rest"  => Some(rest(args.first().unwrap())),
	"push"  => Some(push(args.first().unwrap(), args.get(1).unwrap())),
	_ => None
    }
}

fn len(object: &Object) -> Object {
    match object {
	Object::Str(s) => {
	    Object::Integer(US::graphemes(s.as_str(), true).count() as i32)
	},
	Object::Array(elements) => {
	    Object::Integer(elements.len() as i32)
	},
	_ => Object::Null
    }
}

fn first(object: &Object) -> Object {
    match object {
	Object::Array(elements) => {
	    if let Some(e) = elements.get(0) {
		(**e).clone()
	    } else {
		Object::Null
	    }
	},
	_ => Object::Null
    }
}

fn last(object: &Object) -> Object {
    match object {
	Object::Array(elements) => {
	    if let Some(e) = elements.last() {
		(**e).clone()
	    } else {
		Object::Null
	    }
	},
	_ => Object::Null
    }
}

fn rest(object: &Object) -> Object {
    match object {
	Object::Array(elements) => {
	    if let Some((_, rest)) = elements.split_first() {
		Object::Array((*rest).to_vec())
	    } else {
		Object::Null
	    }
	},
	_ => Object::Null
    }
}

fn push(array: &Object, object: &Object) -> Object {
    match array {
	Object::Array(elements) => {
	    let mut new_vec = (*elements).clone();
	    let new_element = (*object).clone();
	    new_vec.push(Box::new(new_element));
	    Object::Array(new_vec)
	},
	_ => Object::Null
    }
}
