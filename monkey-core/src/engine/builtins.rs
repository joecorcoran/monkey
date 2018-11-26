use unicode_segmentation::UnicodeSegmentation as US;
use object::Object;

const BUILTINS: &[(&str, usize)] = &[
    ("len", 1)
];

pub fn builtin_find(name: &String) -> Option<Object> {
    if let Some((name, arity)) =  BUILTINS.iter().find(|(n, _)| *n == name.as_str()) {
	Some(Object::Builtin(name.to_string(), *arity))
    } else {
	None
    }
}

pub fn builtin_apply(name: &String, arguments: Vec<Object>) -> Option<Object> {
    match name.as_str() {
	"len" => Some(len(arguments.first().unwrap())),
	_ => None
    }
}

fn len(object: &Object) -> Object {
    match object {
	Object::Str(s) => {
	    Object::Integer(US::graphemes(s.as_str(), true).count() as i32)
	},
	_ => Object::Null
    }
}
