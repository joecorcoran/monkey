use object::Object;
use std::collections::btree_map::BTreeMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env {
    bindings: BTreeMap<String, Object>,
    outer: Option<EnvRef>
}

impl Env {
    pub fn new(outer: Option<EnvRef>) -> Env {
	Env {
	    bindings: BTreeMap::<String, Object>::new(),
	    outer: outer
	}
    }

    pub fn env_ref(outer: Option<EnvRef>) -> EnvRef {
	Rc::new(RefCell::new(Self::new(outer)))
    }

    pub fn get(&self, key: &String) -> Option<Object> {
	if let Some(obj) = self.bindings.get(key) {
	    Some(obj.clone())
	} else {
	    match self.outer {
		Some(ref o) => o.borrow().get(key),
		_ => None
	    }
	}
    }

    pub fn set(&mut self, key: String, value: Object) -> Object {
	let ret = value.clone();
	self.bindings.insert(key, value);
	ret
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn child_env_lookup(env: EnvRef) -> Option<Object> {
	let child_one = Env::env_ref(Some(env.clone()));
	child_one.borrow_mut().set(String::from("foo"), Object::Integer(1));

	let child_two = Env::env_ref(Some(env.clone()));
	child_two.borrow_mut().set(String::from("foo"), Object::Integer(10));

	let c = child_two.borrow();
	c.get(&String::from("foo"))
    }

    #[test]
    fn binding() {
	let root = Env::env_ref(None);
	root.borrow_mut().set(String::from("qux"), Object::Integer(99));
	let env = Env::env_ref(Some(root));

	assert_eq!(Some(Object::Integer(99)), env.borrow().get(&String::from("qux")));
	assert_eq!(Some(Object::Integer(10)), child_env_lookup(env));
    }
}
