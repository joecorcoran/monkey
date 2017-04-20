use object::Object;
use std::collections::btree_map::BTreeMap;
use std::rc::Rc;

type OuterEnv = Option<Rc<Env>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env {
    bindings: BTreeMap<String, Object>,
    outer: OuterEnv
}

impl Env {
    pub fn new(outer: OuterEnv) -> Env {
	Env {
	    bindings: BTreeMap::<String, Object>::new(),
	    outer: outer
	}
    }

    pub fn get(&self, key: &String) -> Option<&Object> {
	if let Some(ref outer) = self.outer {
	    outer.get(key)
	} else {
	    self.bindings.get(key)
	}
    }

    pub fn set(&mut self, key: String, value: Object) -> Object {
	// TODO This is inefficient but not sure how to work around it.
	// Once insert takes ownership of the value, it's only possible
	// to return a reference, which the engine can't work with.
	let ret = value.to_owned();
	self.bindings.insert(key, value);
	ret
    }
}
