use std::collections::btree_map::BTreeMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Env(Env),
    Integer(i32),
    Null,
    Return(Box<Object>)
}

type OuterEnv = Option<Rc<Env>>;

#[derive(Debug, PartialEq, Eq)]
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

    pub fn set(&mut self, key: String, value: Object) -> Option<Object> {
	self.bindings.insert(key, value)
    }
}
