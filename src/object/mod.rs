#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Integer(i32),
    Null,
    Return(Box<Object>)
}
