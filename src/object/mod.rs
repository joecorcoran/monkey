#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null
}
