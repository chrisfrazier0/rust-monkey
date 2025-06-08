use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Object {
  Integer(i32),
  Boolean(bool),
  Null,
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      Object::Integer(i) => write!(f, "{}", i),
      Object::Boolean(b) => write!(f, "{}", b),
      Object::Null => write!(f, "null"),
    }
  }
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
