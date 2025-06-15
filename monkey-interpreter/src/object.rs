use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Value {
  Wrap(Object),
  Return(Object),
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Wrap(obj) => write!(f, "{}", obj),
      Value::Return(obj) => write!(f, "{}", obj),
    }
  }
}

impl Value {
  pub fn into_return(self) -> Value {
    match self {
      Self::Wrap(o) => Self::Return(o),
      Self::Return(_) => self,
    }
  }

  pub fn unbox(&self) -> &Object {
    match self {
      Value::Wrap(o) | Value::Return(o) => o,
    }
  }
}

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
