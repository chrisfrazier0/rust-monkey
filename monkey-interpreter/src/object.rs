use std::{collections::HashMap, fmt};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
  Wrap(Object),
  Return(Object),
  Error(String),
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Wrap(obj) => write!(f, "{}", obj),
      Self::Return(obj) => write!(f, "{}", obj),
      Self::Error(str) => write!(f, "ERROR {}", str),
    }
  }
}

impl Value {
  pub fn into_return(self) -> Value {
    match self {
      Self::Wrap(o) => Self::Return(o),
      Self::Return(_) | Self::Error(_) => self,
    }
  }

  pub fn unbox(&self) -> Option<&Object> {
    match self {
      Value::Wrap(o) | Value::Return(o) => Some(o),
      Value::Error(_) => None,
    }
  }
}

#[derive(PartialEq, Debug, Clone)]
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

pub struct Environment {
  store: HashMap<String, Value>,
}

impl Environment {
  pub fn new() -> Self {
    Environment {
      store: HashMap::new(),
    }
  }

  pub fn insert(&mut self, key: &str, value: Value) {
    self.store.insert(key.to_string(), value);
  }

  pub fn get(&self, key: &str) -> Option<&Value> {
    self.store.get(key)
  }
}
