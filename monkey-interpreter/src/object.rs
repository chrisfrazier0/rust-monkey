use std::{collections::HashMap, fmt, rc::Rc};

use crate::ast::Function;

#[derive(Debug, Clone)]
pub enum Value {
  Wrap(Object),
  Return(Object),
  Function(Rc<Function>, Environment),
  Error(String),
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Wrap(a), Self::Wrap(b)) => a == b,
      (Self::Return(a), Self::Return(b)) => a == b,
      (Self::Function(f1, e1), Self::Function(f2, e2)) => Rc::ptr_eq(f1, f2) && e1 == e2,
      (Self::Error(a), Self::Error(b)) => a == b,
      _ => false,
    }
  }
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Wrap(obj) => write!(f, "{}", obj),
      Self::Return(obj) => write!(f, "{}", obj),
      Self::Function(func, _) => write!(f, "{}", func),
      Self::Error(str) => write!(f, "ERROR {}", str),
    }
  }
}

impl Value {
  pub fn into_return(self) -> Value {
    match self {
      Self::Wrap(o) => Self::Return(o),
      Self::Return(_) | Self::Function(_, _) | Self::Error(_) => self,
    }
  }

  pub fn unbox(&self) -> Option<&Object> {
    match self {
      Value::Wrap(o) | Value::Return(o) => Some(o),
      Value::Function(_, _) | Value::Error(_) => None,
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

#[derive(PartialEq, Debug, Clone)]
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
