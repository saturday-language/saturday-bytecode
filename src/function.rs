use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use crate::chunk::Chunk;

pub struct Function {
  arity: usize,
  pub chunk: RefCell<Chunk>,
  name: String,
}


impl PartialOrd for Function {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    panic!("comparing the ord of two functions");
  }
}

impl PartialEq for Function {
  fn eq(&self, other: &Self) -> bool {
    false
  }
}

impl Clone for Function {
  fn clone(&self) -> Self {
    Self {
      arity: self.arity,
      chunk: self.chunk.clone(),
      name: self.name.clone(),
    }
  }
}

impl Display for Function {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!("<fn {}>", self.name)
  }
}

impl Function {
  pub fn new() -> Self {
    Function {
      arity: 0,
      chunk: RefCell::new(Chunk::new()),
      name: "".to_string(),
    }
  }
}