use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Debug)]
pub struct Function {
  arity: usize,
  pub chunk: Rc<Chunk>,
  name: String,
}


impl PartialOrd for Function {
  fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
    panic!("comparing the ord of two functions");
  }
}

impl PartialEq for Function {
  fn eq(&self, _other: &Self) -> bool {
    todo!()
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
    if self.name.is_empty() {
      write!(f, "<script>")
    } else {
      write!(f, "<fn {}>", self.name)
    }
  }
}

impl Function {
  pub fn new(chunk: &Rc<Chunk>) -> Self {
    Function {
      arity: 0,
      chunk: Rc::clone(chunk),
      name: "".to_string(),
    }
  }

  pub fn get_chunk(&self) -> Rc<Chunk> {
    Rc::clone(&self.chunk)
  }
}