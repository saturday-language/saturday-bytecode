use crate::object::Obj;
use std::fmt::{write, Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum Value {
  Boolean(bool),
  Number(f64),
  Nil,
  Obj(usize),
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Boolean(b) => write!(f, "{b}"),
      Value::Number(n) => write!(f, "{n}"),
      Value::Nil => write!(f, "nil"),
      Value::Obj(o) => write!(f, "Object[{o}]"),
    }
  }
}

impl Add for Value {
  type Output = Value;

  fn add(self, other: Value) -> Value {
    match (self, other) {
      (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
      _ => panic!("Invalid operation"),
    }
  }
}

impl Sub for Value {
  type Output = Value;

  fn sub(self, other: Value) -> Value {
    match (self, other) {
      (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
      _ => panic!("Invalid operation"),
    }
  }
}

impl Mul for Value {
  type Output = Value;

  fn mul(self, other: Value) -> Value {
    match (self, other) {
      (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
      _ => panic!("Invalid operation"),
    }
  }
}

impl Div for Value {
  type Output = Value;

  fn div(self, other: Value) -> Value {
    match (self, other) {
      (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
      _ => panic!("Invalid operation"),
    }
  }
}

impl Neg for Value {
  type Output = Value;

  fn neg(self) -> Value {
    match self {
      Value::Number(a) => Value::Number(-a),
      _ => panic!("Invalid operation"),
    }
  }
}

impl Value {
  pub fn is_number(&self) -> bool {
    matches!(self, Value::Number(_))
  }

  pub fn is_falsy(&self) -> bool {
    matches!(self, Value::Number(n) if n == &0.0)
      || matches!(self, Value::Nil | Value::Boolean(false))
  }
}

pub struct ValueArray {
  values: Vec<Value>,
  objects: Vec<Obj>,
}

impl ValueArray {
  pub fn new() -> Self {
    Self {
      values: Vec::new(),
      objects: Vec::new(),
    }
  }

  pub fn write(&mut self, value: Value) -> u8 {
    let count = self.values.len() as u8;
    self.values.push(value);
    count
  }

  pub fn make_object_string(&mut self, s: String) -> usize {
    let ret = self.objects.len();
    self.objects.push(Obj::String(s));
    ret
  }

  pub fn print_value(&self, which: usize) {
    print!("{}", self.values[which]);
  }

  pub fn read_value(&self, which: usize) -> Value {
    self.values[which]
  }
}
