use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Copy, Clone)]
pub enum Value {
  Boolean(bool),
  Number(f64),
  Nil,
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Boolean(b) => write!(f, "{b}"),
      Value::Number(n) => write!(f, "{n}"),
      Value::Nil => write!(f, "nil"),
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
    matches!(self, Value::Number(n) if n == &0.0) || matches!(self, Value::Nil | Value::Boolean(false))
  }
}

pub struct ValueArray {
  values: Vec<Value>,
}

impl ValueArray {
  pub fn new() -> Self {
    Self { values: Vec::new() }
  }

  pub fn write(&mut self, value: Value) -> u8 {
    let count = self.values.len() as u8;
    self.values.push(value);
    count
  }

  pub fn print_value(&self, which: usize) {
    print!("{}", self.values[which]);
  }

  pub fn read_value(&self, which: usize) -> Value {
    self.values[which]
  }
}
