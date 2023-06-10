use crate::chunk::{Chunk, OpCode};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use crate::value::Value;
use crate::InterpretResult;
use std::cell::RefCell;

pub struct Compiler<'a> {
  parser: Parser,
  scanner: Scanner,
  chunk: &'a mut Chunk,
  rules: Vec<ParseRule>,
}

#[derive(Default)]
pub struct Parser {
  current: Token,
  previous: Token,
  had_error: RefCell<bool>,
  panic_mode: RefCell<bool>,
}

#[derive(Copy, Clone)]
struct ParseRule {
  prefix: Option<fn(&mut Compiler)>,
  infix: Option<fn(&mut Compiler)>,
  precedence: Precedent,
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
enum Precedent {
  None = 0,
  Assignment, // =
  Or,         // or
  And,        // and
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // * /
  Unary,      // ! -
  Call,       // . ()
  Primary,
}

impl From<usize> for Precedent {
  fn from(v: usize) -> Self {
    match v {
      0 => Precedent::None,
      1 => Precedent::Assignment,
      2 => Precedent::Or,
      3 => Precedent::And,
      4 => Precedent::Equality,
      5 => Precedent::Comparison,
      6 => Precedent::Term,
      7 => Precedent::Factor,
      8 => Precedent::Unary,
      9 => Precedent::Call,
      10 => Precedent::Primary,
      _ => panic!("cannot convert {v} into Precedence"),
    }
  }
}

impl Precedent {
  /*
  fn previous(&self) -> Self {
    if *self == Precedent::None {
      panic!("no previous before None");
    } else {
      let p = *self as usize;
      (p - 1).into()
    }
  }
   */

  fn next(&self) -> Self {
    if *self == Precedent::Primary {
      panic!("no next after Primary");
    } else {
      let p = *self as usize;
      (p + 1).into()
    }
  }
}

impl<'a> Compiler<'a> {
  pub fn new(chunk: &'a mut Chunk) -> Self {
    // 定义一个长度为NumberOfTokens的数组，初始化值时ParseRule[None]
    let mut rules = vec![
      ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedent::None,
      };
      TokenType::NumberOfTokens as usize
    ];
    rules[TokenType::LeftParen as usize] = ParseRule {
      prefix: Some(|c| c.grouping()),
      infix: None,
      precedence: Precedent::None,
    };
    rules[TokenType::Minus as usize] = ParseRule {
      prefix: Some(|c| c.unary()),
      infix: Some(|c| c.binary()),
      precedence: Precedent::Term,
    };
    rules[TokenType::Plus as usize] = ParseRule {
      prefix: None,
      infix: Some(|c| c.binary()),
      precedence: Precedent::Term,
    };
    rules[TokenType::Slash as usize] = ParseRule {
      prefix: None,
      infix: Some(|c| c.binary()),
      precedence: Precedent::Factor,
    };
    rules[TokenType::Star as usize] = ParseRule {
      prefix: None,
      infix: Some(|c| c.binary()),
      precedence: Precedent::Factor,
    };
    rules[TokenType::Number as usize].prefix = Some(|c| c.number());
    rules[TokenType::False as usize].prefix = Some(|c| c.literal());
    rules[TokenType::True as usize].prefix = Some(|c| c.literal());
    rules[TokenType::Nil as usize].prefix = Some(|c| c.literal());
    rules[TokenType::Bang as usize].prefix = Some(|c| c.unary());

    rules[TokenType::BangEqual as usize] = ParseRule {
      prefix: None,
      infix: Some(|c| c.binary()),
      precedence: Precedent::Equality,
    };
    rules[TokenType::EqualEqual as usize] = rules[TokenType::BangEqual as usize];

    rules[TokenType::Greater as usize] = ParseRule {
      prefix: None,
      infix: Some(|c| c.binary()),
      precedence: Precedent::Comparison,
    };
    rules[TokenType::GreaterEqual as usize] = rules[TokenType::Greater as usize];
    rules[TokenType::Less as usize] = rules[TokenType::Greater as usize];
    rules[TokenType::LessEqual as usize] = rules[TokenType::Greater as usize];

    rules[TokenType::String as usize].prefix = Some(|c| c.string());

    Self {
      parser: Parser::default(),
      scanner: Scanner::new(""),
      chunk,
      rules,
    }
  }

  pub fn compile(&mut self, source: &str) -> Result<(), InterpretResult> {
    self.scanner = Scanner::new(source);
    self.advance();
    self.expression();

    self.consume(TokenType::Eof, "Expect end of expression.");
    self.end_compiler();

    if *self.parser.had_error.borrow() {
      Err(InterpretResult::CompileError)
    } else {
      Ok(())
    }
  }

  fn advance(&mut self) {
    self.parser.previous = self.parser.current.clone();

    loop {
      self.parser.current = self.scanner.scan_token();
      if self.parser.current.t_type != TokenType::Error {
        break;
      }

      let message = self.parser.current.lexeme.as_str();
      self.error_at_current(message);
    }
  }

  fn consume(&mut self, t_type: TokenType, message: &str) {
    if self.parser.current.t_type == t_type {
      self.advance();
      return;
    }

    self.error_at_current(message);
  }

  fn emit_byte(&mut self, byte: u8) {
    self.chunk.write(byte, self.parser.previous.line);
  }

  fn emit_bytes(&mut self, byte1: OpCode, byte2: u8) {
    self.emit_byte(byte1.into());
    self.emit_byte(byte2);
  }

  fn emit_return(&mut self) {
    self.emit_byte(OpCode::Return.into());
  }

  fn make_constant(&mut self, value: Value) -> u8 {
    if let Some(constant) = self.chunk.add_constant(value) {
      constant
    } else {
      self.error("Too many constants in one chunk.");
      0
    }
  }

  fn emit_constant(&mut self, value: Value) {
    let constant = self.make_constant(value);
    self.emit_bytes(OpCode::Constant, constant);
  }

  fn end_compiler(&mut self) {
    self.emit_return();
    #[cfg(feature = "debug_print_code")]
    if !*self.parser.had_error.borrow() {
      self.chunk.disassemble("code");
    }
  }

  fn binary(&mut self) {
    let operator_type = self.parser.previous.t_type;
    let rule = self.rules[operator_type as usize].precedence.next();

    self.parse_precedence(rule);

    match operator_type {
      TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not.into()),
      TokenType::EqualEqual => self.emit_byte(OpCode::Equal.into()),
      TokenType::Greater => self.emit_byte(OpCode::Greater.into()),
      TokenType::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not.into()),
      TokenType::Less => self.emit_byte(OpCode::Less.into()),
      TokenType::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not.into()),
      TokenType::Plus => self.emit_byte(OpCode::Add.into()),
      TokenType::Minus => self.emit_byte(OpCode::Subtract.into()),
      TokenType::Star => self.emit_byte(OpCode::Multiply.into()),
      TokenType::Slash => self.emit_byte(OpCode::Divide.into()),
      _ => todo!(),
    }
  }

  fn literal(&mut self) {
    match self.parser.previous.t_type {
      TokenType::False => self.emit_byte(OpCode::False.into()),
      TokenType::Nil => self.emit_byte(OpCode::Nil.into()),
      TokenType::True => self.emit_byte(OpCode::True.into()),
      _ => unreachable!(),
    }
  }

  fn grouping(&mut self) {
    self.expression();
    self.consume(TokenType::RightParen, "Expect ')' after expression.");
  }

  fn number(&mut self) {
    let value = self.parser.previous.lexeme.parse::<f64>().unwrap();
    self.emit_constant(Value::Number(value));
  }

  fn string(&mut self) {
    let len = self.parser.previous.lexeme.len() - 1;
    let string = self.parser.previous.lexeme[1..len].to_string();
    self.emit_constant(Value::Str(string))
  }

  fn unary(&mut self) {
    let operator_type = self.parser.previous.t_type;

    self.parse_precedence(Precedent::Unary);

    match operator_type {
      TokenType::Minus => self.emit_byte(OpCode::Negate.into()),
      TokenType::Bang => self.emit_byte(OpCode::Not.into()),
      _ => unimplemented!("nope"),
    }
  }

  fn parse_precedence(&mut self, precedent: Precedent) {
    self.advance();
    if let Some(prefix_rule) = self.rules[self.parser.previous.t_type as usize].prefix {
      prefix_rule(self);
      while precedent <= self.rules[self.parser.current.t_type as usize].precedence {
        self.advance();
        if let Some(infix_rule) = self.rules[self.parser.previous.t_type as usize].infix {
          infix_rule(self);
        }
      }
    } else {
      self.error("Expect expression.");
    }
  }

  fn expression(&mut self) {
    self.parse_precedence(Precedent::Assignment);
  }

  fn error_at_current(&self, message: &str) {
    self.error_at(&self.parser.current, message);
  }

  fn error(&self, message: &str) {
    self.error_at(&self.parser.previous, message);
  }

  fn error_at(&self, token: &Token, message: &str) {
    if *self.parser.panic_mode.borrow() {
      return;
    }

    self.parser.panic_mode.replace(true);

    eprint!("[line {}] Error", token.line);

    if token.t_type == TokenType::Eof {
      eprint!(" at end");
    } else if token.t_type == TokenType::Error {
      // ignore
    } else {
      eprint!(" at '{}'", token.lexeme);
    }

    eprintln!(": {message}");
    self.parser.had_error.replace(true);
  }
}
