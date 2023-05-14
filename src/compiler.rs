use std::cell::RefCell;
use crate::chunk::{Chunk, OpCode};
use crate::InterpretResult;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};

pub struct Compiler<'a> {
  parser: Parser,
  scanner: Scanner,
  chunk: &'a mut Chunk,
}

#[derive(Default)]
pub struct Parser {
  current: Token,
  previous: Token,
  had_error: RefCell<bool>,
  panic_mode: RefCell<bool>,
}

impl<'a> Compiler<'a> {
  pub fn new(chunk: &'a mut Chunk) -> Self {
    Self {
      parser: Parser::default(),
      scanner: Scanner::new(""),
      chunk,
    }
  }

  pub fn compile(&mut self, source: &str) -> Result<(), InterpretResult> {
    self.scanner = Scanner::new(source);
    self.advance();
    // self.expression()?;

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

  fn end_compiler(&mut self) {
    self.emit_return();
  }

  fn expression(&mut self) {
    
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
