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
  rules: Vec<ParseRule<'a>>,
  locals: RefCell<Vec<Local>>,
  scope_depth: usize,
}

#[derive(Default)]
pub struct Parser {
  current: Token,
  previous: Token,
  had_error: RefCell<bool>,
  panic_mode: RefCell<bool>,
}

#[derive(Copy, Clone)]
struct ParseRule<'a> {
  prefix: Option<fn(&mut Compiler<'a>, bool)>,
  infix: Option<fn(&mut Compiler<'a>, bool)>,
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

struct Local {
  name: Token,
  depth: Option<usize>,
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
  pub const MAX: usize = u16::MAX as usize;

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

    rules[TokenType::LeftParen as usize].prefix = Some(Compiler::grouping);

    rules[TokenType::Minus as usize] = ParseRule {
      prefix: Some(Compiler::unary),
      infix: Some(Compiler::binary),
      precedence: Precedent::Term,
    };
    rules[TokenType::Plus as usize] = ParseRule {
      prefix: None,
      infix: Some(Compiler::binary),
      precedence: Precedent::Term,
    };
    rules[TokenType::Slash as usize] = ParseRule {
      prefix: None,
      infix: Some(Compiler::binary),
      precedence: Precedent::Factor,
    };
    rules[TokenType::Star as usize] = ParseRule {
      prefix: None,
      infix: Some(Compiler::binary),
      precedence: Precedent::Factor,
    };
    rules[TokenType::Number as usize].prefix = Some(Compiler::number);
    rules[TokenType::False as usize].prefix = Some(Compiler::literal);
    rules[TokenType::True as usize].prefix = Some(Compiler::literal);
    rules[TokenType::Nil as usize].prefix = Some(Compiler::literal);
    rules[TokenType::Bang as usize].prefix = Some(Compiler::unary);

    rules[TokenType::BangEqual as usize] = ParseRule {
      prefix: None,
      infix: Some(Compiler::binary),
      precedence: Precedent::Equality,
    };
    rules[TokenType::EqualEqual as usize] = rules[TokenType::BangEqual as usize];

    rules[TokenType::Greater as usize] = ParseRule {
      prefix: None,
      infix: Some(Compiler::binary),
      precedence: Precedent::Comparison,
    };
    rules[TokenType::GreaterEqual as usize] = rules[TokenType::Greater as usize];
    rules[TokenType::Less as usize] = rules[TokenType::Greater as usize];
    rules[TokenType::LessEqual as usize] = rules[TokenType::Greater as usize];

    rules[TokenType::String as usize].prefix = Some(Compiler::string);
    rules[TokenType::Identifier as usize].prefix = Some(Compiler::variable);

    rules[TokenType::And as usize].infix = Some(Compiler::and);
    rules[TokenType::And as usize].precedence = Precedent::And;

    rules[TokenType::Or as usize].infix = Some(Compiler::or);
    rules[TokenType::Or as usize].precedence = Precedent::Or;

    Self {
      parser: Parser::default(),
      scanner: Scanner::new(""),
      chunk,
      rules,
      locals: RefCell::new(Vec::new()),
      scope_depth: 0,
    }
  }

  pub fn compile(&mut self, source: &str) -> Result<(), InterpretResult> {
    self.scanner = Scanner::new(source);
    self.advance();

    while !self.is_match(TokenType::Eof) {
      self.declaration();
    }

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

  /// 表达式结尾 必须换行
  fn consume_statement_end(&mut self) {
    if vec![TokenType::Eof, TokenType::Wrap, TokenType::SemiColon].contains(&self.parser.current.t_type) {
      self.advance();
      return;
    }

    if self.check(TokenType::RightBrace) {
      return;
    }

    self.error_at_current("Expect Wrap after value.");
  }

  fn check(&self, t_type: TokenType) -> bool {
    self.parser.current.t_type == t_type
  }

  fn is_match(&mut self, t_type: TokenType) -> bool {
    if self.check(t_type) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn emit_byte(&mut self, byte: u8) {
    self.chunk.write(byte, self.parser.previous.line);
  }

  fn emit_bytes(&mut self, byte1: OpCode, byte2: u8) {
    self.emit_byte(byte1.into());
    self.emit_byte(byte2);
  }

  fn emit_loop(&mut self, loop_start: usize) {
    self.emit_byte(OpCode::Loop.into());

    let offset = self.chunk.count() + 2 - loop_start;
    if offset > u16::MAX as usize {
      self.error("Loop body too large.");
    }

    self.emit_byte(((offset >> 8) & 0xff) as u8);
    self.emit_byte((offset & 0xff) as u8);
  }

  /// 在需要被跳过的指令之前插入 需要在解析完要被跳过的指令之后使用patch_jump方法存储要跳转到的位置
  /// 在真正执行时拿到该位置再进行跳转，从而跳过emit_jump和patch_jump之间的指令
  fn emit_jump(&mut self, instruction: OpCode) -> usize {
    self.emit_byte(instruction.into());
    self.emit_byte(0xff);
    self.emit_byte(0xff);
    self.chunk.count() - 2
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

  /// 在此方法之前会插入Jump或者JmpIfFalse指令，当插入这类指令后开始解析可能需要被跳过的指令
  /// 此时堆栈中就能够获取jump或者jumpIfFalse指令需要跳过的距离，然后再通过这个patch_jump方法
  /// 添加一个要跳转到的位置，到时候vm在执行到jump或者jumpIfFalse的时候就可以直接拿到这个要跳转的位置
  /// 直接修改offset的值即可完成跳转
  fn patch_jump(&mut self, offset: usize) {
    let jump = self.chunk.count() - offset - 2;

    if jump > Self::MAX {
      self.error("Too much code to jump over.");
    }

    self.chunk.write_at(offset, ((jump >> 8) & 0xff) as u8);
    self.chunk.write_at(offset + 1, (jump & 0xff) as u8);
  }

  fn end_compiler(&mut self) {
    self.emit_return();
    #[cfg(feature = "debug_print_code")]
    if !*self.parser.had_error.borrow() {
      self.chunk.disassemble("code");
    }
  }

  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn end_scope(&mut self) {
    self.scope_depth -= 1;

    while self.locals.borrow().len() > 0
      && self.locals.borrow().last().unwrap().depth.unwrap() > self.scope_depth
    {
      self.emit_byte(OpCode::Pop.into());
      self.locals.borrow_mut().pop();
    }
  }

  fn binary(&mut self, _: bool) {
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

  fn literal(&mut self, _: bool) {
    match self.parser.previous.t_type {
      TokenType::False => self.emit_byte(OpCode::False.into()),
      TokenType::Nil => self.emit_byte(OpCode::Nil.into()),
      TokenType::True => self.emit_byte(OpCode::True.into()),
      _ => unreachable!(),
    }
  }

  fn grouping(&mut self, _: bool) {
    self.expression();
    self.consume(TokenType::RightParen, "Expect ')' after expression.");
  }

  fn number(&mut self, _: bool) {
    let value = self.parser.previous.lexeme.parse::<f64>().unwrap();
    self.emit_constant(Value::Number(value));
  }

  fn or(&mut self, _: bool) {
    let else_jump = self.emit_jump(OpCode::JumpIfFalse);
    let end_jump = self.emit_jump(OpCode::Jump);

    self.patch_jump(else_jump);
    self.emit_byte(OpCode::Pop.into());

    self.parse_precedence(Precedent::Or);
    self.patch_jump(end_jump);
  }

  fn string(&mut self, _: bool) {
    let len = self.parser.previous.lexeme.len() - 1;
    let string = self.parser.previous.lexeme[1..len].to_string();
    self.emit_constant(Value::Str(string))
  }

  fn resolve_local(&self, name: &Token) -> Option<u8> {
    for (e, v) in self.locals.borrow().iter().rev().enumerate() {
      if v.name.lexeme == name.lexeme {
        if v.depth.is_none() {
          self.error("Can't read local variable in its own initializer.");
        }
        return Some((self.locals.borrow().len() - e - 1) as u8);
      }
    }
    None
  }

  fn named_variable(&mut self, name: &Token, can_assign: bool) {
    let (arg, get_op, set_op) = if let Some(local_arg) = self.resolve_local(name) {
      (local_arg, OpCode::GetLocal, OpCode::SetLocal)
    } else {
      (
        self.identifier_constant(name),
        OpCode::GetGlobal,
        OpCode::SetGlobal,
      )
    };

    if can_assign && self.is_match(TokenType::Equal) {
      self.expression();
      self.emit_bytes(set_op, arg);
    } else {
      self.emit_bytes(get_op, arg);
    }
  }

  fn variable(&mut self, can_assign: bool) {
    let name = self.parser.previous.clone();
    self.named_variable(&name, can_assign);
  }

  fn unary(&mut self, _: bool) {
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
      let can_assign = precedent <= Precedent::Assignment;
      prefix_rule(self, can_assign);
      while precedent <= self.rules[self.parser.current.t_type as usize].precedence {
        self.advance();
        if let Some(infix_rule) = self.rules[self.parser.previous.t_type as usize].infix {
          infix_rule(self, can_assign);
        }

        if can_assign && self.is_match(TokenType::Equal) {
          self.error("Invalid assignment target.");
        }
      }
    } else {
      self.error("Expect expression.");
    }
  }

  fn identifier_constant(&mut self, name: &Token) -> u8 {
    self.make_constant(Value::Str(name.lexeme.clone()))
  }

  fn add_local(&self, name: &Token) {
    if self.locals.borrow().len() >= 256 {
      self.error("Too many local variables in function.");
      return;
    }

    let loc = Local {
      name: name.clone(),
      depth: None,
    };

    self.locals.borrow_mut().push(loc);
  }

  fn declare_variable(&mut self) {
    if self.scope_depth == 0 {
      return;
    }

    let name = self.parser.previous.lexeme.clone();
    if self
      .locals
      .borrow()
      .iter()
      .filter(|x| x.name.lexeme == name)
      .count()
      != 0
    {
      self.error("Already a variable with this name in this scope.");
    } else {
      self.add_local(&self.parser.previous);
    }
  }

  fn parse_variable(&mut self, error_message: &str) -> u8 {
    self.consume(TokenType::Identifier, error_message);

    self.declare_variable();
    if self.scope_depth > 0 {
      return 0;
    }
    let name = self.parser.previous.clone();
    self.identifier_constant(&name)
  }

  fn mark_initialized(&mut self) {
    let len = self.locals.borrow().len() - 1;
    let mut locals = self.locals.borrow_mut();
    locals[len].depth = Some(self.scope_depth);
  }

  fn define_variable(&mut self, global: u8) {
    if self.scope_depth == 0 {
      self.emit_bytes(OpCode::DefineGlobal, global);
    } else {
      self.mark_initialized();
    }
  }

  fn and(&mut self, _: bool) {
    let end_jump = self.emit_jump(OpCode::JumpIfFalse);
    self.emit_byte(OpCode::Pop.into());
    self.parse_precedence(Precedent::And);
    self.patch_jump(end_jump);
  }

  fn expression(&mut self) {
    self.parse_precedence(Precedent::Assignment);
  }

  fn block(&mut self) {
    while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
      self.declaration();
    }

    self.consume(TokenType::RightBrace, "Expect '}' after block.");
  }

  fn def_declaration(&mut self) {
    let global = self.parse_variable("Expect variable name.");

    if self.is_match(TokenType::Equal) {
      self.expression();
    } else {
      self.emit_byte(OpCode::Nil.into());
    }

    self.consume_statement_end();
    self.define_variable(global);
  }

  fn expression_statement(&mut self) {
    self.expression();
    self.consume_statement_end();
    self.emit_byte(OpCode::Pop.into());
  }

  fn for_statement(&mut self) {
    self.begin_scope();
    // for-变量定义解析
    if self.is_match(TokenType::SemiColon) {
      // No initializer
    } else if self.is_match(TokenType::Def) {
      self.def_declaration();
    } else {
      self.expression_statement(); // consume semicolon
    }

    let mut loop_start = self.chunk.count();

    // for-判断条件解析
    let exit_jump = if self.is_match(TokenType::SemiColon) {
      None
    } else {
      self.expression();
      self.consume(TokenType::SemiColon, "Expect ';' after loop condition.");

      // 如果上面表达式解析出的结果是false 则跳出循环
      let result = self.emit_jump(OpCode::JumpIfFalse);
      self.emit_byte(OpCode::Pop.into());

      Some(result)
    };

    // for-条件修改解析
    if !self.check(TokenType::LeftBrace) {
      let body_jump = self.emit_jump(OpCode::Jump);
      let increment_start = self.chunk.count();

      self.expression();
      self.emit_byte(OpCode::Pop.into());

      self.emit_loop(loop_start);
      loop_start = increment_start;
      self.patch_jump(body_jump);
    }

    self.statement();
    self.emit_loop(loop_start);

    if let Some(exit) = exit_jump {
      self.patch_jump(exit);
      self.emit_byte(OpCode::Pop.into());
    }

    self.end_scope();
  }

  fn if_statement(&mut self) {
    self.expression();

    let then_jump = self.emit_jump(OpCode::JumpIfFalse);
    self.emit_byte(OpCode::Pop.into());
    self.statement();

    let else_jump = self.emit_jump(OpCode::Jump);

    self.emit_byte(OpCode::Pop.into());
    self.patch_jump(then_jump);

    if self.is_match(TokenType::Else) {
      self.statement();
    }

    self.patch_jump(else_jump);
  }

  fn print_statement(&mut self) {
    self.expression();
    self.consume_statement_end();
    self.emit_byte(OpCode::Print.into());
  }

  fn while_statement(&mut self) {
    let loop_start = self.chunk.count();

    self.expression();

    let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
    self.emit_byte(OpCode::Pop.into());
    self.statement();
    self.emit_loop(loop_start);

    self.patch_jump(exit_jump);
    self.emit_byte(OpCode::Pop.into());
  }

  fn synchronize(&mut self) {
    self.parser.panic_mode.replace(false);

    while self.parser.current.t_type != TokenType::Eof {
      if self.parser.previous.t_type == TokenType::SemiColon {
        return;
      }

      if matches!(
        self.parser.current.t_type,
        TokenType::Class
          | TokenType::Fun
          | TokenType::Def
          | TokenType::For
          | TokenType::If
          | TokenType::While
          | TokenType::Print
          | TokenType::Return
      ) {
        return;
      }
      self.advance();
    }
  }

  fn declaration(&mut self) {
    // 去除表达式前的换行
    self.skip_new_line();

    if self.is_match(TokenType::Def) {
      self.def_declaration();
    } else {
      self.statement();
    }

    if *self.parser.panic_mode.borrow() {
      self.synchronize();
    }

    // 去除表达式后的换行
    self.skip_new_line();
  }

  fn skip_new_line(&mut self) {
    while self.check(TokenType::Wrap) {
      self.advance();
    }
  }

  fn statement(&mut self) {
    if self.is_match(TokenType::Print) {
      self.print_statement();
    } else if self.is_match(TokenType::For) {
      self.for_statement();
    } else if self.is_match(TokenType::If) {
      self.if_statement();
    } else if self.is_match(TokenType::While) {
      self.while_statement();
    } else if self.is_match(TokenType::LeftBrace) {
      self.begin_scope();
      self.block();
      self.end_scope();
    } else {
      self.expression_statement();
    }
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

