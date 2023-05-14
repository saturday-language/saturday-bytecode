use crate::scanner::{Scanner, TokenType};

pub struct Compiler;

impl Compiler {
  pub fn new() -> Self {
    Self {}
  }

  pub fn compile(&self, source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = 0;

    loop {
      let token = scanner.scan_token();
      if token.line != line {
        print!("{:4}", token.line);
        line = token.line;
      } else {
        print!("    | ");
      }
      println!("{:10?} '{}'", token.t_type, token.lexeme);

      if token.t_type == TokenType::Eof {
        break;
      }
    }
  }
}
