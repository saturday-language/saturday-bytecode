pub struct Token {
  pub t_type: TokenType,
  pub lexeme: String,
  pub line: usize,
}

impl Default for Token {
  fn default() -> Self {
    Self {
      t_type: TokenType::Undefined,
      lexeme: String::new(),
      line: 0,
    }
  }
}

impl Clone for Token {
  fn clone(&self) -> Self {
    Self {
      t_type: self.t_type,
      lexeme: self.lexeme.clone(),
      line: self.line,
    }
  }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
  LeftParen,  // (
  RightParen, // )
  LeftBrace,  // {
  RightBrace, // }
  Comma,
  Dot,
  Minus,
  Plus,
  SemiColon,
  Slash,
  Star,
  Bang,      // !
  BangEqual, // !=
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Identifier,
  String,
  Number,
  And,
  Class,
  Else,
  False,
  Fun,
  For,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Def,
  While,
  Error,
  Eof,
  Break,
  Undefined,
}
