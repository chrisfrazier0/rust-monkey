#[derive(PartialEq, Debug)]
pub enum TokenType {
  True,
  False,
  Let,
  Ident,
  Int,
  Bang,
  Assign,
  Plus,
  Minus,
  Slash,
  Asterisk,
  Function,
  LParen,
  RParen,
  LBrace,
  RBrace,
  If,
  Else,
  Eq,
  NotEq,
  LessThan,
  GreaterThan,
  Comma,
  Semicolon,
  Return,
  Eof,
  Invalid,
}

impl From<&str> for TokenType {
  fn from(value: &str) -> Self {
    match value {
      "true" => TokenType::True,
      "false" => TokenType::False,
      "let" => TokenType::Let,
      "fn" => TokenType::Function,
      "if" => TokenType::If,
      "else" => TokenType::Else,
      "return" => TokenType::Return,
      _ => TokenType::Ident,
    }
  }
}

#[derive(PartialEq, Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub literal: String,
}

impl Token {
  pub fn new(token_type: TokenType, literal: &str) -> Self {
    Token {
      token_type,
      literal: literal.to_string(),
    }
  }
}
