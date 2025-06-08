use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      TokenType::True => write!(f, "true"),
      TokenType::False => write!(f, "false"),
      TokenType::Let => write!(f, "let"),
      TokenType::Ident => write!(f, "<ident>"),
      TokenType::Int => write!(f, "<int>"),
      TokenType::Bang => write!(f, "!"),
      TokenType::Assign => write!(f, "="),
      TokenType::Plus => write!(f, "+"),
      TokenType::Minus => write!(f, "-"),
      TokenType::Slash => write!(f, "/"),
      TokenType::Asterisk => write!(f, "*"),
      TokenType::Function => write!(f, "fn"),
      TokenType::LParen => write!(f, "("),
      TokenType::RParen => write!(f, ")"),
      TokenType::LBrace => write!(f, "{{"),
      TokenType::RBrace => write!(f, "}}"),
      TokenType::If => write!(f, "if"),
      TokenType::Else => write!(f, "else"),
      TokenType::Eq => write!(f, "=="),
      TokenType::NotEq => write!(f, "!="),
      TokenType::LessThan => write!(f, "<"),
      TokenType::GreaterThan => write!(f, ">"),
      TokenType::Comma => write!(f, ","),
      TokenType::Semicolon => write!(f, ";"),
      TokenType::Return => write!(f, "return"),
      TokenType::Eof => write!(f, "<eof>"),
      TokenType::Invalid => write!(f, "<invalid>"),
    }
  }
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

#[derive(PartialEq, Clone, Debug)]
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
