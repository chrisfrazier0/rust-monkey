use std::{any::Any, fmt};

use crate::token::Token;

pub trait Node: fmt::Display + fmt::Debug {
  fn token_literal(&self) -> String;
  fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone)]
pub struct Program {
  pub statements: Vec<Statement>,
}

impl Node for Program {
  fn token_literal(&self) -> String {
    if !self.statements.is_empty() {
      self.statements[0].token_literal()
    } else {
      String::from("<no-statements>")
    }
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut out = vec![];
    for stmt in &self.statements {
      out.push(stmt.to_string());
    }
    write!(f, "{}", out.join("\n"))
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  Let(LetStatement),
  Return(ReturnStatement),
  Expression(ExpressionStatement),
  Block(BlockStatement),
}

impl Node for Statement {
  fn token_literal(&self) -> String {
    match self {
      Statement::Let(s) => s.token_literal(),
      Statement::Return(s) => s.token_literal(),
      Statement::Expression(s) => s.token_literal(),
      Statement::Block(s) => s.token_literal(),
    }
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Let(s) => s.fmt(f),
      Statement::Return(s) => s.fmt(f),
      Statement::Expression(s) => s.fmt(f),
      Statement::Block(s) => s.fmt(f),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Expression {
  Identifier(Identifier),
  Integer(Integer),
  Boolean(Boolean),
  Prefix(Box<PrefixExpression>),
  Infix(Box<InfixExpression>),
  If(Box<IfExpression>),
  Function(Box<Function>),
  Call(Box<CallExpression>),
}

impl Node for Expression {
  fn token_literal(&self) -> String {
    match self {
      Expression::Identifier(e) => e.token_literal(),
      Expression::Integer(e) => e.token_literal(),
      Expression::Boolean(e) => e.token_literal(),
      Expression::Prefix(e) => e.token_literal(),
      Expression::Infix(e) => e.token_literal(),
      Expression::If(e) => e.token_literal(),
      Expression::Function(e) => e.token_literal(),
      Expression::Call(e) => e.token_literal(),
    }
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(e) => e.fmt(f),
      Expression::Integer(e) => e.fmt(f),
      Expression::Boolean(e) => e.fmt(f),
      Expression::Prefix(e) => e.fmt(f),
      Expression::Infix(e) => e.fmt(f),
      Expression::If(e) => e.fmt(f),
      Expression::Function(e) => e.fmt(f),
      Expression::Call(e) => e.fmt(f),
    }
  }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
  pub token: Token,
  pub name: Identifier,
  pub value: Option<Box<Expression>>,
}

impl Node for LetStatement {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for LetStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let value_str = self
      .value
      .as_ref()
      .map(|v| v.to_string())
      .unwrap_or("".to_string());
    write!(f, "{} {} = {};", self.token_literal(), self.name, value_str)
  }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
  pub token: Token,
  pub value: Option<Box<Expression>>,
}

impl Node for ReturnStatement {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for ReturnStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let value_str = self
      .value
      .as_ref()
      .map(|v| v.to_string())
      .unwrap_or("".to_string());
    write!(f, "{} {};", self.token_literal(), value_str)
  }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
  pub token: Token,
  pub expr: Option<Box<Expression>>,
}

impl Node for ExpressionStatement {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for ExpressionStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let expr_str = self
      .expr
      .as_ref()
      .map(|e| e.to_string())
      .unwrap_or("".to_string());
    write!(f, "{};", expr_str)
  }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
  pub token: Token,
  pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for BlockStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut out = vec![];
    for stmt in &self.statements {
      out.push(stmt.to_string());
    }
    write!(f, "{}", out.join("\n"))
  }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
  pub token: Token,
  pub operator: String,
  pub right: Option<Box<Expression>>,
}

impl Node for PrefixExpression {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for PrefixExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let right_str = self
      .right
      .as_ref()
      .map(|e| e.to_string())
      .unwrap_or("".to_string());
    write!(f, "({}{})", self.operator, right_str)
  }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
  pub token: Token,
  pub left: Box<Expression>,
  pub operator: String,
  pub right: Option<Box<Expression>>,
}

impl Node for InfixExpression {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for InfixExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let right_str = self
      .right
      .as_ref()
      .map(|e| e.to_string())
      .unwrap_or("".to_string());
    write!(f, "({} {} {})", self.left, self.operator, right_str)
  }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
  pub token: Token,
  pub condition: Option<Box<Expression>>,
  pub consequence: BlockStatement,
  pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for IfExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut out = format!(
      "if ({}) {{ {} }}",
      self
        .condition
        .as_ref()
        .map(|e| e.to_string())
        .unwrap_or("".to_string()),
      self.consequence
    );
    if let Some(alt) = &self.alternative {
      out.push_str(&format!(" else {{ {} }}", alt));
    }
    write!(f, "{}", out)
  }
}

#[derive(Debug, Clone)]
pub struct Identifier {
  pub token: Token,
  pub value: String,
}

impl Node for Identifier {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Boolean {
  pub token: Token,
  pub value: bool,
}

impl Node for Boolean {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Integer {
  pub token: Token,
  pub value: i32,
}

impl Node for Integer {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Integer {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Function {
  pub token: Token,
  pub parameters: Vec<Identifier>,
  pub body: BlockStatement,
}

impl Node for Function {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let params = self
      .parameters
      .iter()
      .map(|p| p.to_string())
      .collect::<Vec<String>>()
      .join(", ");
    write!(
      f,
      "{}({}) {{ {} }}",
      self.token_literal(),
      params,
      self.body
    )
  }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
  pub token: Token,
  pub function: Box<Expression>,
  pub arguments: Vec<Box<Expression>>,
}

impl Node for CallExpression {
  fn token_literal(&self) -> String {
    self.token.literal.clone()
  }
  fn as_any(&self) -> &dyn Any {
    self
  }
}

impl fmt::Display for CallExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let args = self
      .arguments
      .iter()
      .map(|a| a.to_string())
      .collect::<Vec<String>>()
      .join(", ");
    write!(f, "{}({})", self.function, args)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::token::TokenType;

  #[test]
  fn parser_program_to_string() {
    let program = Program {
      statements: vec![Statement::Let(LetStatement {
        token: Token::new(TokenType::Let, "let"),
        name: Identifier {
          token: Token::new(TokenType::Ident, "myVar"),
          value: "myVar".to_string(),
        },
        value: Some(Box::new(Expression::Identifier(Identifier {
          token: Token::new(TokenType::Ident, "anotherVar"),
          value: "anotherVar".to_string(),
        }))),
      })],
    };
    assert_eq!(program.to_string(), "let myVar = anotherVar;");
  }
}
