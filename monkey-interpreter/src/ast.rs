use std::{any::Any, fmt};

use crate::token::Token;

pub trait Statement: Node {}
pub trait Expression: Node {}
pub trait Node: Any + fmt::Display {
  fn token_literal(&self) -> String;
  fn as_any(&self) -> &dyn Any;
}

macro_rules! impl_node {
  ($type:ty, $kind:ty) => {
    impl $kind for $type {}
    impl Node for $type {
      fn token_literal(&self) -> String {
        self.token.literal.clone()
      }
      fn as_any(&self) -> &dyn Any {
        self
      }
    }
  };
}

pub struct Program {
  pub statements: Vec<Box<dyn Statement>>,
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

pub struct BlockStatement {
  pub token: Token,
  pub statements: Vec<Box<dyn Statement>>,
}

impl_node!(BlockStatement, Statement);
impl fmt::Display for BlockStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut out = vec![];
    for stmt in &self.statements {
      out.push(stmt.to_string());
    }
    write!(f, "{}", out.join("\n"))
  }
}

pub struct LetStatement {
  pub token: Token,
  pub name: Identifier,
  pub value: Option<Box<dyn Expression>>,
}

impl_node!(LetStatement, Statement);
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

pub struct ReturnStatement {
  pub token: Token,
  pub value: Option<Box<dyn Expression>>,
}

impl_node!(ReturnStatement, Statement);
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

pub struct ExpressionStatement {
  pub token: Token,
  pub expr: Option<Box<dyn Expression>>,
}

impl_node!(ExpressionStatement, Statement);
impl fmt::Display for ExpressionStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let expr_str = self
      .expr
      .as_ref()
      .map(|e| e.to_string())
      .unwrap_or("".to_string());
    write!(f, "{};", expr_str,)
  }
}

pub struct PrefixExpression {
  pub token: Token,
  pub operator: String,
  pub right: Option<Box<dyn Expression>>,
}

impl_node!(PrefixExpression, Expression);
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

pub struct InfixExpression {
  pub token: Token,
  pub left: Box<dyn Expression>,
  pub operator: String,
  pub right: Option<Box<dyn Expression>>,
}

impl_node!(InfixExpression, Expression);
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

pub struct IfExpression {
  pub token: Token,
  pub condition: Option<Box<dyn Expression>>,
  pub consequence: BlockStatement,
  pub alternative: Option<BlockStatement>,
}

impl_node!(IfExpression, Expression);
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

pub struct Identifier {
  pub token: Token,
  pub value: String,
}

impl_node!(Identifier, Expression);
impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

pub struct Boolean {
  pub token: Token,
  pub value: bool,
}

impl_node!(Boolean, Expression);
impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

pub struct Integer {
  pub token: Token,
  pub value: i32,
}

impl_node!(Integer, Expression);
impl fmt::Display for Integer {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

pub struct Function {
  pub token: Token,
  pub parameters: Vec<Identifier>,
  pub body: BlockStatement,
}

impl_node!(Function, Expression);
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
      self.body,
    )
  }
}

pub struct CallExpression {
  pub token: Token,
  pub function: Box<dyn Expression>, // Identifier or Function
  pub arguments: Vec<Box<dyn Expression>>,
}

impl_node!(CallExpression, Expression);
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
      statements: vec![Box::new(LetStatement {
        token: Token::new(TokenType::Let, "let"),
        name: Identifier {
          token: Token::new(TokenType::Ident, "myVar"),
          value: "myVar".to_string(),
        },
        value: Some(Box::new(Identifier {
          token: Token::new(TokenType::Ident, "anotherVar"),
          value: "anotherVar".to_string(),
        })),
      })],
    };
    assert_eq!(program.to_string(), "let myVar = anotherVar;");
  }
}
