use crate::{
  ast::{
    BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, Function, Identifier,
    IfExpression, InfixExpression, Integer, LetStatement, PrefixExpression, Program,
    ReturnStatement, Statement,
  },
  lexer::Lexer,
  token::{Token, TokenType},
};

#[repr(u8)]
#[derive(PartialEq, PartialOrd)]
enum Precedence {
  Lowest,
  Equals,
  LessGreater,
  Sum,
  Product,
  Prefix,
  Call,
}

fn get_precedence(tt: &TokenType) -> Precedence {
  match tt {
    TokenType::Eq => Precedence::Equals,
    TokenType::NotEq => Precedence::Equals,
    TokenType::LessThan => Precedence::LessGreater,
    TokenType::GreaterThan => Precedence::LessGreater,
    TokenType::Plus => Precedence::Sum,
    TokenType::Minus => Precedence::Sum,
    TokenType::Slash => Precedence::Product,
    TokenType::Asterisk => Precedence::Product,
    TokenType::LParen => Precedence::Call,
    _ => Precedence::Lowest,
  }
}

pub struct Parser {
  pub errors: Vec<String>,

  lexer: Lexer,
  current_token: Token,
  peek_token: Token,
}

impl Parser {
  pub fn new(mut lexer: Lexer) -> Self {
    let current_token = lexer.next_token();
    let peek_token = lexer.next_token();
    Parser {
      errors: vec![],
      lexer,
      current_token,
      peek_token,
    }
  }

  pub fn from(input: &str) -> Self {
    Self::new(Lexer::new(input))
  }

  pub fn parse_program(&mut self) -> Program {
    let mut program = Program { statements: vec![] };
    while self.current_token.token_type != TokenType::Eof {
      if let Some(stmt) = self.parse_statement() {
        program.statements.push(stmt);
      }
      if !self.current_is(&TokenType::Semicolon) && !self.peek_is(&TokenType::Eof) {
        self.semicolon_error(&self.current_token.token_type.clone());
      }
      self.next_token();
    }
    program
  }

  fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
    match self.current_token.token_type {
      TokenType::LBrace => {
        let block = Box::new(self.parse_block_statement());
        self.next_token();
        Some(block)
      }
      TokenType::Let => self
        .parse_let_statement()
        .map(|s| Box::new(s) as Box<dyn Statement>),
      TokenType::Return => Some(Box::new(self.parse_return_statement())),
      _ => Some(Box::new(self.parse_expression_statement())),
    }
  }

  fn parse_block_statement(&mut self) -> BlockStatement {
    let mut block = BlockStatement {
      token: self.current_token.clone(),
      statements: vec![],
    };
    self.next_token();
    while !self.current_is(&TokenType::RBrace) && !self.current_is(&TokenType::Eof) {
      if let Some(stmt) = self.parse_statement() {
        block.statements.push(stmt);
      }
      self.next_token();
    }
    block
  }

  fn parse_let_statement(&mut self) -> Option<LetStatement> {
    let token = self.current_token.clone();
    if !self.expect_peek(&TokenType::Ident) {
      return None;
    }
    let name = Identifier {
      token: self.current_token.clone(),
      value: self.current_token.literal.clone(),
    };
    if !self.expect_peek(&TokenType::Assign) {
      return None;
    }
    self.next_token();
    let value = self.parse_expression(Precedence::Lowest);
    if self.peek_is(&TokenType::Semicolon) {
      self.next_token();
    }
    Some(LetStatement { token, name, value })
  }

  fn parse_return_statement(&mut self) -> ReturnStatement {
    let token = self.current_token.clone();
    self.next_token();
    let mut value = None;
    if !self.current_is(&TokenType::Semicolon) {
      value = self.parse_expression(Precedence::Lowest);
      if self.peek_is(&TokenType::Semicolon) {
        self.next_token();
      }
    }
    ReturnStatement { token, value }
  }

  fn parse_expression_statement(&mut self) -> ExpressionStatement {
    let token = self.current_token.clone();
    let expr = self.parse_expression(Precedence::Lowest);
    if self.peek_is(&TokenType::Semicolon) {
      self.next_token();
    }
    ExpressionStatement { token, expr }
  }

  fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
    let mut left = match self.current_token.token_type {
      TokenType::If => self.parse_if_expr(),
      TokenType::Function => self.parse_function(),
      TokenType::LParen => self.parse_grouped(),
      TokenType::Int => self.parse_integer(),
      TokenType::Ident => Some(self.parse_identifier()),
      TokenType::True | TokenType::False => Some(self.parse_boolean()),
      TokenType::Bang | TokenType::Minus => Some(self.parse_prefix()),
      _ => {
        let ch = self.current_token.literal.clone();
        self.prefix_parse_error(&ch);
        return None;
      }
    };
    while precedence < self.peek_precedence() {
      self.next_token();
      left = match self.current_token.token_type {
        TokenType::Eq
        | TokenType::NotEq
        | TokenType::LessThan
        | TokenType::GreaterThan
        | TokenType::Plus
        | TokenType::Minus
        | TokenType::Slash
        | TokenType::Asterisk => Some(self.parse_infix(left.unwrap())),
        TokenType::LParen => Some(self.parse_call_expression(left.unwrap())),
        _ => left,
      };
    }
    left
  }

  fn parse_if_expr(&mut self) -> Option<Box<dyn Expression>> {
    let token = self.current_token.clone();
    let mut braces = false;
    if self.peek_is(&TokenType::LParen) {
      braces = true;
      self.next_token();
    }
    self.next_token();
    let condition = self.parse_expression(Precedence::Lowest);
    if braces && !self.expect_peek(&TokenType::RParen) {
      return None;
    }
    if !self.expect_peek(&TokenType::LBrace) {
      return None;
    }
    let consequence = self.parse_block_statement();
    let mut alternative = None;
    if self.peek_is(&TokenType::Else) {
      self.next_token();
      if !self.expect_peek(&TokenType::LBrace) {
        return None;
      }
      alternative = Some(self.parse_block_statement());
    }
    self.next_token();
    Some(Box::new(IfExpression {
      token,
      condition,
      consequence,
      alternative,
    }))
  }

  fn parse_function(&mut self) -> Option<Box<dyn Expression>> {
    let token = self.current_token.clone();
    if !self.expect_peek(&TokenType::LParen) {
      return None;
    }
    let parameters = self.parse_function_params();
    if !self.expect_peek(&TokenType::LBrace) {
      return None;
    }
    let body = self.parse_block_statement();
    self.next_token();
    Some(Box::new(Function {
      token,
      parameters,
      body,
    }))
  }

  fn parse_function_params(&mut self) -> Vec<Identifier> {
    let mut idents = vec![];
    if self.peek_is(&TokenType::RParen) {
      self.next_token();
      return idents;
    }
    self.next_token();
    idents.push(Identifier {
      token: self.current_token.clone(),
      value: self.current_token.literal.clone(),
    });
    while self.peek_is(&TokenType::Comma) {
      self.next_token();
      self.next_token();
      idents.push(Identifier {
        token: self.current_token.clone(),
        value: self.current_token.literal.clone(),
      });
    }
    if !self.expect_peek(&TokenType::RParen) {
      return vec![];
    }
    idents
  }

  fn parse_call_expression(&mut self, function: Box<dyn Expression>) -> Box<dyn Expression> {
    Box::new(CallExpression {
      token: self.current_token.clone(),
      function,
      arguments: self.parse_call_args(),
    })
  }

  fn parse_call_args(&mut self) -> Vec<Box<dyn Expression>> {
    let mut args = vec![];
    if self.peek_is(&TokenType::RParen) {
      self.next_token();
      return args;
    }
    self.next_token();
    if let Some(arg) = self.parse_expression(Precedence::Lowest) {
      args.push(arg);
    }
    while self.peek_is(&TokenType::Comma) {
      self.next_token();
      self.next_token();
      if let Some(arg) = self.parse_expression(Precedence::Lowest) {
        args.push(arg);
      }
    }
    if !self.expect_peek(&TokenType::RParen) {
      return vec![];
    }
    args
  }

  fn parse_prefix(&mut self) -> Box<dyn Expression> {
    let token = self.current_token.clone();
    let operator = self.current_token.literal.clone();
    self.next_token();
    Box::new(PrefixExpression {
      token,
      operator,
      right: self.parse_expression(Precedence::Prefix),
    })
  }

  fn parse_infix(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
    let token = self.current_token.clone();
    let operator = self.current_token.literal.clone();
    let precedence = self.current_precedence();
    self.next_token();
    Box::new(InfixExpression {
      token,
      left,
      operator,
      right: self.parse_expression(precedence),
    })
  }

  fn parse_grouped(&mut self) -> Option<Box<dyn Expression>> {
    self.next_token();
    let expr = self.parse_expression(Precedence::Lowest);
    if !self.expect_peek(&TokenType::RParen) {
      return None;
    }
    expr
  }

  fn parse_identifier(&self) -> Box<dyn Expression> {
    Box::new(Identifier {
      token: self.current_token.clone(),
      value: self.current_token.literal.clone(),
    })
  }

  fn parse_boolean(&mut self) -> Box<dyn Expression> {
    Box::new(Boolean {
      token: self.current_token.clone(),
      value: self.current_is(&TokenType::True),
    })
  }

  fn parse_integer(&mut self) -> Option<Box<dyn Expression>> {
    let token = self.current_token.clone();
    let Ok(value) = self.current_token.literal.parse::<i32>() else {
      self.errors.push(format!(
        "Could not parse {} as integer",
        self.current_token.literal
      ));
      return None;
    };
    Some(Box::new(Integer { token, value }))
  }

  fn next_token(&mut self) {
    self.current_token = self.peek_token.clone();
    self.peek_token = self.lexer.next_token();
  }

  fn current_is(&self, tt: &TokenType) -> bool {
    &self.current_token.token_type == tt
  }

  fn peek_is(&self, tt: &TokenType) -> bool {
    &self.peek_token.token_type == tt
  }

  fn expect_peek(&mut self, tt: &TokenType) -> bool {
    if self.peek_is(tt) {
      self.next_token();
      return true;
    }
    self.peek_error(tt);
    false
  }

  fn current_precedence(&self) -> Precedence {
    get_precedence(&self.current_token.token_type)
  }

  fn peek_precedence(&self) -> Precedence {
    get_precedence(&self.peek_token.token_type)
  }

  fn semicolon_error(&mut self, tt: &TokenType) {
    self.errors.push(format!(
      "All statements should end with a Semicolon ';', got: {}",
      tt,
    ))
  }

  fn peek_error(&mut self, tt: &TokenType) {
    self.errors.push(format!(
      "Expected next token to be {:?} '{}', got '{}' instead",
      tt, tt, self.peek_token.token_type,
    ));
  }

  fn prefix_parse_error(&mut self, ch: &str) {
    self
      .errors
      .push(format!("No prefix parse function for '{}' found", ch));
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast::{
    CallExpression, ExpressionStatement, Function, IfExpression, InfixExpression, Integer,
    LetStatement, Node, PrefixExpression, ReturnStatement,
  };

  enum Expected {
    Boolean(bool),
    Integer(i32),
    Identifier(String),
  }

  #[test]
  fn parser_let_statements() {
    let tests = vec![
      ("let x = 5;", "x", Expected::Integer(5)),
      ("let y = true;", "y", Expected::Boolean(true)),
      (
        "let foobar = y;",
        "foobar",
        Expected::Identifier("y".to_string()),
      ),
    ];

    for (input, name, value) in tests {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();

      check_parser_errors(&parser.errors);
      assert_eq!(program.statements.len(), 1, "Should have one statement");
      assert_eq!(program.statements[0].token_literal(), "let");

      let let_stmt = program.statements[0]
        .as_any()
        .downcast_ref::<LetStatement>()
        .expect("Statement is not a LetStatement");

      assert_eq!(let_stmt.name.value, name);
      assert_eq!(let_stmt.name.token_literal(), name);

      let let_val = let_stmt.value.as_ref().unwrap().as_ref();
      test_literal_expression(let_val, &value);
    }
  }

  #[test]
  fn parser_return_statements() {
    let tests = vec![
      ("return 5;", Expected::Integer(5)),
      ("return 10;", Expected::Integer(10)),
      ("return 993322;", Expected::Integer(993322)),
      ("return true;", Expected::Boolean(true)),
      ("return abc;", Expected::Identifier("abc".to_string())),
    ];

    for (input, value) in tests {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();

      check_parser_errors(&parser.errors);
      assert_eq!(program.statements.len(), 1, "Should have one statement");

      let ret_stmt = program.statements[0]
        .as_any()
        .downcast_ref::<ReturnStatement>()
        .expect("Statement is not a ReturnStatement");

      assert_eq!(ret_stmt.token_literal(), "return");

      let ret_val = ret_stmt.value.as_ref().unwrap().as_ref();
      test_literal_expression(ret_val, &value);
    }
  }

  #[test]
  fn parser_ident_expression() {
    let input = "foobar;";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let ident = stmt.expr.as_ref().unwrap().as_ref();
    test_identifier(ident, "foobar");
  }

  #[test]
  fn parser_boolean_expression() {
    let input = r#"
      true;
      false;
      "#;

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 2, "Should have two statements");

    for (i, statement) in program.statements.iter().enumerate() {
      let stmt = statement
        .as_any()
        .downcast_ref::<ExpressionStatement>()
        .expect("Statement is not an ExpressionStatement");

      let bool = stmt.expr.as_ref().unwrap().as_ref();
      test_boolean(bool, i == 0);
    }
  }

  #[test]
  fn parser_integer_expression() {
    let input = "5;";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let lit = stmt.expr.as_ref().unwrap().as_ref();
    test_integer_literal(lit, 5);
  }

  #[test]
  fn parser_prefix_expression() {
    let tests = vec![
      ("!5;", "!", Expected::Integer(5)),
      ("-15;", "-", Expected::Integer(15)),
      ("!true;", "!", Expected::Boolean(true)),
      ("!false;", "!", Expected::Boolean(false)),
    ];

    for (input, op, value) in tests {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();

      check_parser_errors(&parser.errors);
      assert_eq!(program.statements.len(), 1, "Should have one statement");

      let stmt = program.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>()
        .expect("Statement is not an ExpressionStatement");

      let expr = stmt
        .expr
        .as_ref()
        .unwrap()
        .as_any()
        .downcast_ref::<PrefixExpression>()
        .expect("ExpressionStatement is not a PrefixExpression");

      let right = expr.right.as_ref().unwrap().as_ref();
      assert_eq!(expr.operator, op);
      test_literal_expression(right, &value);
    }
  }

  #[test]
  fn parser_infix_expression() {
    let tests = vec![
      ("5 + 5;", Expected::Integer(5), "+", Expected::Integer(5)),
      ("5 - 5;", Expected::Integer(5), "-", Expected::Integer(5)),
      ("5 * 5;", Expected::Integer(5), "*", Expected::Integer(5)),
      ("5 / 5;", Expected::Integer(5), "/", Expected::Integer(5)),
      ("5 > 5;", Expected::Integer(5), ">", Expected::Integer(5)),
      ("5 < 5;", Expected::Integer(5), "<", Expected::Integer(5)),
      ("5 == 5;", Expected::Integer(5), "==", Expected::Integer(5)),
      ("5 != 5;", Expected::Integer(5), "!=", Expected::Integer(5)),
      (
        "true == true;",
        Expected::Boolean(true),
        "==",
        Expected::Boolean(true),
      ),
      (
        "true != false;",
        Expected::Boolean(true),
        "!=",
        Expected::Boolean(false),
      ),
      (
        "false == false;",
        Expected::Boolean(false),
        "==",
        Expected::Boolean(false),
      ),
      (
        "a + b;",
        Expected::Identifier("a".to_string()),
        "+",
        Expected::Identifier("b".to_string()),
      ),
    ];

    for (input, left, op, right) in tests {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();

      check_parser_errors(&parser.errors);
      assert_eq!(program.statements.len(), 1, "Should have one statement");

      let stmt = program.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>()
        .expect("Statement is not an ExpressionStatement");

      let expr = stmt.expr.as_ref().unwrap().as_ref();
      test_infix_expression(expr, &left, op, &right);
    }
  }

  #[test]
  fn parser_if_expression() {
    let input = "if (x < y) { x };";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let if_expr = stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<IfExpression>()
      .expect("ExpressionStatement is not an IfExpression");

    let condition = if_expr.condition.as_ref().unwrap().as_ref();
    test_infix_expression(
      condition,
      &Expected::Identifier("x".to_string()),
      "<",
      &Expected::Identifier("y".to_string()),
    );

    let con_len = if_expr.consequence.statements.len();
    assert_eq!(con_len, 1, "Should have 1 consequence statement");

    let expr_stmt = if_expr.consequence.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Consequence is not an ExpressionStatement");

    let ident = expr_stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("Consequence is not an Identifier");

    test_identifier(ident, "x");
    assert!(if_expr.alternative.is_none());
  }

  #[test]
  fn parser_if_else_expression() {
    let input = "if (x > y) { x } else { y };";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let if_expr = stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<IfExpression>()
      .expect("ExpressionStatement is not an IfExpression");

    let condition = if_expr.condition.as_ref().unwrap().as_ref();
    test_infix_expression(
      condition,
      &Expected::Identifier("x".to_string()),
      ">",
      &Expected::Identifier("y".to_string()),
    );

    let con_len = if_expr.consequence.statements.len();
    assert_eq!(con_len, 1, "Should have 1 consequence statement");

    let expr_stmt = if_expr.consequence.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Consequence is not an ExpressionStatement");

    let ident = expr_stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("Consequence is not an Identifier");

    test_identifier(ident, "x");

    let alt_len = if_expr.alternative.as_ref().unwrap().statements.len();
    assert_eq!(alt_len, 1, "Should have 1 alternative statement");

    let expr_stmt = if_expr.alternative.as_ref().unwrap().statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Alternative is not an ExpressionStatement");

    let ident = expr_stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("Alternative is not an Identifier");

    test_identifier(ident, "y");
  }

  #[test]
  fn parser_if_else_expression_shorthand() {
    let input = "if x > y { x } else { y };";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let if_expr = stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<IfExpression>()
      .expect("ExpressionStatement is not an IfExpression");

    let condition = if_expr.condition.as_ref().unwrap().as_ref();
    test_infix_expression(
      condition,
      &Expected::Identifier("x".to_string()),
      ">",
      &Expected::Identifier("y".to_string()),
    );

    let con_len = if_expr.consequence.statements.len();
    assert_eq!(con_len, 1, "Should have 1 consequence statement");

    let expr_stmt = if_expr.consequence.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Consequence is not an ExpressionStatement");

    let ident = expr_stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("Consequence is not an Identifier");

    test_identifier(ident, "x");

    let alt_len = if_expr.alternative.as_ref().unwrap().statements.len();
    assert_eq!(alt_len, 1, "Should have 1 alternative statement");

    let expr_stmt = if_expr.alternative.as_ref().unwrap().statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Alternative is not an ExpressionStatement");

    let ident = expr_stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("Alternative is not an Identifier");

    test_identifier(ident, "y");
  }

  #[test]
  fn parser_function_expression() {
    let input = "fn(x, y) { x + y; };";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let func = stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<Function>()
      .expect("ExpressionStatement is not a Function");

    assert_eq!(func.parameters.len(), 2, "Function should have 2 params");
    test_literal_expression(&func.parameters[0], &Expected::Identifier("x".to_string()));
    test_literal_expression(&func.parameters[1], &Expected::Identifier("y".to_string()));

    let body_len = func.body.statements.len();
    assert_eq!(body_len, 1, "Body should have 1 statement");

    let body_stmt = func.body.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Function body is not an ExpressionStatement");

    let expr = body_stmt.expr.as_ref().unwrap().as_ref();
    test_infix_expression(
      expr,
      &Expected::Identifier("x".to_string()),
      "+",
      &Expected::Identifier("y".to_string()),
    );
  }

  #[test]
  fn parser_function_params() {
    let tests = vec![
      ("fn() {};", vec![]),
      ("fn(x) {};", vec!["x".to_string()]),
      (
        "fn(x, y, z) {};",
        vec!["x".to_string(), "y".to_string(), "z".to_string()],
      ),
    ];

    for (input, expected) in tests {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();
      check_parser_errors(&parser.errors);

      let stmt = program.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>()
        .expect("Statement is not an ExpressionStatement");

      let func = stmt
        .expr
        .as_ref()
        .unwrap()
        .as_any()
        .downcast_ref::<Function>()
        .expect("ExpressionStatement is not a Function");

      assert_eq!(
        func.parameters.len(),
        expected.len(),
        "Expected {} params, got {}",
        expected.len(),
        func.parameters.len(),
      );
      for (i, ident) in func.parameters.iter().enumerate() {
        test_identifier(ident, &expected[i]);
      }
    }
  }

  #[test]
  fn parser_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut parser = Parser::from(input);
    let program = parser.parse_program();

    check_parser_errors(&parser.errors);
    assert_eq!(program.statements.len(), 1, "Should have one statement");

    let stmt = program.statements[0]
      .as_any()
      .downcast_ref::<ExpressionStatement>()
      .expect("Statement is not an ExpressionStatement");

    let call = stmt
      .expr
      .as_ref()
      .unwrap()
      .as_any()
      .downcast_ref::<CallExpression>()
      .expect("ExpressionStatement is not a CallExpression");

    test_identifier(call.function.as_ref(), "add");
    assert_eq!(call.arguments.len(), 3, "Should have three arguments");
    test_integer_literal(call.arguments[0].as_ref(), 1);
    test_infix_expression(
      call.arguments[1].as_ref(),
      &Expected::Integer(2),
      "*",
      &Expected::Integer(3),
    );
    test_infix_expression(
      call.arguments[2].as_ref(),
      &Expected::Integer(4),
      "+",
      &Expected::Integer(5),
    );
  }

  #[test]
  fn parser_precedence() {
    let tests = vec![
      ("-a * b;", "((-a) * b);"),
      ("!-a;", "(!(-a));"),
      ("a + b + c;", "((a + b) + c);"),
      ("a + b - c;", "((a + b) - c);"),
      ("a * b * c;", "((a * b) * c);"),
      ("a * b / c;", "((a * b) / c);"),
      ("a + b / c;", "(a + (b / c));"),
      ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
      ("3 + 4; -5 * 5;", "(3 + 4);\n((-5) * 5);"),
      ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
      ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5;",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
      ),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5;",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
      ),
      ("true;", "true;"),
      ("false;", "false;"),
      ("3 > 5 == false;", "((3 > 5) == false);"),
      ("3 < 5 == true;", "((3 < 5) == true);"),
      ("!true + 5;", "((!true) + 5);"),
      ("!false + 5 * 3;", "((!false) + (5 * 3));"),
      ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
      ("(5 + 5) * 2;", "((5 + 5) * 2);"),
      ("2 / (5 + 5);", "(2 / (5 + 5));"),
      ("-(5 + 5);", "(-(5 + 5));"),
      ("!(true == true);", "(!(true == true));"),
      ("a + add(b * c) + d;", "((a + add((b * c))) + d);"),
      (
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
      ),
      (
        "add(a + b + c * d / f + g);",
        "add((((a + b) + ((c * d) / f)) + g));",
      ),
    ];

    for (_, (input, output)) in tests.iter().enumerate() {
      let mut parser = Parser::from(input);
      let program = parser.parse_program();

      check_parser_errors(&parser.errors);
      assert_eq!(&program.to_string(), output);
    }
  }

  fn check_parser_errors(errors: &Vec<String>) {
    if !errors.is_empty() {
      println!("Parser has {} errors", errors.len());
      for err in errors {
        println!("Parser error: {}", err);
      }
    }
    assert_eq!(errors.len(), 0, "Parser returned errors");
  }

  fn test_identifier(ident: &dyn Expression, value: &str) {
    let ident = ident
      .as_any()
      .downcast_ref::<Identifier>()
      .expect("ExpressionStatement is not an Identifier");

    assert_eq!(ident.value, value);
    assert_eq!(ident.token_literal(), value);
  }

  fn test_boolean(b: &dyn Expression, value: bool) {
    let expected_lit = format!("{}", value);
    let bool = b
      .as_any()
      .downcast_ref::<Boolean>()
      .expect("ExpressionStatement is not a Boolean");

    assert_eq!(bool.value, value);
    assert_eq!(bool.token_literal(), expected_lit);
  }

  fn test_integer_literal(il: &dyn Expression, value: i32) {
    let int = il
      .as_any()
      .downcast_ref::<Integer>()
      .expect("Expression is not an Integer");

    assert_eq!(int.value, value);
    assert_eq!(int.token_literal(), format!("{value}"));
  }

  fn test_literal_expression(expr: &dyn Expression, expected: &Expected) {
    match expected {
      Expected::Boolean(b) => test_boolean(expr, *b),
      Expected::Integer(v) => test_integer_literal(expr, *v),
      Expected::Identifier(x) => test_identifier(expr, &x),
    }
  }

  fn test_infix_expression(expr: &dyn Expression, left: &Expected, op: &str, right: &Expected) {
    let infix = expr
      .as_any()
      .downcast_ref::<InfixExpression>()
      .expect("ExpressionStatement is not a PrefixExpression");

    let right_expr = infix.right.as_ref().unwrap().as_ref();
    test_literal_expression(infix.left.as_ref(), left);
    assert_eq!(infix.operator, op);
    test_literal_expression(right_expr, right);
  }
}
