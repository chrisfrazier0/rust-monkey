use crate::{
  ast::{
    BlockStatement, Boolean, ExpressionStatement, IfExpression, InfixExpression, Integer, Node,
    PrefixExpression, Program, Statement,
  },
  object::{FALSE, Object, TRUE},
};

pub fn eval(node: &dyn Node) -> Object {
  // Statements
  if let Some(p) = node.as_any().downcast_ref::<Program>() {
    return eval_statements(&p.statements);
  } else if let Some(bs) = node.as_any().downcast_ref::<BlockStatement>() {
    return eval_statements(&bs.statements);
  } else if let Some(es) = node.as_any().downcast_ref::<ExpressionStatement>() {
    if let Some(ref expr) = es.expr {
      return eval(expr.as_ref());
    }
  }

  // Expressions
  if let Some(prefix) = node.as_any().downcast_ref::<PrefixExpression>() {
    if let Some(ref right) = prefix.right {
      let right = eval(right.as_ref());
      return eval_prefix_expression(&prefix.operator, &right);
    }
  } else if let Some(infix) = node.as_any().downcast_ref::<InfixExpression>() {
    if let Some(ref right) = infix.right {
      let left = eval(infix.left.as_ref());
      let right = eval(right.as_ref());
      return eval_infix_expression(&infix.operator, &left, &right);
    }
  } else if let Some(if_expr) = node.as_any().downcast_ref::<IfExpression>() {
    return eval_if_expression(if_expr);
  } else if let Some(il) = node.as_any().downcast_ref::<Integer>() {
    return Object::Integer(il.value);
  } else if let Some(b) = node.as_any().downcast_ref::<Boolean>() {
    return bool_to_object(b.value);
  }

  // Default
  Object::Null
}

fn eval_statements(statements: &Vec<Box<dyn Statement>>) -> Object {
  let mut result = Object::Null;
  for stmt in statements {
    result = eval(stmt.as_ref());
  }
  result
}

fn eval_prefix_expression(op: &str, right: &Object) -> Object {
  match op {
    "!" => eval_bang_operator(right),
    "-" => eval_minus_operator(right),
    _ => Object::Null,
  }
}

fn eval_infix_expression(op: &str, left: &Object, right: &Object) -> Object {
  match (left, right) {
    (Object::Integer(l), Object::Integer(r)) => eval_infix_integers(op, l, r),
    (Object::Boolean(l), Object::Boolean(r)) => eval_infix_booleans(op, l, r),
    _ => Object::Null,
  }
}

fn eval_infix_integers(op: &str, left: &i32, right: &i32) -> Object {
  match op {
    "+" => Object::Integer(left + right),
    "-" => Object::Integer(left - right),
    "*" => Object::Integer(left * right),
    "/" => Object::Integer(left / right),
    "<" => bool_to_object(left < right),
    ">" => bool_to_object(left > right),
    "==" => bool_to_object(left == right),
    "!=" => bool_to_object(left != right),
    _ => Object::Null,
  }
}

fn eval_infix_booleans(op: &str, left: &bool, right: &bool) -> Object {
  match op {
    "==" => bool_to_object(left == right),
    "!=" => bool_to_object(left != right),
    _ => Object::Null,
  }
}

pub fn eval_if_expression(node: &IfExpression) -> Object {
  let Some(condition) = &node.condition else {
    return Object::Null;
  };
  let condition_obj = eval(condition.as_ref());
  if is_truthy(&condition_obj) {
    return eval(&node.consequence);
  } else if let Some(alt) = &node.alternative {
    return eval(alt);
  }
  Object::Null
}

fn eval_bang_operator(target: &Object) -> Object {
  match target {
    &TRUE => FALSE,
    &FALSE => TRUE,
    Object::Null => TRUE,
    _ => FALSE,
  }
}

fn eval_minus_operator(target: &Object) -> Object {
  match target {
    &TRUE => TRUE,
    &FALSE => FALSE,
    &Object::Null => Object::Null,
    &Object::Integer(x) => Object::Integer(-x),
  }
}

fn bool_to_object(b: bool) -> Object {
  if b { TRUE } else { FALSE }
}

fn is_truthy(obj: &Object) -> bool {
  match obj {
    &Object::Null => false,
    &TRUE => true,
    &FALSE => false,
    _ => true,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{object::Object, parser::Parser};

  #[test]
  fn eval_integer() {
    let tests = vec![
      ("5;", Object::Integer(5)),
      ("10;", Object::Integer(10)),
      ("-5;", Object::Integer(-5)),
      ("-10;", Object::Integer(-10)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  #[test]
  fn eval_boolean() {
    let tests = vec![
      ("true;", Object::Boolean(true)),
      ("false;", Object::Boolean(false)),
      ("1 < 2", Object::Boolean(true)),
      ("1 > 2", Object::Boolean(false)),
      ("1 < 1", Object::Boolean(false)),
      ("1 > 1", Object::Boolean(false)),
      ("1 == 1", Object::Boolean(true)),
      ("1 != 1", Object::Boolean(false)),
      ("1 == 2", Object::Boolean(false)),
      ("1 != 2", Object::Boolean(true)),
      ("true == true", Object::Boolean(true)),
      ("false == false", Object::Boolean(true)),
      ("true == false", Object::Boolean(false)),
      ("true != false", Object::Boolean(true)),
      ("false != true", Object::Boolean(true)),
      ("(1 < 2) == true", Object::Boolean(true)),
      ("(1 < 2) == false", Object::Boolean(false)),
      ("(1 > 2) == true", Object::Boolean(false)),
      ("(1 > 2) == false", Object::Boolean(true)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  #[test]
  fn eval_minus_prefix_operator() {
    let tests = vec![
      ("-5;", Object::Integer(-5)),
      ("-true;", Object::Boolean(true)),
      ("-false;", Object::Boolean(false)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  #[test]
  fn eval_bang_operator() {
    let tests = vec![
      ("!true;", FALSE),
      ("!false;", TRUE),
      ("!5;", FALSE),
      ("!!true;", TRUE),
      ("!!false", FALSE),
      ("!!5", TRUE),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  #[test]
  fn eval_infix_with_integers() {
    let tests = vec![
      ("5", Object::Integer(5)),
      ("10", Object::Integer(10)),
      ("-5", Object::Integer(-5)),
      ("-10", Object::Integer(-10)),
      ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
      ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
      ("-50 + 100 + -50", Object::Integer(0)),
      ("5 * 2 + 10", Object::Integer(20)),
      ("5 + 2 * 10", Object::Integer(25)),
      ("20 + 2 * -10", Object::Integer(0)),
      ("50 / 2 * 2 + 10", Object::Integer(60)),
      ("2 * (5 + 10)", Object::Integer(30)),
      ("3 * 3 * 3 + 10", Object::Integer(37)),
      ("3 * (3 * 3) + 10", Object::Integer(37)),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  #[test]
  fn eval_if_else_expression() {
    let tests = vec![
      ("if (true) { 10 };", Object::Integer(10)),
      ("if (false) { 10 };", Object::Null),
      ("if (1) { 10 };", Object::Integer(10)),
      ("if (1 < 2) { 10 };", Object::Integer(10)),
      ("if (1 > 2) { 10 };", Object::Null),
      ("if (1 < 2) { 10 } else { 20 };", Object::Integer(10)),
      ("if (1 > 2) { 10 } else { 20 };", Object::Integer(20)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, obj);
    }
  }

  fn test_eval(input: &str) -> Object {
    let program = Parser::from(input).parse_program();
    eval(&program)
  }
}
