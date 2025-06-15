use std::mem::discriminant;

use crate::{
  ast::{
    BlockStatement, Boolean, ExpressionStatement, Identifier, IfExpression, InfixExpression,
    Integer, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement,
  },
  object::{Environment, Object, Value},
};

pub fn eval(node: &dyn Node, env: &mut Environment) -> Value {
  // Statements
  if let Some(p) = node.as_any().downcast_ref::<Program>() {
    return eval_statements(&p.statements, env);
  } else if let Some(bs) = node.as_any().downcast_ref::<BlockStatement>() {
    return eval_statements(&bs.statements, env);
  } else if let Some(rs) = node.as_any().downcast_ref::<ReturnStatement>() {
    if let Some(ref val) = rs.value {
      return eval(val.as_ref(), env).into_return();
    }
  } else if let Some(ls) = node.as_any().downcast_ref::<LetStatement>() {
    if let Some(ref val) = ls.value {
      let val = eval(val.as_ref(), env);
      if matches!(val, Value::Error(_)) {
        return val;
      }
      env.insert(&ls.name.value, val);
      return Value::Wrap(Object::Null);
    }
  } else if let Some(es) = node.as_any().downcast_ref::<ExpressionStatement>() {
    if let Some(ref expr) = es.expr {
      return eval(expr.as_ref(), env);
    }
  }

  // Expressions
  if let Some(prefix) = node.as_any().downcast_ref::<PrefixExpression>() {
    if let Some(ref right) = prefix.right {
      let right = eval(right.as_ref(), env);
      if matches!(right, Value::Error(_)) {
        return right;
      }
      return eval_prefix_expression(&prefix.operator, &right);
    }
  } else if let Some(infix) = node.as_any().downcast_ref::<InfixExpression>() {
    if let Some(ref right) = infix.right {
      let left = eval(infix.left.as_ref(), env);
      if matches!(left, Value::Error(_)) {
        return left;
      }
      let right = eval(right.as_ref(), env);
      if matches!(right, Value::Error(_)) {
        return right;
      }
      return eval_infix_expression(&infix.operator, &left, &right);
    }
  } else if let Some(if_expr) = node.as_any().downcast_ref::<IfExpression>() {
    return eval_if_expression(if_expr, env);
  } else if let Some(il) = node.as_any().downcast_ref::<Integer>() {
    return Value::Wrap(Object::Integer(il.value));
  } else if let Some(b) = node.as_any().downcast_ref::<Boolean>() {
    return bool_to_value(b.value);
  } else if let Some(ident) = node.as_any().downcast_ref::<Identifier>() {
    match env.get(&ident.value) {
      Some(val) => return val.clone(),
      None => return Value::Error(format!("identifier not found: {}", ident.value)),
    }
  }

  // Default
  Value::Error(format!("unknown node type: {}", node))
}

fn eval_statements(statements: &Vec<Box<dyn Statement>>, env: &mut Environment) -> Value {
  let mut result = Value::Wrap(Object::Null);
  for stmt in statements {
    result = eval(stmt.as_ref(), env);
    if matches!(result, Value::Return(_)) || matches!(result, Value::Error(_)) {
      return result;
    }
  }
  result
}

fn eval_prefix_expression(op: &str, right: &Value) -> Value {
  match op {
    "!" => eval_bang_operator(right),
    "-" => eval_minus_operator(right),
    _ => Value::Error(format!("unknown operator: {}{}", op, right)),
  }
}

fn eval_infix_expression(op: &str, left: &Value, right: &Value) -> Value {
  match (left.unbox(), right.unbox()) {
    (Some(Object::Integer(l)), Some(Object::Integer(r))) => eval_infix_integers(op, l, r),
    (Some(Object::Boolean(l)), Some(Object::Boolean(r))) => eval_infix_booleans(op, l, r),
    (Some(l), Some(r)) => {
      if discriminant(l) != discriminant(r) {
        Value::Error(format!("type mismatch: {} {} {}", left, op, right))
      } else {
        Value::Error(format!("unknown operator: {} {} {}", left, op, right))
      }
    }
    _ => Value::Error(format!("unknown operator: {} {} {}", left, op, right)),
  }
}

fn eval_infix_integers(op: &str, left: &i32, right: &i32) -> Value {
  match op {
    "+" => Value::Wrap(Object::Integer(left + right)),
    "-" => Value::Wrap(Object::Integer(left - right)),
    "*" => Value::Wrap(Object::Integer(left * right)),
    "/" => Value::Wrap(Object::Integer(left / right)),
    "<" => bool_to_value(left < right),
    ">" => bool_to_value(left > right),
    "==" => bool_to_value(left == right),
    "!=" => bool_to_value(left != right),
    _ => Value::Error(format!("unknown operator: {} {} {}", left, op, right)),
  }
}

fn eval_infix_booleans(op: &str, left: &bool, right: &bool) -> Value {
  match op {
    "==" => bool_to_value(left == right),
    "!=" => bool_to_value(left != right),
    _ => Value::Error(format!("unknown operator: {} {} {}", left, op, right)),
  }
}

pub fn eval_if_expression(node: &IfExpression, env: &mut Environment) -> Value {
  let Some(condition) = &node.condition else {
    return Value::Wrap(Object::Null);
  };
  let condition_obj = eval(condition.as_ref(), env);
  if matches!(condition_obj, Value::Error(_)) {
    return condition_obj;
  }
  if is_truthy(&condition_obj) {
    return eval(&node.consequence, env);
  } else if let Some(alt) = &node.alternative {
    return eval(alt, env);
  }
  Value::Wrap(Object::Null)
}

fn eval_bang_operator(target: &Value) -> Value {
  match target.unbox() {
    Some(Object::Integer(0)) => Value::Wrap(Object::Boolean(true)),
    Some(Object::Boolean(true)) => Value::Wrap(Object::Boolean(false)),
    Some(Object::Boolean(false)) => Value::Wrap(Object::Boolean(true)),
    Some(Object::Null) => Value::Error("unknown operator: !null".to_string()),
    None => Value::Error("unknown operator: !Error".to_string()),
    _ => Value::Wrap(Object::Boolean(false)),
  }
}

fn eval_minus_operator(target: &Value) -> Value {
  match target.unbox() {
    Some(Object::Integer(x)) => Value::Wrap(Object::Integer(-x)),
    None => Value::Error("unknown operator: -Error".to_string()),
    _ => Value::Error(format!("unknown operator: -{}", target)),
  }
}

fn bool_to_value(b: bool) -> Value {
  if b {
    Value::Wrap(Object::Boolean(true))
  } else {
    Value::Wrap(Object::Boolean(false))
  }
}

fn is_truthy(obj: &Value) -> bool {
  match obj.unbox() {
    None => false,
    Some(Object::Null) => false,
    Some(Object::Integer(0)) => false,
    Some(Object::Boolean(true)) => true,
    Some(Object::Boolean(false)) => false,
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
      assert_eq!(result, Value::Wrap(obj));
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
      assert_eq!(result, Value::Wrap(obj));
    }
  }

  #[test]
  fn eval_minus_prefix_operator() {
    let tests = vec![("-5;", Object::Integer(-5)), ("--5;", Object::Integer(5))];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, Value::Wrap(obj));
    }
  }

  #[test]
  fn eval_bang_operator() {
    let tests = vec![
      ("!true;", Object::Boolean(false)),
      ("!false;", Object::Boolean(true)),
      ("!5;", Object::Boolean(false)),
      ("!!true;", Object::Boolean(true)),
      ("!!false", Object::Boolean(false)),
      ("!!5", Object::Boolean(true)),
      ("!!0", Object::Boolean(false)),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, Value::Wrap(obj));
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
      assert_eq!(result, Value::Wrap(obj));
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
      assert_eq!(result, Value::Wrap(obj));
    }
  }

  #[test]
  fn eval_return_statements() {
    let tests = vec![
      ("return 10;", Object::Integer(10)),
      ("return 10; 9;", Object::Integer(10)),
      ("return 2*5; 9;", Object::Integer(10)),
      ("9; return 2 * 5; 9;", Object::Integer(10)),
      (
        r#"
        if (10 > 1) {
          if (10 > 1) {
            return 10;
          }
          return 1;
        };
        "#,
        Object::Integer(10),
      ),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, Value::Return(obj));
    }
  }

  #[test]
  fn eval_error_handling() {
    let tests = vec![
      ("5 + true;", "type mismatch: 5 + true"),
      ("5 + true; 5;", "type mismatch: 5 + true"),
      ("-true", "unknown operator: -true"),
      ("true + false;", "unknown operator: true + false"),
      ("5; true + false; 5", "unknown operator: true + false"),
      (
        "if 10 > 1 { true + false; }",
        "unknown operator: true + false",
      ),
      (
        r#"
        if (10 > 1) {
          if (10 > 1) {
            return true + false;
          }
          return 1;
        };
        "#,
        "unknown operator: true + false",
      ),
      ("foobar", "identifier not found: foobar"),
    ];
    for (input, expected) in tests {
      let result = test_eval(input);
      if let Value::Error(err) = result {
        assert_eq!(err, expected);
      } else {
        panic!("Expected an error, got: {:?}", result);
      }
    }
  }

  #[test]
  fn eval_let_statements() {
    let tests = vec![
      ("let a = 5; a;", Object::Integer(5)),
      ("let a = 5 * 5; a;", Object::Integer(25)),
      ("let a = 5; let b = a; b;", Object::Integer(5)),
      (
        "let a = 5; let b = a; let c = a + b + 5; c;",
        Object::Integer(15),
      ),
    ];
    for (input, obj) in tests {
      let result = test_eval(input);
      assert_eq!(result, Value::Wrap(obj));
    }
  }

  fn test_eval(input: &str) -> Value {
    let program = Parser::from(input).parse_program();
    let mut env = Environment::new();
    eval(&program, &mut env)
  }
}
