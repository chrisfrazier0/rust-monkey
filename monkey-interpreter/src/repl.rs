use std::io::{self, BufRead, Write};

use crate::lexer::Lexer;
use crate::token::TokenType;

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(mut input: R, mut output: W) -> io::Result<()> {
  loop {
    write!(output, "{}", PROMPT)?;
    output.flush()?;

    let mut line = String::new();
    if input.read_line(&mut line)? == 0 {
      return Ok(());
    }

    let mut lexer = Lexer::new(line.trim());
    loop {
      let token = lexer.next_token();
      if token.token_type == TokenType::Eof {
        break;
      }
      writeln!(output, "{:?}", token)?;
    }
  }
}
