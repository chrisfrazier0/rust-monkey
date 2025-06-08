use std::io::{self, BufRead, Write};

use crate::parser::Parser;

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start<R: BufRead, W: Write>(mut input: R, mut output: W) -> io::Result<()> {
  loop {
    write!(output, "{}", PROMPT)?;
    output.flush()?;

    let mut line = String::new();
    if input.read_line(&mut line)? == 0 {
      return Ok(());
    }

    let mut parser = Parser::from(&line);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
      writeln!(
        output,
        "{}Woops! We ran into some monkey business here!\nParser errors:",
        MONKEY_FACE
      )?;
      for msg in parser.errors {
        writeln!(output, "  - {}", msg)?;
      }
      continue;
    }

    writeln!(output, "{}", program)?;
  }
}
