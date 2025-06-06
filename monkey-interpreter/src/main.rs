use std::io;

use monkey_interpreter::repl;

fn main() -> io::Result<()> {
  let mut username = whoami::username();
  username = username
    .chars()
    .next()
    .map(|c| c.to_uppercase().to_string() + &username[1..])
    .unwrap();

  println!(
    "Hello {}! This is the Monkey programming language!",
    username
  );
  println!("Feel free to type in commands");

  repl::start(io::stdin().lock(), io::stdout())
}
