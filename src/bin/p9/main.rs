mod parser;
mod token;
use std::io::{Write, stdin, stdout};

use token::Lexer;
use parser::Parser;

type ParsingResult<T> = Result<T, String>;
trait Visit {
    fn visit(&self) -> i64;
}
fn main() {
    pretty_env_logger::init();
    let mut input = String::new();
    loop {
        input.clear();
        print!("calc> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input).unwrap();
        let source = input.trim();
        if source.is_empty() {
            continue;
        }
        let lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(lexer);
        match parser.expr() {
            Ok(expr) => {
                println!("{}", expr.visit());
            }
            Err(e) => println!("Error: {}", e),
        }
    }
}
