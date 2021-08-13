mod parser;
mod token;
use std::{
    collections::HashMap,
    io::{stdin, stdout, Write},
};

use parser::Parser;
use token::Lexer;

type ParsingResult<T> = Result<T, String>;
pub trait Visit {
    fn visit(&self, context: &mut HashMap<String, i64>) -> ParsingResult<i64>;
}
fn main() {
    pretty_env_logger::init();
    let mut input = String::new();
    let mut context = HashMap::new();
    loop {
        input.clear();
        context.clear();
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
                println!("{}", expr.visit(&mut context).unwrap());
            }
            Err(e) => println!("Error: {}", e),
        }
    }
}
