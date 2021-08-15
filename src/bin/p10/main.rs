mod parser;
mod token;
use std::{collections::HashMap, fmt::Display, fs::File, io::Read, process::exit};

use parser::Parser;
use token::Lexer;

type ParsingResult<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq)]
pub enum VisitRet {
    Float(f64),
    Int(i64),
    None,
}

impl Display for VisitRet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VisitRet::Float(val) => write!(f, "{}", val),
            VisitRet::Int(val) => write!(f, "{}", val),
            VisitRet::None => write!(f, "None"),
        }
    }
}

pub trait Visit {
    fn visit(&self, context: &mut HashMap<String, VisitRet>) -> ParsingResult<VisitRet>;
}

impl<T: Visit> Visit for Box<T> {
    fn visit(&self, context: &mut HashMap<String, VisitRet>) -> ParsingResult<VisitRet> {
        self.as_ref().visit(context)
    }
}

const USAGE: &str = "usage: prog <FILE>
<FILE> pascal program file
";

fn main() {
    pretty_env_logger::init();
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("{}", USAGE);
        exit(1)
    }
    let file_path = args.get(1).unwrap();
    let mut content = String::new();
    let mut file = File::open(file_path).unwrap();
    file.read_to_string(&mut content).unwrap();
    let lexer = Lexer::new(content);
    let mut parser = Parser::new(lexer);
    let mut context = HashMap::new();
    match parser.program() {
        Ok(prop) => {
            if let Err(err) = prop.visit(&mut context) {
                println!("Error: {}", err);
                exit(1)
            } else {
                println!("execution result:");
                for (var, val) in context {
                    println!("{} := {}", var, val);
                }
            }
        }
        Err(err) => {
            println!("Error: {}", err);
            exit(1)
        }
    }
}
