use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::mem::discriminant;

type ParsingResult<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Integer(i64),
    Plus,
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(val) => write!(f, "Token<INTEGER {}>", val),
            Token::Plus => write!(f, "PLUS"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

impl Token {
    fn into_val(self) -> ParsingResult<i64> {
        match self {
            Token::Integer(val) => Ok(val),
            _ => Err(format!("{} is not a integer term", self)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Interpreter {
    text: String,
    pos: usize,
    current_token: Option<Token>,
}

impl Interpreter {
    fn new(text: String) -> Self {
        Self {
            text,
            pos: 0,
            current_token: None,
        }
    }

    fn next_token(&mut self) -> ParsingResult<Token> {
        if self.pos > self.text.len() - 1 {
            return Ok(Token::Eof);
        }
        let current_char = self.text.chars().nth(self.pos).unwrap();
        if current_char.is_digit(10) {
            self.pos += 1;
            return Ok(Token::Integer(current_char.to_digit(10).unwrap().into()));
        }
        if current_char == '+' {
            self.pos += 1;
            return Ok(Token::Plus);
        }
        return Err(format!("invalid token {}", current_char));
    }

    fn eat(&mut self, token: &Token) -> ParsingResult<()> {
        if let Some(c_token) = &self.current_token {
            if discriminant(c_token) == discriminant(token) {
                self.current_token = Some(self.next_token()?);
                Ok(())
            } else {
                Err(format!("expect {}, found {}", token, c_token))
            }
            // alt compare impl
            // match (c_token, token) {
            //     (Token::Integer(_), Token::Integer(_)) => {
            //         self.current_token = Some(self.next_token()?);
            //         Ok(())
            //     }
            //     (Token::Plus, Token::Plus) => {
            //         self.current_token = Some(self.next_token()?);
            //         Ok(())
            //     }
            //     (Token::EOF, Token::EOF) => {
            //         self.current_token = Some(self.next_token()?);
            //         Ok(())
            //     }
            //     _ => Err(format!("expect {}, found {}", c_token, token)),
            // }
        } else {
            Err("consume token while in init state".to_string())
        }
    }

    fn expr(&mut self) -> ParsingResult<i64> {
        self.current_token = Some(self.next_token()?);

        let left = self.current_token.clone().unwrap();
        self.eat(&Token::Integer(0))?;

        let _op = self.current_token.clone().unwrap();
        self.eat(&Token::Plus)?;

        let right = self.current_token.clone().unwrap();
        self.eat(&Token::Integer(0))?;
        Ok(left.into_val()? + right.into_val()?)
    }
}

fn main() {
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
        let mut interpreter = Interpreter::new(source.to_string());
        match interpreter.expr() {
            Ok(ret) => {
                println!("{}", ret);
            }
            Err(e) => println!("Error: {}", e),
        }
    }
}
