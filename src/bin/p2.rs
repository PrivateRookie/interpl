use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::mem::discriminant;

type ParsingResult<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Integer(i64),
    Plus,
    Minus,
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(val) => write!(f, "Token<INTEGER {}>", val),
            Token::Plus => write!(f, "PLUS"),
            Token::Minus => write!(f, "MINUS"),
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
    current_char: Option<char>,
}

impl Interpreter {
    fn new(text: String) -> Self {
        let current_char = text.chars().next();
        Self {
            text,
            pos: 0,
            current_token: None,
            current_char,
        }
    }

    /// advance the 'pos' pointer and set the current_char
    fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.text.chars().nth(self.pos);
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_some() && self.current_char.unwrap().is_whitespace() {
            self.advance()
        }
    }

    fn integer(&mut self) -> ParsingResult<Token> {
        let mut num_str = String::new();
        while let Some(ch) = self.current_char {
            if ch.is_digit(10) {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let num = num_str
            .parse()
            .map_err(|e| format!("invalid i64 value {}: {}", num_str, e))?;
        Ok(Token::Integer(num))
    }

    fn next_token(&mut self) -> ParsingResult<Token> {
        loop {
            match self.current_char {
                Some(ch) => {
                    if ch.is_whitespace() {
                        self.skip_whitespace();
                        continue;
                    }
                    if ch.is_digit(10) {
                        break self.integer();
                    }
                    if ch == '+' {
                        self.advance();
                        break Ok(Token::Plus);
                    }
                    if ch == '-' {
                        self.advance();
                        break Ok(Token::Minus);
                    }

                    break Err(format!("invalid char {}", ch));
                }
                None => break Ok(Token::Eof),
            }
        }
    }

    fn eat(&mut self, token: &Token) -> ParsingResult<()> {
        if let Some(c_token) = &self.current_token {
            if discriminant(c_token) == discriminant(token) {
                self.current_token = Some(self.next_token()?);
                Ok(())
            } else {
                Err(format!("expect {}, found {}", token, c_token))
            }
        } else {
            Err("consume token while in init state".to_string())
        }
    }

    fn expr(&mut self) -> ParsingResult<i64> {
        self.current_token = Some(self.next_token()?);

        let left = self.current_token.clone().unwrap();
        self.eat(&Token::Integer(0))?;

        let op = self.current_token.clone().unwrap();
        self.eat(&op)?;

        let right = self.current_token.clone().unwrap();
        self.eat(&Token::Integer(0))?;
        match op {
            Token::Plus => Ok(left.into_val()? + right.into_val()?),
            Token::Minus => Ok(left.into_val()? - right.into_val()?),
            _ => unreachable!(),
        }
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
