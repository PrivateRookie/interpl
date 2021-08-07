use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::mem::discriminant;

type ParsingResult<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Integer(i64),
    Plus,
    Minus,
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(val) => write!(f, "Token<INTEGER {}>", val),
            Token::Plus => write!(f, "PLUS"),
            Token::Minus => write!(f, "MINUS"),
            Token::EOF => write!(f, "EOF"),
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
        let current_char = text.chars().nth(0);
        Self {
            text,
            pos: 0,
            current_token: None,
            current_char,
        }
    }
}

/// Lexer code
impl Interpreter {
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
        loop {
            match self.current_char {
                Some(ch) => {
                    if ch.is_digit(10) {
                        num_str.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                }
                None => break,
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
                None => break Ok(Token::EOF),
            }
        }
    }
}

/// Parser/Interpreter code
impl Interpreter {
    fn eat(&mut self, token: &Token) -> ParsingResult<()> {
        if let Some(c_token) = &self.current_token {
            if discriminant(c_token) == discriminant(token) {
                self.current_token = Some(self.next_token()?);
                Ok(())
            } else {
                Err(format!("expect {}, found {}", token, c_token))
            }
        } else {
            Err(format!("consume token while in init state"))
        }
    }

    fn term(&mut self) -> ParsingResult<i64> {
        let token = self.current_token.clone().unwrap();
        self.eat(&Token::Integer(0))?;
        token.into_val()
    }

    fn expr(&mut self) -> ParsingResult<i64> {
        self.current_token = Some(self.next_token()?);
        let mut ret = self.term()?;
        loop {
            if let Some(current_token) = &self.current_token {
                match current_token {
                    Token::Integer(_) => return Err(format!("invalid token {}", current_token)),
                    Token::Plus => {
                        self.eat(&Token::Plus)?;
                        ret += self.term()?;
                    }
                    Token::Minus => {
                        self.eat(&Token::Minus)?;
                        ret -= self.term()?;
                    }
                    Token::EOF => break,
                }
            } else {
                break;
            }
        }
        Ok(ret)
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
