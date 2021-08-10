use std::fmt::Display;
use std::io::{stdin, stdout, Write};

type ParsingResult<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenTy {
    Integer,
    Mul,
    Div,
    EOF,
}

impl Display for TokenTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTy::Integer => write!(f, "INTEGER"),
            TokenTy::Mul => write!(f, "MUL"),
            TokenTy::Div => write!(f, "DIV"),
            TokenTy::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    ty: TokenTy,
    raw: String,
    start: usize,
    end: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token<{} {}>", self.ty, self.raw)
    }
}

impl Token {
    fn integer_token(raw: String, start: usize, end: usize) -> Self {
        Self {
            ty: TokenTy::Integer,
            raw,
            start,
            end,
        }
    }

    fn parse_int(&self) -> ParsingResult<i64> {
        match self.ty {
            TokenTy::Integer => self.raw.parse::<i64>().map_err(|e| format!("{}", e)),
            _ => Err(format!("can't cast {} to i64", self)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Lexer {
    text: String,
    pos: usize,
    current_char: Option<char>,
}

impl Lexer {
    fn new(text: String) -> Self {
        let current_char = text.chars().nth(0);
        Self {
            text,
            pos: 0,
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
        let mut raw = String::new();
        let start = self.pos;
        loop {
            match self.current_char {
                Some(ch) => {
                    if ch.is_digit(10) {
                        raw.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        let end = self.pos;
        let token = Token::integer_token(raw, start, end);
        Ok(token)
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
                    if ch == '*' {
                        self.advance();
                        break Ok(Token {
                            ty: TokenTy::Mul,
                            raw: ch.to_string(),
                            start: self.pos,
                            end: self.pos,
                        });
                    }
                    if ch == '/' {
                        self.advance();
                        break Ok(Token {
                            ty: TokenTy::Div,
                            raw: ch.to_string(),
                            start: self.pos,
                            end: self.pos,
                        });
                    }
                    let source_subset: String = self.text.chars().take(self.pos + 1).collect();
                    let ws = " ".repeat(source_subset.len() - 1);
                    let diagnose_info = format!(
                        "invalid char `{}` at position {} \n{}\n{}^",
                        ch, self.pos, source_subset, ws,
                    );
                    break Err(diagnose_info);
                }
                None => {
                    break Ok(Token {
                        ty: TokenTy::EOF,
                        raw: String::new(),
                        start: self.pos,
                        end: self.pos,
                    })
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Interpreter {
    lexer: Lexer,
    current_token: Option<Token>,
}

impl Interpreter {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token().ok();
        Self {
            lexer,
            current_token,
        }
    }
}

/// Parser/Interpreter code
impl Interpreter {
    fn eat(&mut self, token_ty: TokenTy) -> ParsingResult<()> {
        if let Some(c_token) = &self.current_token {
            if c_token.ty == token_ty {
                self.current_token = Some(self.lexer.next_token()?);
                Ok(())
            } else {
                Err(format!("expect {}, found {}", token_ty, c_token.ty))
            }
        } else {
            Err(format!("consume token while in init state"))
        }
    }

    fn factor(&mut self) -> ParsingResult<i64> {
        let token = self.current_token.clone().unwrap();
        self.eat(TokenTy::Integer)?;
        token.parse_int()
    }

    fn expr(&mut self) -> ParsingResult<i64> {
        let mut ret = self.factor()?;
        loop {
            if let Some(current_token) = &self.current_token {
                match current_token.ty {
                    TokenTy::Integer => return Err(format!("invalid token {}", current_token)),
                    TokenTy::Mul => {
                        self.eat(TokenTy::Mul)?;
                        ret *= self.factor()?;
                    }
                    TokenTy::Div => {
                        self.eat(TokenTy::Div)?;
                        ret /= self.factor()?;
                    }
                    TokenTy::EOF => break,
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
    println!("calc impl with more friendly error info.\n");
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
        let mut interpreter = Interpreter::new(lexer);
        match interpreter.expr() {
            Ok(ret) => {
                println!("{}", ret);
            }
            Err(e) => println!("Error: {}", e),
        }
    }
}
