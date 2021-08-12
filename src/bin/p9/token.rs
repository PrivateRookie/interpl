use super::ParsingResult;
use std::fmt::Display;

const OP_CHARS: &str = "+-*/";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTy {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
    Id,
    Assign,
    Begin,
    End,
    Semi,
    Dot,
    Eof,
}

impl Display for TokenTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTy::Integer => write!(f, "Integer"),
            TokenTy::Mul => write!(f, "Mul"),
            TokenTy::Div => write!(f, "Div"),
            TokenTy::Eof => write!(f, "EOF"),
            TokenTy::Plus => write!(f, "Plus"),
            TokenTy::LParen => write!(f, "LParent"),
            TokenTy::RParen => write!(f, "RParent"),
            TokenTy::Minus => write!(f, "Minus"),
            TokenTy::Id => write!(f, "Id"),
            TokenTy::Assign => write!(f, "Assign"),
            TokenTy::Begin => write!(f, "Begin"),
            TokenTy::End => write!(f, "End"),
            TokenTy::Semi => write!(f, "Semi"),
            TokenTy::Dot => write!(f, "Dot"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenTy,
    pub raw: String,
    pub start: usize,
    pub end: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token<{} `{}` @{}:{}>",
            self.ty, self.raw, self.start, self.end
        )
    }
}

impl Token {
    pub fn integer_token(raw: String, start: usize, end: usize) -> Self {
        Self {
            ty: TokenTy::Integer,
            raw,
            start,
            end,
        }
    }

    pub fn single_char(ty: TokenTy, ch: char, pos: usize) -> Self {
        Self {
            ty,
            raw: ch.to_string(),
            start: pos,
            end: pos,
        }
    }

    pub fn parse_int(&self) -> ParsingResult<i64> {
        match self.ty {
            TokenTy::Integer => self.raw.parse::<i64>().map_err(|e| format!("{}", e)),
            _ => Err(format!("can't cast {} to i64", self)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer {
    pub text: String,
    pub pos: usize,
    pub current_char: Option<char>,
}

impl Lexer {
    pub fn new(text: String) -> Self {
        let current_char = text.chars().next();
        Self {
            text,
            pos: 0,
            current_char,
        }
    }

    /// advance the 'pos' pointer and set the current_char
    pub fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.text.chars().nth(self.pos);
    }

    pub fn peek(&self) -> Option<char> {
        self.text.chars().nth(self.pos + 1)
    }

    pub fn skip_whitespace(&mut self) {
        while self.current_char.is_some() && self.current_char.unwrap().is_whitespace() {
            self.advance()
        }
    }

    pub fn integer(&mut self) -> ParsingResult<Token> {
        let mut raw = String::new();
        let start = self.pos;
        while let Some(ch) = self.current_char {
            if ch.is_digit(10) {
                raw.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let end = self.pos;
        let token = Token::integer_token(raw, start, end);
        Ok(token)
    }

    pub fn _id(&mut self) -> ParsingResult<Token> {
        let mut id = String::new();
        let start = self.pos;
        while let Some(ch) = self.current_char {
            if ch.is_ascii_alphanumeric() {
                id.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let ty = match id.as_str() {
            "BEGIN" => TokenTy::Begin,
            "END" => TokenTy::End,
            _ => TokenTy::Id,
        };
        Ok(Token {
            ty,
            raw: id,
            start,
            end: self.pos,
        })
    }

    pub fn next_token(&mut self) -> ParsingResult<Token> {
        loop {
            match self.current_char {
                Some(ch) => {
                    if ch.is_whitespace() {
                        self.skip_whitespace();
                        continue;
                    }
                    if ch.is_ascii_alphabetic() {
                        break self._id();
                    }
                    if ch.is_digit(10) {
                        break self.integer();
                    }
                    if ch == ':' && self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        break Ok(Token {
                            ty: TokenTy::Assign,
                            raw: ":=".to_string(),
                            start: self.pos,
                            end: self.pos + 1,
                        });
                    }
                    if ch == ';' {
                        self.advance();
                        break Ok(Token::single_char(TokenTy::Semi, ch, self.pos));
                    }
                    if ch == '(' {
                        self.advance();
                        break Ok(Token::single_char(TokenTy::LParen, ch, self.pos));
                    }
                    if ch == ')' {
                        self.advance();
                        break Ok(Token::single_char(TokenTy::RParen, ch, self.pos));
                    }
                    if OP_CHARS.contains(ch) {
                        let ty = match ch {
                            '+' => TokenTy::Plus,
                            '-' => TokenTy::Minus,
                            '*' => TokenTy::Mul,
                            '/' => TokenTy::Div,
                            _ => unreachable!(),
                        };
                        self.advance();
                        break Ok(Token::single_char(ty, ch, self.pos));
                    }
                    if ch == '.' {
                        self.advance();
                        break Ok(Token::single_char(TokenTy::Dot, ch, self.pos));
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
                        ty: TokenTy::Eof,
                        raw: String::new(),
                        start: self.pos,
                        end: self.pos,
                    })
                }
            }
        }
    }
}
