use super::ParsingResult;
use std::fmt::Display;

const OP_CHARS: &str = "+-*/";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTy {
    Program,
    Var,
    Comma,
    Colon,
    Integer,
    Float,
    IntegerConst,
    RealConst,
    Plus,
    Minus,
    Mul,
    IntegerDiv,
    RealDiv,
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
        let s = match self {
            TokenTy::Program => "Program",
            TokenTy::Var => "Var",
            TokenTy::Colon => "Colon",
            TokenTy::Comma => "Comma",
            TokenTy::Integer => "Integer",
            TokenTy::Mul => "Mul",
            TokenTy::Eof => "EOF",
            TokenTy::Plus => "Plus",
            TokenTy::LParen => "LParent",
            TokenTy::RParen => "RParent",
            TokenTy::Minus => "Minus",
            TokenTy::Id => "Id",
            TokenTy::Assign => "Assign",
            TokenTy::Begin => "Begin",
            TokenTy::End => "End",
            TokenTy::Semi => "Semi",
            TokenTy::Dot => "Dot",
            TokenTy::Float => "Float",
            TokenTy::IntegerConst => "IntegerValue",
            TokenTy::RealConst => "RealValue",
            TokenTy::IntegerDiv => "IntegerDiv",
            TokenTy::RealDiv => "RealDiv",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenTy,
    pub raw: String,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token<{} `{}` {}>",
            self.ty,
            self.raw,
            self.print_position()
        )
    }
}

impl Token {
    pub fn single_char(ty: TokenTy, ch: char, line: usize, pos: usize) -> Self {
        Self {
            ty,
            raw: ch.to_string(),
            line,
            start: pos,
            end: pos,
        }
    }

    pub fn parse_int(&self) -> ParsingResult<i64> {
        match self.ty {
            TokenTy::IntegerConst => self.raw.parse::<i64>().map_err(|e| format!("{}", e)),
            _ => Err(format!("can't cast {} to i64", self)),
        }
    }

    pub fn parse_float(&self) -> ParsingResult<f64> {
        match self.ty {
            TokenTy::RealConst => self.raw.parse::<f64>().map_err(|e| format!("{}", e)),
            _ => Err(format!("can't cast {} to f64", self)),
        }
    }

    pub fn print_position(&self) -> String {
        if self.start == self.end {
            format!("line {} col {}", self.line + 1, self.start + 1)
        } else {
            format!(
                "line {} col {} -> {}",
                self.line + 1,
                self.start + 1,
                self.end + 1
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer {
    pub text: String,
    pub pos: usize,
    pub current_line_pos: usize,
    pub current_line: usize,
    pub current_char: Option<char>,
    enter_newline: bool,
}

impl Lexer {
    pub fn new(text: String) -> Self {
        let current_char = text.chars().next();
        Self {
            text,
            pos: 0,
            current_line_pos: 0,
            current_line: 0,
            current_char,
            enter_newline: false,
        }
    }

    /// advance the 'pos' pointer and set the current_char
    pub fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.text.chars().nth(self.pos);
        if self.enter_newline {
            self.enter_newline = false;
            self.current_line += 1;
            self.current_line_pos = 0;
        } else {
            self.current_line_pos += 1;
        }
        if self.current_char == Some('\n') {
            self.enter_newline = true
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.text.chars().nth(self.pos + 1)
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance()
            } else {
                break;
            }
        }
    }

    pub fn skip_comment(&mut self) {
        while let Some(ch) = self.current_char {
            if ch != '}' {
                self.advance()
            } else {
                break;
            }
        }
        self.advance()
    }

    pub fn number(&mut self) -> ParsingResult<Token> {
        let mut raw = String::new();
        let start = self.current_line_pos;
        // should not handle '.' in first loop
        // otherwise "1.2.23" become valid token
        while let Some(ch) = self.current_char {
            if ch.is_digit(10) {
                raw.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let token = if self.current_char == Some('.') {
            raw.push(self.current_char.unwrap());
            self.advance();
            while let Some(ch) = self.current_char {
                if ch.is_digit(10) {
                    raw.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
            let end = self.current_line_pos;
            Token {
                ty: TokenTy::RealConst,
                raw,
                line: self.current_line,
                start,
                end,
            }
        } else {
            let end = self.current_line_pos;
            Token {
                ty: TokenTy::IntegerConst,
                raw,
                line: self.current_line,
                start,
                end,
            }
        };
        Ok(token)
    }

    pub fn _id(&mut self) -> ParsingResult<Token> {
        let mut id = String::new();
        let start = self.current_line_pos;
        while let Some(ch) = self.current_char {
            if ch.is_ascii_alphanumeric() {
                id.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let ty = match id.to_lowercase().as_str() {
            "program" => TokenTy::Program,
            "var" => TokenTy::Var,
            "integer" => TokenTy::Integer,
            "real" => TokenTy::Float,
            "div" => TokenTy::IntegerDiv,
            "begin" => TokenTy::Begin,
            "end" => TokenTy::End,
            _ => TokenTy::Id,
        };
        Ok(Token {
            ty,
            raw: id,
            line: self.current_line,
            start,
            end: self.current_line_pos,
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
                    if ch == '{' {
                        self.skip_comment();
                        continue;
                    }
                    if ch.is_ascii_alphabetic() {
                        break self._id();
                    }
                    if ch.is_digit(10) {
                        break self.number();
                    }
                    if ch == ',' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::Comma,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if ch == ':' && self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        break Ok(Token {
                            ty: TokenTy::Assign,
                            raw: ":=".to_string(),
                            line: self.current_line,
                            start: self.current_line_pos,
                            end: self.current_line_pos + 1,
                        });
                    }
                    if ch == ':' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::Colon,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if ch == ';' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::Semi,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if ch == '(' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::LParen,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if ch == ')' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::RParen,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if OP_CHARS.contains(ch) {
                        let ty = match ch {
                            '+' => TokenTy::Plus,
                            '-' => TokenTy::Minus,
                            '*' => TokenTy::Mul,
                            '/' => TokenTy::RealDiv,
                            _ => unreachable!(),
                        };
                        self.advance();
                        break Ok(Token::single_char(
                            ty,
                            ch,
                            self.current_line,
                            self.current_line_pos,
                        ));
                    }
                    if ch == '.' {
                        self.advance();
                        break Ok(Token::single_char(
                            TokenTy::Dot,
                            ch,
                            self.current_line,
                            self.pos,
                        ));
                    }
                    // meet unknown ch, generate dianoetic info
                    let source_subset: String = self
                        .text
                        .lines()
                        .nth(self.current_line)
                        .unwrap()
                        .chars()
                        .take(self.current_line_pos + 1)
                        .collect();
                    let ws = " ".repeat(source_subset.len() - 1);
                    let diagnose_info = format!(
                        "invalid char `{}` at position {} \n{}\n{}^",
                        ch, self.current_line_pos, source_subset, ws,
                    );
                    break Err(diagnose_info);
                }
                None => {
                    break Ok(Token {
                        ty: TokenTy::Eof,
                        raw: String::new(),
                        line: self.current_line,
                        start: self.current_line_pos,
                        end: self.current_line_pos,
                    })
                }
            }
        }
    }
}
