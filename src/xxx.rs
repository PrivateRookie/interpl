#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Integer(i64),
    Plus,
    Minus,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// parsed token value
    pub val: TokenValue,
    /// start position of token in source str
    pub pos: usize,
    /// len of token in source str
    pub len: usize,
}

impl Token {
    pub fn is_term(&self) -> bool {
        match self.val {
            TokenValue::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_operation(&self) -> bool {
        match self.val {
            TokenValue::Plus | TokenValue::Minus => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self.val {
            TokenValue::EOF => true,
            _ => false,
        }
    }
}

pub fn lexer(source: &str) -> Result<Vec<Token>, String> {
    let mut ret = vec![];
    let mut current_digit = String::new();
    let mut start_idx = 0;
    for (idx, ch) in source.chars().enumerate() {
        if ch.is_digit(10) {
            // record start pos when we encounter a new integer
            if current_digit.is_empty() {
                start_idx = idx
            }
            current_digit.push(ch);
        } else {
            if !current_digit.is_empty() {
                // digit is end, generate digit token and reset relative variable
                let val = current_digit.parse::<i64>().unwrap();
                ret.push(Token {
                    val: TokenValue::Integer(val),
                    pos: start_idx,
                    len: idx - start_idx,
                });
                current_digit.clear();
                start_idx = 0;
            }
            // keep going to consume whitespace
            if ch.is_whitespace() {
                continue;
            }

            if ch == '+' {
                ret.push(Token {
                    val: TokenValue::Plus,
                    pos: idx,
                    len: 1,
                });
                continue;
            }

            if ch == '-' {
                ret.push(Token {
                    val: TokenValue::Plus,
                    pos: idx,
                    len: 1,
                });
                continue;
            }

            return Err(format!("unknown ch {}", ch));
        }
    }
    ret.push(Token {
        val: TokenValue::EOF,
        pos: source.len(),
        len: 0,
    });
    Ok(ret)
}

/// integer token
#[derive(Debug, Clone, PartialEq)]
pub struct Term(Token);

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Term(Term),
    BiOp {
        op: Operation,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}



/// ## parse ast
/// expr -> term1 [+-] term2 | term
///
/// term -> integer
///
/// - 1 -> term -> expr
/// - 2 + 1 -> term plus term
pub fn parse(tokens: Vec<Token>) -> Result<Expr, String> {
    if tokens.is_empty() {
        return Err(format!("empty tokens"));
    }

    // let mut postfix = Vec::with_capacity(tokens.len());
    let mut term_stack: Vec<Token> = vec![];
    let mut op_stack: Vec<Token> = vec![];
    let end_idx = tokens.len() - 1;
    for (idx, token) in tokens.into_iter().enumerate() {

    }

    todo!()
}

#[test]
fn test_lexer() {
    dbg!(lexer("1 + 2 + 3 - 4").unwrap());
    dbg!(lexer("12-131-++").unwrap());
}

#[test]
fn test_parser() {
    let single_term_expr = lexer("1").unwrap();
    let bi_op_expr = lexer("1 + 1").unwrap();
    let multi = lexer("1 + 2 - 3 + 4").unwrap();
}

fn main() {}
