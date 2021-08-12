use super::{
    token::{Token, TokenTy},
    Lexer, ParsingResult, Visit,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BiOperator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiOperate {
    pub op: BiOperator,
    pub left: Expr,
    pub right: Expr,
}

impl Visit for BiOperate {
    fn visit(&self) -> i64 {
        let left = self.left.visit();
        let right = self.right.visit();
        match self.op {
            BiOperator::Plus => left + right,
            BiOperator::Minus => left - right,
            BiOperator::Mul => left * right,
            BiOperator::Div => left / right,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperate {
    pub op: UnaryOperator,
    pub expr: Expr,
}

impl Visit for UnaryOperate {
    fn visit(&self) -> i64 {
        match self.op {
            UnaryOperator::Plus => self.expr.visit(),
            UnaryOperator::Minus => -(self.expr.visit()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub val: i64,
}

impl Visit for Num {
    fn visit(&self) -> i64 {
        self.val
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound {
    pub children: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub var: String,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BiOperate(Box<BiOperate>),
    UnaryOperate(Box<UnaryOperate>),
    Num(Num),
}

impl Visit for Expr {
    fn visit(&self) -> i64 {
        match self {
            Expr::BiOperate(bin_op) => bin_op.visit(),
            Expr::UnaryOperate(unary_op) => unary_op.visit(),
            Expr::Num(num) => num.visit(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Option<Token>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token().ok();
        Self {
            lexer,
            current_token,
        }
    }

    pub fn eat(&mut self, token_ty: TokenTy) -> ParsingResult<()> {
        if let Some(c_token) = &self.current_token {
            if c_token.ty == token_ty {
                self.current_token = Some(self.lexer.next_token()?);
                Ok(())
            } else {
                Err(format!("expect {}, found {}", token_ty, c_token.ty))
            }
        } else {
            Err("consume token while in init state".to_string())
        }
    }

    /// factor : PLUS factor
    ///        | MINUS factor
    ///        | INTEGER
    ///        | LPAREN expr RPAREN
    ///        | variable
    pub fn factor(&mut self) -> ParsingResult<Expr> {
        let token = self.current_token.clone().unwrap();
        match token.ty {
            TokenTy::Plus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let expr = self.expr()?;
                Ok(Expr::UnaryOperate(Box::new(UnaryOperate {
                    op: UnaryOperator::Plus,
                    expr,
                })))
            }
            TokenTy::Minus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let expr = self.expr()?;
                Ok(Expr::UnaryOperate(Box::new(UnaryOperate {
                    op: UnaryOperator::Minus,
                    expr,
                })))
            }
            TokenTy::Integer => {
                log::debug!("[FACTOR] integer");
                self.eat(token.ty.clone())?;
                let val = token.parse_int()?;
                Ok(Expr::Num(Num { val }))
            }
            TokenTy::LParen => {
                log::debug!("[FACTOR] paren");
                self.eat(token.ty)?;
                let expr = self.expr()?;
                self.eat(TokenTy::RParen)?;
                Ok(expr)
            }
            TokenTy::Id => {
                log::debug!("[FACTOR] id");
                self.eat(token.ty)?;
                self.variable()
            }
            _ => Err(format!("expect integer or '(', got {}", token)),
        }
    }

    pub fn variable(&mut self) -> ParsingResult<String> {
        todo!()
    }

    pub fn term(&mut self) -> ParsingResult<Expr> {
        log::debug!("[TERM  ] parse left");
        let left = self.factor()?;
        log::debug!("[TERM  ] parse op");
        if let Some(current_token) = &self.current_token {
            match &current_token.ty {
                TokenTy::Mul => {
                    self.eat(TokenTy::Mul)?;
                    let right = self.factor()?;
                    Ok(Expr::BiOperate(Box::new(BiOperate {
                        op: BiOperator::Mul,
                        left,
                        right,
                    })))
                }
                TokenTy::Div => {
                    self.eat(TokenTy::Div)?;
                    let right = self.factor()?;
                    Ok(Expr::BiOperate(Box::new(BiOperate {
                        op: BiOperator::Div,
                        left,
                        right,
                    })))
                }
                TokenTy::Plus | TokenTy::Minus | TokenTy::Eof | TokenTy::RParen => Ok(left),
                _ => {
                    return Err(format!(
                        "parse term error, unexpected token {}",
                        current_token
                    ));
                }
            }
        } else {
            Err("enter empty token".to_string())
        }
    }

    pub fn expr(&mut self) -> ParsingResult<Expr> {
        log::debug!("[EXPR  ] parse left");
        let left = self.term()?;
        log::debug!("[EXPR  ] parse op");
        if let Some(current_token) = &self.current_token {
            match &current_token.ty {
                TokenTy::Plus => {
                    self.eat(TokenTy::Plus)?;
                    let right = self.term()?;
                    Ok(Expr::BiOperate(Box::new(BiOperate {
                        op: BiOperator::Plus,
                        left,
                        right,
                    })))
                }
                TokenTy::Minus => {
                    self.eat(TokenTy::Minus)?;
                    let right = self.term()?;
                    Ok(Expr::BiOperate(Box::new(BiOperate {
                        op: BiOperator::Minus,
                        left,
                        right,
                    })))
                }
                TokenTy::Eof | TokenTy::RParen => Ok(left),
                _ => {
                    return Err(format!(
                        "parse term error, unexpected token {}",
                        current_token
                    ));
                }
            }
        } else {
            Err("enter empty token".to_string())
        }
    }
}
