use super::{
    token::{Token, TokenTy},
    Lexer, ParsingResult, Visit,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub compound_statement: Compound,
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
pub struct Assignment<T: Visit> {
    pub var: Var,
    pub expr: NewExpr<T>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NewExpr<T: Visit> {
    Unary(Box<NewTerm<T>>),
    Binary(Box<BiOperate<NewTerm<T>>>),
}

impl <T: Visit> Visit for NewExpr<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NewTerm<T: Visit> {
    Unary(Box<Factor<T>>),
    Binary(Box<BiOperate<Factor<T>>>),
}

impl<T: Visit> Visit for NewTerm<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor<T: Visit> {
    UnaryOp(UnaryOperate<T>),
    Num(Num),
    Paren(NewExpr),
    Var(Var),
}

impl<T: Visit> Visit for Factor<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T: Visit> {
    BiOperate(Box<BiOperate<Expr<T>>>),
    UnaryOperate(Box<UnaryOperate<T>>),
    Num(Num),
    Var(Var),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BiOperator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiOperate<T: Visit> {
    pub op: BiOperator,
    pub left: T,
    pub right: T,
}

impl<T: Visit> Visit for BiOperate<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperate<T: Visit> {
    pub op: UnaryOperator,
    pub ele: T,
}

impl<T: Visit> Visit for UnaryOperate<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub val: i64,
}

impl Visit for Num {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub id: String,
}

impl Visit for Var {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
    }
}

impl<T: Visit> Visit for Expr<T> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        todo!()
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

    /// program : compound_statement DOT
    pub fn program(&mut self) -> ParsingResult<Program> {
        let compound_statement = self.compound_statement()?;
        self.eat(TokenTy::Dot)?;
        Ok(Program { compound_statement })
    }

    /// compound_statement : BEGIN statement_list END
    pub fn compound_statement(&mut self) -> ParsingResult<Compound> {
        self.eat(TokenTy::Begin)?;
        let children = self.statement_list()?;
        self.eat(TokenTy::End)?;
        Ok(Compound { children })
    }

    /// statement_list : statement | statement SEMI statement_list
    pub fn statement_list(&mut self) -> ParsingResult<Vec<Statement>> {
        let first = self.statement()?;
        let mut ret = vec![first];
        while let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Semi {
                self.eat(TokenTy::Semi)?;
                ret.push(self.statement()?)
            } else {
                break;
            }
        }
        Ok(ret)
    }

    /// statement : compound_statement
    ///           | assignment_statement
    ///           | empty
    pub fn statement(&mut self) -> ParsingResult<Statement> {
        if let Some(current_token) = &self.current_token {
            match current_token.ty {
                TokenTy::Begin => Ok(Statement::Compound(self.compound_statement()?)),
                TokenTy::Id => Ok(Statement::Assignment(self.assignment()?)),
                _ => Ok(Statement::Empty),
            }
        } else {
            Err("expect more token".to_string())
        }
    }

    /// assignment_statement : variable ASSIGN expr
    pub fn assignment(&mut self) -> ParsingResult<Assignment> {
        let var = self.variable()?;
        self.eat(TokenTy::Assign)?;
        let expr = self.expr()?;
        Ok(Assignment { var, expr })
    }

    /// expr: term ((PLUS | MINUS) term)*
    pub fn expr(&mut self) -> ParsingResult<NewExpr> {
        let left = self.term()?;
        if let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Plus {
                self.eat(TokenTy::Plus)?;
                let right = self.term()?;
                let op = BiOperator::Plus;
                Ok(NewExpr::Binary(Box::new(BiOperate { op, left, right })))
            } else if current_token.ty == TokenTy::Minus {
                self.eat(TokenTy::Minus)?;
                let right = self.term()?;
                let op = BiOperator::Minus;
                Ok(NewExpr::Binary(Box::new(BiOperate { op, left, right })))
            } else {
                Ok(NewExpr::Unary(Box::new(left)))
            }
        } else {
            Ok(NewExpr::Unary(Box::new(left)))
        }
    }

    /// term: factor ((MUL | DIV) factor)*
    pub fn term(&mut self) -> ParsingResult<NewTerm> {
        let left = self.factor()?;
        if let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Mul {
                self.eat(TokenTy::Mul)?;
                let right = self.factor()?;
                let op = BiOperator::Mul;
                Ok(NewTerm::Binary(Box::new(BiOperate { op, left, right })))
            } else if current_token.ty == TokenTy::Div {
                self.eat(TokenTy::Div)?;
                let right = self.factor()?;
                let op = BiOperator::Minus;
                Ok(NewTerm::Binary(Box::new(BiOperate { op, left, right })))
            } else {
                Ok(NewTerm::Unary(Box::new(left)))
            }
        } else {
            Ok(NewTerm::Unary(Box::new(left)))
        }
    }

    /// factor : PLUS factor
    ///        | MINUS factor
    ///        | INTEGER
    ///        | LPAREN expr RPAREN
    ///        | variable
    pub fn factor(&mut self) -> ParsingResult<Factor<impl Visit>> {
        let token = self.current_token.clone().unwrap();
        match token.ty {
            TokenTy::Plus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let factor = self.factor()?;
                Ok(Factor::UnaryOp(UnaryOperate {
                    op: UnaryOperator::Plus,
                    ele: factor,
                }))
            }
            TokenTy::Minus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let factor = self.factor()?;
                Ok(Factor::UnaryOp(UnaryOperate {
                    op: UnaryOperator::Minus,
                    ele: factor,
                }))
            }
            TokenTy::Integer => {
                log::debug!("[FACTOR] integer");
                self.eat(token.ty.clone())?;
                let val = token.parse_int()?;
                Ok(Factor::Num(Num { val }))
            }
            TokenTy::LParen => {
                log::debug!("[FACTOR] paren");
                self.eat(token.ty)?;
                let expr = self.expr()?;
                self.eat(TokenTy::RParen)?;
                Ok(Factor::Paren(expr))
            }
            TokenTy::Id => {
                log::debug!("[FACTOR] id");
                self.eat(token.ty)?;
                let var = self.variable()?;
                Ok(Factor::Var(var))
            }
            _ => Err(format!("expect factor, got {}", token)),
        }
    }

    pub fn variable(&mut self) -> ParsingResult<Var> {
        if let Some(current_token) = self.current_token.clone() {
            if current_token.ty == TokenTy::Id {
                self.eat(TokenTy::Id)?;
                Ok(Var {
                    id: current_token.raw,
                })
            } else {
                Err(format!("expect variable got {}", current_token))
            }
        } else {
            Err("expect more token".to_string())
        }
    }
}
