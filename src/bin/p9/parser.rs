use super::{
    token::{Token, TokenTy},
    Lexer, ParsingResult, Visit,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub compound_statement: Compound,
}

impl Visit for Program {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        self.compound_statement.visit(context)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound {
    pub children: Vec<Statement>,
}

impl Visit for Compound {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        for child in self.children.iter() {
            child.visit(context)?;
        }
        Ok(0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    Empty,
}

impl Visit for Statement {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Statement::Compound(compound) => compound.visit(context),
            Statement::Assignment(assign) => assign.visit(context),
            Statement::Empty => Ok(0),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub var: Var,
    pub expr: Expr,
}

impl Visit for Assignment {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        let expr_val = self.expr.visit(context)?;
        context.insert(self.var.id.clone(), expr_val);
        Ok(expr_val)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Span(Term),
    Norm(BiOperate<Box<Expr>, Term>),
}

impl Visit for Expr {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Expr::Span(term) => term.visit(context),
            Expr::Norm(binary) => binary.visit(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Span(Factor),
    Norm(BiOperate<Box<Term>, Factor>),
}

impl Visit for Term {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Term::Span(factor) => factor.visit(context),
            Term::Norm(binary) => binary.visit(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    UnaryOp(UnaryOperate<Box<Factor>>),
    Num(Num),
    Paren(Box<Expr>),
    Var(Var),
}

impl Visit for Factor {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Factor::UnaryOp(unary) => unary.visit(context),
            Factor::Num(num) => Ok(num.val),
            Factor::Paren(expr) => expr.visit(context),
            Factor::Var(var) => var.visit(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BiOperator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiOperate<L: Visit, R: Visit> {
    pub op: BiOperator,
    pub left: L,
    pub right: R,
}

impl<L: Visit, R: Visit> Visit for BiOperate<L, R> {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        Ok(match self.op {
            BiOperator::Plus => self.left.visit(context)? + self.right.visit(context)?,
            BiOperator::Minus => self.left.visit(context)? - self.right.visit(context)?,
            BiOperator::Mul => self.left.visit(context)? * self.right.visit(context)?,
            BiOperator::Div => self.left.visit(context)? / self.right.visit(context)?,
        })
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
        match self.op {
            UnaryOperator::Plus => Ok(self.ele.visit(context)?),
            UnaryOperator::Minus => Ok(-self.ele.visit(context)?),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub val: i64,
}

impl Visit for Num {
    fn visit(&self, _context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        Ok(self.val)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub id: String,
}

impl Visit for Var {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        context
            .get(&self.id)
            .cloned()
            .ok_or(format!("{} is not found", self.id))
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
                Err(format!(
                    "consume token failed, expect {}, found {}",
                    token_ty, c_token.ty
                ))
            }
        } else {
            Err("consume token while in init state".to_string())
        }
    }

    /// program : compound_statement DOT
    pub fn program(&mut self) -> ParsingResult<Program> {
        log::debug!("[PROGOM]");
        let compound_statement = self.compound_statement()?;
        self.eat(TokenTy::Dot)?;
        Ok(Program { compound_statement })
    }

    /// compound_statement : BEGIN statement_list END
    pub fn compound_statement(&mut self) -> ParsingResult<Compound> {
        log::debug!("[COMPOUND]");
        self.eat(TokenTy::Begin)?;
        let children = self.statement_list()?;
        self.eat(TokenTy::End)?;
        Ok(Compound { children })
    }

    /// statement_list : statement | statement SEMI statement_list
    pub fn statement_list(&mut self) -> ParsingResult<Vec<Statement>> {
        log::debug!("[STMT_LIST]");
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
        log::debug!("[STMT   ]");
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
        log::debug!("[ASSIGN]");
        let var = self.variable()?;
        self.eat(TokenTy::Assign)?;
        let expr = self.expr()?;
        Ok(Assignment { var, expr })
    }

    /// expr: term ((PLUS | MINUS) term)*
    pub fn expr(&mut self) -> ParsingResult<Expr> {
        log::debug!("[EXPR   ]");
        let mut ret = Expr::Span(self.term()?);
        while let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Plus {
                self.eat(TokenTy::Plus)?;
                let right = self.term()?;
                ret = Expr::Norm(BiOperate {
                    op: BiOperator::Plus,
                    left: Box::new(ret),
                    right,
                })
            } else if current_token.ty == TokenTy::Minus {
                self.eat(TokenTy::Minus)?;
                let right = self.term()?;
                ret = Expr::Norm(BiOperate {
                    op: BiOperator::Minus,
                    left: Box::new(ret),
                    right,
                })
            } else {
                break;
            }
        }
        Ok(ret)
    }

    /// term: factor ((MUL | DIV) factor)*
    pub fn term(&mut self) -> ParsingResult<Term> {
        log::debug!("[TERM   ]");
        let mut ret = Term::Span(self.factor()?);
        while let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Mul {
                self.eat(TokenTy::Mul)?;
                let right = self.factor()?;
                ret = Term::Norm(BiOperate {
                    op: BiOperator::Mul,
                    left: Box::new(ret),
                    right,
                })
            } else if current_token.ty == TokenTy::Div {
                self.eat(TokenTy::Div)?;
                let right = self.factor()?;
                ret = Term::Norm(BiOperate {
                    op: BiOperator::Div,
                    left: Box::new(ret),
                    right,
                })
            } else {
                break;
            }
        }
        Ok(ret)
    }

    /// factor : PLUS factor
    ///        | MINUS factor
    ///        | INTEGER
    ///        | LPAREN expr RPAREN
    ///        | variable
    pub fn factor(&mut self) -> ParsingResult<Factor> {
        let token = self.current_token.clone().unwrap();
        match token.ty {
            TokenTy::Plus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let factor = self.factor()?;
                Ok(Factor::UnaryOp(UnaryOperate {
                    op: UnaryOperator::Plus,
                    ele: Box::new(factor),
                }))
            }
            TokenTy::Minus => {
                log::debug!("[FACTOR] unary");
                self.eat(token.ty)?;
                let factor = self.factor()?;
                Ok(Factor::UnaryOp(UnaryOperate {
                    op: UnaryOperator::Minus,
                    ele: Box::new(factor),
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
                Ok(Factor::Paren(Box::new(expr)))
            }
            TokenTy::Id => {
                log::debug!("[FACTOR] id");
                let var = self.variable()?;
                Ok(Factor::Var(var))
            }
            _ => Err(format!("expect factor, got {}", token)),
        }
    }

    pub fn variable(&mut self) -> ParsingResult<Var> {
        log::debug!("[VAR   ]");
        if let Some(current_token) = self.current_token.clone() {
            if current_token.ty == TokenTy::Id {
                self.eat(TokenTy::Id)?;
                Ok(Var {
                    id: current_token.raw.to_lowercase(),
                })
            } else {
                Err(format!("expect variable got {}", current_token))
            }
        } else {
            Err("expect more token".to_string())
        }
    }
}
