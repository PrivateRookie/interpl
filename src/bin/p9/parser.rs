use log::debug;

use super::{
    token::{Token, TokenTy},
    Lexer, ParsingResult, Visit,
};

macro_rules! ast_debug {
    ($ast:ident) => {
        log::debug!("AST {:?}", $ast);
    };
}

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
    Unary(Term),
    Binary(BiOperate<Box<Expr>, Term>),
}

impl Visit for Expr {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Expr::Unary(term) => term.visit(context),
            Expr::Binary(binary) => binary.visit(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Unary(Factor),
    Binary(BiOperate<Box<Term>, Factor>),
}

impl Visit for Term {
    fn visit(&self, context: &mut std::collections::HashMap<String, i64>) -> ParsingResult<i64> {
        match self {
            Term::Unary(factor) => factor.visit(context),
            Term::Binary(binary) => binary.visit(context),
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
        debug!("parsing program...");
        let compound_statement = self.compound_statement()?;
        self.eat(TokenTy::Dot)?;
        let prog = Program { compound_statement };
        ast_debug!(prog);
        Ok(prog)
    }

    /// compound_statement : BEGIN statement_list END
    pub fn compound_statement(&mut self) -> ParsingResult<Compound> {
        debug!("parsing compound statement...");
        self.eat(TokenTy::Begin)?;
        let children = self.statement_list()?;
        self.eat(TokenTy::End)?;
        let compound = Compound { children };
        ast_debug!(compound);
        Ok(compound)
    }

    /// statement_list : statement | statement SEMI statement_list
    pub fn statement_list(&mut self) -> ParsingResult<Vec<Statement>> {
        debug!("parsing statement list...");
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
        ast_debug!(ret);
        Ok(ret)
    }

    /// statement : compound_statement
    ///           | assignment_statement
    ///           | empty
    pub fn statement(&mut self) -> ParsingResult<Statement> {
        debug!("parsing statement...");
        if let Some(current_token) = &self.current_token {
            let stmt = match current_token.ty {
                TokenTy::Begin => Statement::Compound(self.compound_statement()?),
                TokenTy::Id => Statement::Assignment(self.assignment()?),
                _ => Statement::Empty,
            };
            ast_debug!(stmt);
            Ok(stmt)
        } else {
            Err("expect more token".to_string())
        }
    }

    /// assignment_statement : variable ASSIGN expr
    pub fn assignment(&mut self) -> ParsingResult<Assignment> {
        debug!("parsing assignment statement...");
        let var = self.variable()?;
        self.eat(TokenTy::Assign)?;
        let expr = self.expr()?;
        let assign = Assignment { var, expr };
        ast_debug!(assign);
        Ok(assign)
    }

    /// expr: term ((PLUS | MINUS) term)*
    pub fn expr(&mut self) -> ParsingResult<Expr> {
        debug!("parsing expr...");
        let mut ret = Expr::Unary(self.term()?);
        while let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Plus {
                self.eat(TokenTy::Plus)?;
                let right = self.term()?;
                ret = Expr::Binary(BiOperate {
                    op: BiOperator::Plus,
                    left: Box::new(ret),
                    right,
                })
            } else if current_token.ty == TokenTy::Minus {
                self.eat(TokenTy::Minus)?;
                let right = self.term()?;
                ret = Expr::Binary(BiOperate {
                    op: BiOperator::Minus,
                    left: Box::new(ret),
                    right,
                })
            } else {
                break;
            }
        }
        ast_debug!(ret);
        Ok(ret)
    }

    /// term: factor ((MUL | DIV) factor)*
    pub fn term(&mut self) -> ParsingResult<Term> {
        debug!("parsing term...");
        let mut ret = Term::Unary(self.factor()?);
        while let Some(current_token) = &self.current_token {
            if current_token.ty == TokenTy::Mul {
                self.eat(TokenTy::Mul)?;
                let right = self.factor()?;
                ret = Term::Binary(BiOperate {
                    op: BiOperator::Mul,
                    left: Box::new(ret),
                    right,
                })
            } else if current_token.ty == TokenTy::Div {
                self.eat(TokenTy::Div)?;
                let right = self.factor()?;
                ret = Term::Binary(BiOperate {
                    op: BiOperator::Div,
                    left: Box::new(ret),
                    right,
                })
            } else {
                break;
            }
        }
        ast_debug!(ret);
        Ok(ret)
    }

    /// factor : PLUS factor
    ///        | MINUS factor
    ///        | INTEGER
    ///        | LPAREN expr RPAREN
    ///        | variable
    pub fn factor(&mut self) -> ParsingResult<Factor> {
        debug!("parsing factor...");
        if let Some(token) = self.current_token.clone() {
            match token.ty {
                TokenTy::Plus => {
                    self.eat(token.ty)?;
                    let factor = self.factor()?;
                    let plus_factor = Factor::UnaryOp(UnaryOperate {
                        op: UnaryOperator::Plus,
                        ele: Box::new(factor),
                    });
                    ast_debug!(plus_factor);
                    Ok(plus_factor)
                }
                TokenTy::Minus => {
                    self.eat(token.ty)?;
                    let factor = self.factor()?;
                    let minus_factor = Factor::UnaryOp(UnaryOperate {
                        op: UnaryOperator::Minus,
                        ele: Box::new(factor),
                    });
                    ast_debug!(minus_factor);
                    Ok(minus_factor)
                }
                TokenTy::Integer => {
                    self.eat(token.ty.clone())?;
                    let val = token.parse_int()?;
                    let int_factor = Factor::Num(Num { val });
                    ast_debug!(int_factor);
                    Ok(int_factor)
                }
                TokenTy::LParen => {
                    self.eat(token.ty)?;
                    let expr = self.expr()?;
                    self.eat(TokenTy::RParen)?;
                    let expr_factor = Factor::Paren(Box::new(expr));
                    ast_debug!(expr_factor);
                    Ok(expr_factor)
                }
                TokenTy::Id => {
                    let var = self.variable()?;
                    let var_factor = Factor::Var(var);
                    ast_debug!(var_factor);
                    Ok(var_factor)
                }
                _ => Err(format!("expect factor, got {}", token)),
            }
        } else {
            Err("expect more token".to_string())
        }
    }

    pub fn variable(&mut self) -> ParsingResult<Var> {
        debug!("parsing variable...");
        if let Some(current_token) = self.current_token.clone() {
            if current_token.ty == TokenTy::Id {
                self.eat(TokenTy::Id)?;
                let var = Var {
                    id: current_token.raw.to_lowercase(),
                };
                ast_debug!(var);
                Ok(var)
            } else {
                Err(format!("expect variable got {}", current_token))
            }
        } else {
            Err("expect more token".to_string())
        }
    }
}
