use log::debug;

use super::{
    token::{Token, TokenTy},
    Lexer, ParsingResult, Visit, VisitRet,
};

macro_rules! ast_debug {
    ($ast:ident) => {
        log::debug!("AST {:?}", $ast);
    };
}

/// handle VisitRet operations
macro_rules! visit_ret_op {
    ($left:ident, $op:ident, $right:ident) => {
        match ($left, $right) {
            (VisitRet::Float(l), VisitRet::Float(r)) => {
                if $op == "+" {
                    Ok(VisitRet::Float(l + r))
                } else if $op == "-" {
                    Ok(VisitRet::Float(l - r))
                } else if $op == "*" {
                    Ok(VisitRet::Float(l * r))
                } else if $op == "//" {
                    Ok(VisitRet::Int((l / r) as i64))
                } else if $op == "/" {
                    Ok(VisitRet::Float(l / r))
                } else {
                    Err(format!("unknown operation {}", $op))
                }
            }
            (VisitRet::Float(l), VisitRet::Int(r)) => {
                if $op == "+" {
                    Ok(VisitRet::Float(l + r as f64))
                } else if $op == "-" {
                    Ok(VisitRet::Float(l - r as f64))
                } else if $op == "*" {
                    Ok(VisitRet::Float(l * r as f64))
                } else if $op == "//" {
                    Ok(VisitRet::Int((l / r as f64) as i64))
                } else if $op == "/" {
                    Ok(VisitRet::Float(l / r as f64))
                } else {
                    Err(format!("unknown operation {}", $op))
                }
            }
            (VisitRet::Float(l), VisitRet::None) => Err(format!("None can't {} with {}", $op, l)),
            (VisitRet::Int(l), VisitRet::Float(r)) => {
                if $op == "+" {
                    Ok(VisitRet::Float(l as f64 + r))
                } else if $op == "-" {
                    Ok(VisitRet::Float(l as f64 - r))
                } else if $op == "*" {
                    Ok(VisitRet::Float(l as f64 * r))
                } else if $op == "//" {
                    Ok(VisitRet::Int((l as f64 / r) as i64))
                } else if $op == "/" {
                    Ok(VisitRet::Float(l as f64 / r))
                } else {
                    Err(format!("unknown operation {}", $op))
                }
            }
            (VisitRet::Int(l), VisitRet::Int(r)) => {
                if $op == "+" {
                    Ok(VisitRet::Int(l + r))
                } else if $op == "-" {
                    Ok(VisitRet::Int(l - r))
                } else if $op == "*" {
                    Ok(VisitRet::Int(l * r))
                } else if $op == "//" {
                    Ok(VisitRet::Int(l / r))
                } else if $op == "/" {
                    Ok(VisitRet::Float((l as f64) / (r as f64)))
                } else {
                    Err(format!("unknown operation {}", $op))
                }
            }
            (VisitRet::Int(l), VisitRet::None) => Err(format!("None can't {} with {}", $op, l)),
            (VisitRet::None, VisitRet::Float(r)) => Err(format!("None can't {} with {}", $op, r)),
            (VisitRet::None, VisitRet::Int(r)) => Err(format!("None can't {} with {}", $op, r)),
            (VisitRet::None, VisitRet::None) => {
                if $op == "+" {
                    Ok(VisitRet::None)
                } else if $op == "-" {
                    Ok(VisitRet::None)
                } else if $op == "*" {
                    Ok(VisitRet::None)
                } else if $op == "//" {
                    Ok(VisitRet::None)
                } else if $op == "/" {
                    Ok(VisitRet::None)
                } else {
                    Err(format!("unknown operation {}", $op))
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub name: String,
    pub block: Block,
}

impl Visit for Program {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        self.block.visit(context)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub declarations: Vec<Declaration>,
    pub compound_statement: Compound,
}

impl Visit for Block {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        for declare in self.declarations.iter() {
            declare.visit(context)?;
        }
        self.compound_statement.visit(context)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Integer,
    Real,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub ids: Vec<Var>,
    pub ty: VarType,
}

impl Visit for Declaration {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        let default = match self.ty {
            VarType::Integer => VisitRet::Int(0),
            VarType::Real => VisitRet::Float(0.0),
        };
        for id in self.ids.iter().map(|var| var.id.clone()) {
            context.insert(id, default.clone());
        }
        Ok(default)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound {
    pub children: Vec<Statement>,
}

impl Visit for Compound {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        for child in self.children.iter() {
            child.visit(context)?;
        }
        Ok(VisitRet::None)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    Empty,
}

impl Visit for Statement {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        match self {
            Statement::Compound(compound) => compound.visit(context),
            Statement::Assignment(assign) => assign.visit(context),
            Statement::Empty => Ok(VisitRet::None),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub var: Var,
    pub expr: Expr,
}

impl Visit for Assignment {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        let expr_val = self.expr.visit(context)?;
        context.insert(self.var.id.clone(), expr_val.clone());
        Ok(expr_val)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unary(Term),
    Binary(BiOperate<Box<Expr>, Term>),
}

impl Visit for Expr {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
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
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        match self {
            Term::Unary(factor) => factor.visit(context),
            Term::Binary(binary) => binary.visit(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    UnaryOp(UnaryOperate<Box<Factor>>),
    Integer(i64),
    Float(f64),
    Paren(Box<Expr>),
    Var(Var),
}

impl Visit for Factor {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        match self {
            Factor::UnaryOp(unary) => unary.visit(context),
            Factor::Integer(val) => Ok(VisitRet::Int(*val)),
            Factor::Paren(expr) => expr.visit(context),
            Factor::Var(var) => var.visit(context),
            Factor::Float(val) => Ok(VisitRet::Float(*val)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BiOperator {
    Plus,
    Minus,
    Mul,
    IntegerDiv,
    FloatDiv,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiOperate<L: Visit, R: Visit> {
    pub op: BiOperator,
    pub left: L,
    pub right: R,
}

impl<L: Visit, R: Visit> Visit for BiOperate<L, R> {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        let left = self.left.visit(context)?;
        let right = self.right.visit(context)?;
        let op = match self.op {
            BiOperator::Plus => "+",
            BiOperator::Minus => "-",
            BiOperator::Mul => "*",
            BiOperator::IntegerDiv => "//",
            BiOperator::FloatDiv => "/",
        };
        visit_ret_op!(left, op, right)
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
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
        match self.op {
            UnaryOperator::Plus => Ok(self.ele.visit(context)?),
            UnaryOperator::Minus => {
                let ele = self.ele.visit(context)?;
                Ok(match ele {
                    VisitRet::Float(val) => VisitRet::Float(-val),
                    VisitRet::Int(val) => VisitRet::Int(-val),
                    VisitRet::None => VisitRet::None,
                })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub id: String,
}

impl Visit for Var {
    fn visit(
        &self,
        context: &mut std::collections::HashMap<String, VisitRet>,
    ) -> ParsingResult<VisitRet> {
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
                    "consume token failed, expect {}, found {}. stop at {}",
                    token_ty,
                    c_token.ty,
                    c_token.print_position()
                ))
            }
        } else {
            Err("consume token while in init state".to_string())
        }
    }

    /// program : PROGRAM variable SEMI block DOT
    pub fn program(&mut self) -> ParsingResult<Program> {
        debug!("parsing program...");
        self.eat(TokenTy::Program)?;
        let name = self.variable()?;
        self.eat(TokenTy::Semi)?;
        let block = self.block()?;
        self.eat(TokenTy::Dot)?;
        let prog = Program {
            name: name.id,
            block,
        };
        ast_debug!(prog);
        Ok(prog)
    }

    /// block : declarations compound_statement
    pub fn block(&mut self) -> ParsingResult<Block> {
        debug!("parsing block...");
        if let Some(token) = &self.current_token {
            let declarations = if token.ty == TokenTy::Var {
                self.declarations()?
            } else {
                vec![]
            };
            let compound_statement = self.compound_statement()?;
            let block = Block {
                declarations,
                compound_statement,
            };
            ast_debug!(block);
            Ok(block)
        } else {
            Err("expect more token".to_string())
        }
    }

    /// declarations : VAR (variable_declaration SEMI)+ | empty
    pub fn declarations(&mut self) -> ParsingResult<Vec<Declaration>> {
        debug!("parsing declarations...");
        self.eat(TokenTy::Var)?;
        let mut ret = vec![self.var_declaration()?];
        self.eat(TokenTy::Semi)?;
        while let Some(token) = &self.current_token {
            if token.ty == TokenTy::Id {
                ret.push(self.var_declaration()?);
                self.eat(TokenTy::Semi)?;
            } else {
                break;
            }
        }
        ast_debug!(ret);
        Ok(ret)
    }

    /// variable_declaration : ID (COMMA ID)* COLON type_spec
    pub fn var_declaration(&mut self) -> ParsingResult<Declaration> {
        debug!("parsing variable declaration...");
        let mut ids = vec![self.variable()?];
        while let Some(token) = self.current_token.clone() {
            if token.ty == TokenTy::Comma {
                self.eat(TokenTy::Comma)?;
                ids.push(self.variable()?);
            } else {
                break;
            }
        }
        self.eat(TokenTy::Colon)?;
        let ty = self.type_spec()?;
        let declaration = Declaration { ids, ty };
        ast_debug!(declaration);
        Ok(declaration)
    }

    pub fn type_spec(&mut self) -> ParsingResult<VarType> {
        debug!("parsing type sepc...");
        if let Some(token) = self.current_token.clone() {
            match &token.ty {
                TokenTy::Integer => {
                    self.eat(token.ty)?;
                    let ty = VarType::Integer;
                    ast_debug!(ty);
                    Ok(ty)
                }
                TokenTy::Float => {
                    self.eat(token.ty)?;
                    let ty = VarType::Real;
                    ast_debug!(ty);
                    Ok(ty)
                }
                _ => Err(format!("expect type spec, got {}", token)),
            }
        } else {
            Err("expect more token".to_string())
        }
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
            } else if current_token.ty == TokenTy::IntegerDiv {
                self.eat(TokenTy::IntegerDiv)?;
                let right = self.factor()?;
                ret = Term::Binary(BiOperate {
                    op: BiOperator::IntegerDiv,
                    left: Box::new(ret),
                    right,
                })
            } else if current_token.ty == TokenTy::RealDiv {
                self.eat(TokenTy::RealDiv)?;
                let right = self.factor()?;
                ret = Term::Binary(BiOperate {
                    op: BiOperator::FloatDiv,
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
                TokenTy::IntegerConst => {
                    self.eat(token.ty.clone())?;
                    let val = token.parse_int()?;
                    let int_factor = Factor::Integer(val);
                    ast_debug!(int_factor);
                    Ok(int_factor)
                }
                TokenTy::RealConst => {
                    self.eat(token.ty.clone())?;
                    let val = token.parse_float()?;
                    let float_factor = Factor::Float(val);
                    ast_debug!(float_factor);
                    Ok(float_factor)
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
