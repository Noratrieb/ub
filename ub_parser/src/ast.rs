use crate::span::Span;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    name: PathBuf,
    items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    span: Span,
    kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    U64,
    Ptr(Box<TyKind>),
    Name(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    FnDecl(FnDecl),
    StructDecl(StructDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    name: String,
    params: Vec<FnParam>,
    ret_ty: Ty,
    span: Span,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    name: String,
    ty: Ty,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    name: String,
    span: Span,
    fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    name: String,
    ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    Assignment(Assignment),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    LoopStmt(LoopStmt),
    Item(Item),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    name: String,
    ty: Ty,
    rhs: Option<Expr>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    place: Place,
    rhs: Expr,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    cond: Expr,
    body: Vec<Stmt>,
    else_part: Option<ElsePart>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElsePart {
    Else(Vec<Stmt>, Span),
    ElseIf(Box<IfStmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    cond: Expr,
    body: Vec<Stmt>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStmt {
    body: Vec<Stmt>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinOp(BinOp),
    Place(Place),
    Call(Call),
    Deref(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    kind: BinOpKind,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shr,
    Shl,
    And,
    Or,
    BitAnd,
    BitOr,
    Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Place {
    FieldAccess(FieldAccess),
    Name(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    expr: Box<Expr>,
    field_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
}
