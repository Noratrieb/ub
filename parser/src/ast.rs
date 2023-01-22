use std::{ops::Range, path::PathBuf};

#[derive(Debug, Clone, PartialEq)]
pub struct NodeId(u32);

type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub name: PathBuf,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    U64,
    Ptr(Box<Ty>),
    Name(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    FnDecl(FnDecl),
    StructDecl(StructDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: String,
    pub params: Vec<NameTyPair>,
    pub ret_ty: Option<Ty>,
    pub id: NodeId,
    pub span: Span,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NameTyPair {
    pub name: String,
    pub ty: Ty,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<NameTyPair>,
    pub id: NodeId,
    pub span: Span,
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
    pub name: String,
    pub ty: Option<Ty>,
    pub rhs: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub place: Expr,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub else_part: Option<ElsePart>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElsePart {
    Else(Vec<Stmt>, Span),
    ElseIf(Box<IfStmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStmt {
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    FieldAccess(FieldAccess),
    Call(Call),
    Literal(Literal),
    Name(String),
    Array(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpKind {
    Eq,
    Neq,
    Gt,
    Lt,
    GtEq,
    LtEq,
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
pub struct UnaryOp {
    pub expr: Box<Expr>,
    pub kind: UnaryOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    Not,
    Neg,
    Deref,
    AddrOf,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub expr: Box<Expr>,
    pub field_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String, Span),
    Integer(u64, Span),
}

impl NodeId {
    pub(crate) fn new(id: u32) -> Self {
        Self(id)
    }
}
