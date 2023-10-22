use crate::token::{TokenKind, UnaryOpKind, BinaryOpKind};

pub enum IdentifierKind {
    Variable(String),
    Function(String),
    Constant(String),
    Class(String),
    Property(String),
}

struct UnaryOp {
    op: UnaryOpKind,
    expr: Box<IdentifierKind>,
}

struct BinaryOp {
    op: BinaryOpKind,
    lhs: Box<IdentifierKind>,
    rhs: Box<IdentifierKind>,
}

