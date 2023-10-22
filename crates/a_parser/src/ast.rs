use crate::token::{TokenKind, UnaryOpKind, BinaryOpKind};

// An expression is a single line of code that contains some functionality
// It could be an assignment, a function call, a binary operation, etc.

struct BinaryOp {
    left: Box<Expression>,
    op: BinaryOpKind,
    right: Box<Expression>,
}

struct UnaryOp {
    op: UnaryOpKind,
    expr: Box<Expression>,
}

enum LiteralExpression {
    
}

enum Expression {
    BinaryExpression(BinaryOp),
    UnaryExpression(UnaryOp),

}