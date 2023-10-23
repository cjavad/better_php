use crate::token::{UnaryOpKind, BinaryOpKind, LiteralKind, KeywordKind};

// For now a type is just a string
pub struct Type {
    // In reality a type is just a wrapper around
    // an expression of identifiers and binary operations | and &
    inner: Vec<Expression>
}

pub struct ParameterDefinition {
    index: u64,
    // If name is None, then this is an anonymous parameter
    name: String,
    // Type hints given to the parameter
    type_parameters: Option<Type>,
    // If value is None, then this is a required parameter
    value: Option<Box<Expression>>,
}

pub struct Parameter {
    index: Option<u64>,
    name: Option<String>,
    value: Option<Box<Expression>>,
}

pub struct Keyword {
    kind: KeywordKind,
    // If value is None, then this is a required parameter
    value: Option<Box<Expression>>,
}

pub struct KeywordBlock {
    kind: KeywordKind,
    value: Option<Box<Expression>>,
    body: Vec<Expression>,
    next: Option<Box<Expression>>,
}

pub struct BinaryOp {
    left: Box<Expression>,
    op: BinaryOpKind,
    right: Box<Expression>,
}

pub struct UnaryOp {
    op: UnaryOpKind,
    expr: Box<Expression>,
}

pub struct Function {
    // If name is None, then this is an anonymous function
    name: Option<String>,
    // Type hints given to the function
    type_parameters: Option<Type>,
    // This would be a list of parameters.
    args: Vec<Expression>,
    // This would be the body of the function
    body: Option<Vec<Expression>>,
    // Optinal array of uses
    uses: Option<Vec<IdentifierKind>>,
}

pub struct ClassProperty {
    is_static: bool,
    is_readonly: bool,
    visibility: KeywordKind,
    parameter: ParameterDefinition,
}

pub struct ClassFunction {
    is_static: bool,
    visibility: KeywordKind,
    function: Function,
}

pub enum ClassMember {
    Property(ClassProperty),
    Function(ClassFunction),
}

pub enum ClassKind {
    Class,
    Interface,
    Enum,
    Trait,
}

// Models the trait use and conflict resolution
// as a list of binary operations
// noteably; insteadof and as
pub struct TraitUse {
    traits: Vec<IdentifierKind>,
    // If the alias is None, then the name is used as the alias
    conflict_resolution: Option<Vec<BinaryOp>>,
}

pub struct Class {
    is_final: bool,
    is_readonly: bool,
    is_abstract: bool,
    name: String,
    kind: ClassKind,
    members: Vec<ClassMember>,
    uses: Option<Vec<TraitUse>>,
    extends: Option<Box<IdentifierKind>>,
    implements: Option<Vec<IdentifierKind>>,
}

pub enum IdentifierKind {
    // Damn php
    StaticVariable(String),
    Variable(String),
    // A combination of a variable and &
    Reference(String),
    Constant(String),
    ClassMethod(String),
    FunctionName(String),

}

// An expression is a single line of code that contains some functionality
// It could be an assignment, a function call, a binary operation, etc.
pub enum Expression {
    BinaryExpression(BinaryOp),
    UnaryExpression(UnaryOp),
    Literal(LiteralKind),
    Identifier(IdentifierKind),
    // A parameter could be:
    // A part of a function definition - so it would have an index, name and an optional default value
    // A port of a function call or call instance - so it would have an index and or name and an optional value
    Parameter(Parameter),
    ParameterDefinition(ParameterDefinition),
    // Take the value of one expression and use it as a hash key to get the value of another expression
    ArrayIndex(Box<Expression>, Box<Expression>),
    // An array definition would be a list of expressions, either singular values or a => binary operation between the key and value
    ArrayDefinition(Vec<Expression>),
    // Invoke one expression with a list of expressions
    // Typically these would be parameters
    Invocation(Box<Expression>, Vec<Expression>),
    // Label name expression, uniquely used by goto
    LabelExpression(IdentifierKind),
    // This would be a keyword such as return; or break;
    // They can take singular arguments, but do not need to.
    Keyword(Keyword),
    KeywordBlock(KeywordBlock),
    // Sometimes we would like an empty expression
    // Primarly to keep expressions such as ;; to 
    // be consistent.
    Empty,
}

pub struct AST {
    inner: Vec<Expression>,
}

impl AST {
    pub fn new() -> AST {
        AST {
            inner: Vec::new(),
        }
    }

    pub fn push(&mut self, expr: Expression) {
        self.inner.push(expr);
    }
}