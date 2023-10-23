use core::panic;
use std::iter::Peekable;

use logos::Lexer;

use crate::{token::{UnaryOpKind, BinaryOpKind, LiteralKind, KeywordKind, Token, IdentifierKind}, parser::ParserState};

pub trait FromToken {
    fn from_token_stream(tokens: &mut Peekable<&mut Lexer<Token>>, state: &mut ParserState, until_token: Option<Token>) -> Result<Self, ()> where Self: Sized;
}

// For now a type is just a string
#[derive(Debug)]

pub struct Type {
    // In reality a type is just a wrapper around
    // an expression of identifiers and binary operations | and &
    inner: Vec<Expression>
}

#[derive(Debug)]

pub struct ParameterDefinition {
    index: u64,
    // If name is None, then this is an anonymous parameter
    name: String,
    // Type hints given to the parameter
    type_parameters: Option<Type>,
    // If value is None, then this is a required parameter
    value: Option<Box<Expression>>,
}

#[derive(Debug)]


pub struct Parameter {
    index: Option<u64>,
    name: Option<String>,
    value: Option<Box<Expression>>,
}


#[derive(Debug)]
pub struct Keyword {
    kind: KeywordKind,
    // If value is None, then this is a required parameter
    value: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct KeywordBlock {
    kind: KeywordKind,
    value: Option<Box<Expression>>,
    body: Vec<Expression>,
    next: Option<Box<Expression>>,
}


#[derive(Debug)]
pub struct BinaryOp {
    left: Box<Expression>,
    op: BinaryOpKind,
    right: Box<Expression>,
}


#[derive(Debug)]
pub struct UnaryOp {
    op: UnaryOpKind,
    expr: Box<Expression>,
}


#[derive(Debug)]
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
    uses: Option<Vec<Identifier>>,
}

impl FromToken for Function {
    fn from_token_stream(tokens: &mut Peekable<&mut logos::Lexer<'_, Token>>, state: &mut ParserState, until_token: Option<Token>) -> Result<Self, ()> {
        let mut name: Option<String> = None;
        let mut type_parameters = None;
        let mut args = Vec::new();
        let mut body = None;
        let mut uses = None;

        let Some(name_token) = tokens.peek()
        else { panic!("Unexpected end of file") };

        // check if next token matches
        // Token::Identifier(IdentifierKind::Other(name))

        match name_token {
            Ok(Token::Identifier(IdentifierKind::Other(s))) => {
                // Consume the token
                // Set the name
                name = Some(s.to_owned());
            },
            Ok(_) => {
                if !state.allow_anonymous_functions {
                    panic!("Expected identifier");
                }
            },
            _ => panic!("Unexpected end of file"),
        };

        if name.is_some() {
            tokens.next();
        }

        // No matter want we except LParen next
        if !tokens.next().expect("Unexpected end of file").unwrap().eq(&Token::LParen) {
            panic!("Expected parenthesis");
        }

        loop {
            let token = tokens.next().expect("Unexpected end of while").unwrap();
            let next_token = tokens.peek().expect("Unexpected end of while").clone().unwrap();

            println!("{:?}", token);

            match (token, next_token) {
                (_, Token::RParen) => {
                    tokens.next();
                    break;
                },
                (Token::Comma, _) => continue,
                _ => {
                    let expr = Expression::from_token_stream(tokens, state, Some(Token::Comma))?;
                    args.push(expr);
                }
            }
        }

        println!("test");

        // TODO Implement use
        // TODO Implement return type


        Ok(Self {
            name,
            type_parameters,
            args,
            body,
            uses,
        })
    }
}

#[derive(Debug)]
pub struct ClassProperty {
    is_static: bool,
    is_readonly: bool,
    visibility: KeywordKind,
    parameter: ParameterDefinition,
}

#[derive(Debug)]
pub struct ClassFunction {
    is_static: bool,
    visibility: KeywordKind,
    function: Function,
}

#[derive(Debug)]
pub enum ClassMember {
    Property(ClassProperty),
    Function(ClassFunction),
}

#[derive(Debug)]
pub enum ClassKind {
    Class,
    Interface,
    Enum,
    Trait,
}

// Models the trait use and conflict resolution
// as a list of binary operations
// noteably; insteadof and as
#[derive(Debug)]
pub struct TraitUse {
    traits: Vec<Identifier>,
    // If the alias is None, then the name is used as the alias
    conflict_resolution: Option<Vec<BinaryOp>>,
}

#[derive(Debug)]
pub struct Class {
    is_final: bool,
    is_readonly: bool,
    is_abstract: bool,
    name: String,
    kind: ClassKind,
    members: Vec<ClassMember>,
    uses: Option<Vec<TraitUse>>,
    extends: Option<Box<Identifier>>,
    implements: Option<Vec<Identifier>>,
}

#[derive(Debug)]
pub enum Identifier {
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
#[derive(Debug)]
pub enum Expression {
    BinaryExpression(BinaryOp),
    UnaryExpression(UnaryOp),
    Literal(LiteralKind),
    Identifier(Identifier),
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
    LabelExpression(Identifier),
    // This would be a keyword such as return; or break;
    // They can take singular arguments, but do not need to.
    Class(Class),
    Function(Function),
    Keyword(Keyword),
    KeywordBlock(KeywordBlock),
    // Sometimes we would like an empty expression
    // Primarly to keep expressions such as ;; to 
    // be consistent.
    Empty,
}

impl FromToken for Expression {
    fn from_token_stream(tokens: &mut Peekable<&mut logos::Lexer<'_, Token>>, state: &mut ParserState, until_token: Option<Token>) -> Result<Self, ()> where Self: Sized {
        loop {
            let token = tokens.peek().expect("Unexpected end of file").clone().unwrap();

            if let Some(until_token) = until_token.clone() {
                if token.eq(&until_token) {
                    break;
                }
            }

            if (token.eq(&Token::Keyword(KeywordKind::Function))) {
                tokens.next(); // Consume the keyword
                let function = Function::from_token_stream(tokens, state, None)?;
                return Ok(Self::Function(function));
            }

            match token {
                Token::Semicolon | Token::Comma | Token::RParen => {
                    break;
                },

                Token::Variable(var) => {
                    tokens.next();
                    return Ok(Self::Identifier(Identifier::Variable(var)));
                },
                _ => {
                    continue;
                }
            }

        }

        Ok(Self::Empty)
    }
}

#[derive(Debug)]
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