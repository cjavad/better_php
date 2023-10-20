use logos::{Logos, Lexer};

#[derive(Debug, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    BitAnd,
    BitOr,
    Or,
    Xor,
    Shl,
    Shr,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Eq,
    NotEq,
    Spaceship,
    Concat,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    PowAssign,
    AndAssign,
    BitAndAssign,
    BitOrAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    ConcatAssign,
    Coalesce,
    CoalesceAssign,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOpKind {
    Not,
    BitNot,
    Plus,
    Minus,
    Inc,
    Dec,
}

#[derive(Debug, PartialEq)]
pub enum KeywordKind {
    For,
    ForEach,
    While,
    If,
    Else,
    ElseIf,
    Fn,
    Goto,
    Return,
    Break,
    Continue,
    Function,
    Throw,
    Trait,
    Class,
    Extends,
    Interface,
    Implements,
    Enum,
    Namespace,
    Use,
    As,
    Match,
    Switch,
    Case,
    Default,
    Try,
    Catch,
    Finally,
    Do,
    Const,
    Abstract,
    Static,
    Readonly,
    Public,
    Protected,
    Private,
    Final,
    New,
    Clone,
    Yield,
    YieldFrom,
    True,
    False,
    Null,
    Self_,
    Parent,
    Global,
}

#[derive(Debug, PartialEq)]
pub enum IdentifierKind {
    FullyQualifiedNamespace(String),
    RelativeNamespace(String),
    Other(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LexerError {
    pub message: String,
}

impl Default for LexerError {
    fn default() -> Self {
        LexerError {
            message: "Unknown error".to_string(),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum TokenKind {
    #[token(r"+", |_| BinaryOpKind::Add)]
    #[token(r"-", |_| BinaryOpKind::Sub)]
    #[token(r"*", |_| BinaryOpKind::Mul)]
    #[token(r"/", |_| BinaryOpKind::Div)]
    #[token(r"%", |_| BinaryOpKind::Mod)]
    #[token(r"**", |_| BinaryOpKind::Pow)]
    #[token(r"&", |_| BinaryOpKind::BitAnd)]
    #[token(r"|", |_| BinaryOpKind::BitOr)]
    #[token(r"^", |_| BinaryOpKind::Xor)]
    #[token(r"<<", |_| BinaryOpKind::Shl)]
    #[token(r">>", |_| BinaryOpKind::Shr)]
    #[token(r">", |_| BinaryOpKind::Greater)]
    #[token(r"<", |_| BinaryOpKind::Less)]
    #[token(r">=", |_| BinaryOpKind::GreaterEq)]
    #[token(r"<=", |_| BinaryOpKind::LessEq)]
    #[token(r"==", |_| BinaryOpKind::Eq)]
    #[token(r"!=", |_| BinaryOpKind::NotEq)]
    #[token(r"<>", |_| BinaryOpKind::NotEq)]
    #[token(r"<=>", |_| BinaryOpKind::Spaceship)]
    #[token(r"&&", |_| BinaryOpKind::And)]
    #[token(r"||", |_| BinaryOpKind::Or)]
    #[token(r".", |_| BinaryOpKind::Concat)]
    #[token(r"=", |_| BinaryOpKind::Assign)]
    #[token(r"+=", |_| BinaryOpKind::AddAssign)]
    #[token(r"-=", |_| BinaryOpKind::SubAssign)]
    #[token(r"*=", |_| BinaryOpKind::MulAssign)]
    #[token(r"/=", |_| BinaryOpKind::DivAssign)]
    #[token(r"%=", |_| BinaryOpKind::ModAssign)]
    #[token(r"**=", |_| BinaryOpKind::PowAssign)]
    #[token(r"&=", |_| BinaryOpKind::BitAndAssign)]
    #[token(r"&&=", |_| BinaryOpKind::AndAssign)]
    #[token(r"|=", |_| BinaryOpKind::BitOrAssign)]
    #[token(r"||=", |_| BinaryOpKind::OrAssign)]
    #[token(r"^=", |_| BinaryOpKind::XorAssign)]
    #[token(r"<<=", |_| BinaryOpKind::ShlAssign)]
    #[token(r">>=", |_| BinaryOpKind::ShrAssign)]
    #[token(r".=", |_| BinaryOpKind::ConcatAssign)]
    #[token(r"??", |_| BinaryOpKind::Coalesce)]
    #[token(r"??=", |_| BinaryOpKind::CoalesceAssign)]
    BinaryOp(BinaryOpKind),
    
    #[token(r"~", |_| UnaryOpKind::BitNot)]
    #[token(r"!", |_| UnaryOpKind::Not)]
    #[token(r"++", |_| UnaryOpKind::Inc)]
    #[token(r"--", |_| UnaryOpKind::Dec)]
    UnaryOp(UnaryOpKind),

    #[token(r"true", |_| KeywordKind::True)]
    #[token(r"false", |_| KeywordKind::False)]
    #[token(r"null", |_| KeywordKind::Null)]
    #[token(r"self", |_| KeywordKind::Self_)]
    #[token(r"parent", |_| KeywordKind::Parent)]
    #[token(r"global", |_| KeywordKind::Global)]
    #[token(r"for", |_| KeywordKind::For)]
    #[token(r"foreach", |_| KeywordKind::ForEach)]
    #[token(r"while", |_| KeywordKind::While)]
    #[token(r"if", |_| KeywordKind::If)]
    #[token(r"else", |_| KeywordKind::Else)]
    #[token(r"elseif", |_| KeywordKind::ElseIf)]
    #[token(r"fn", |_| KeywordKind::Fn)]
    #[token(r"goto", |_| KeywordKind::Goto)]
    #[token(r"return", |_| KeywordKind::Return)]
    #[token(r"break", |_| KeywordKind::Break)]
    #[token(r"continue", |_| KeywordKind::Continue)]
    #[token(r"function", |_| KeywordKind::Function)]
    #[token(r"throw", |_| KeywordKind::Throw)]
    #[token(r"trait", |_| KeywordKind::Trait)]
    #[token(r"class", |_| KeywordKind::Class)]
    #[token(r"extends", |_| KeywordKind::Extends)]
    #[token(r"interface", |_| KeywordKind::Interface)]
    #[token(r"implements", |_| KeywordKind::Implements)]
    #[token(r"enum", |_| KeywordKind::Enum)]
    #[token(r"namespace", |_| KeywordKind::Namespace)]
    #[token(r"use", |_| KeywordKind::Use)]
    #[token(r"as", |_| KeywordKind::As)]
    #[token(r"match", |_| KeywordKind::Match)]
    #[token(r"switch", |_| KeywordKind::Switch)]
    #[token(r"case", |_| KeywordKind::Case)]
    #[token(r"default", |_| KeywordKind::Default)]
    #[token(r"try", |_| KeywordKind::Try)]
    #[token(r"catch", |_| KeywordKind::Catch)]
    #[token(r"finally", |_| KeywordKind::Finally)]
    #[token(r"do", |_| KeywordKind::Do)]
    #[token(r"const", |_| KeywordKind::Const)]
    #[token(r"abstract", |_| KeywordKind::Abstract)]
    #[token(r"static", |_| KeywordKind::Static)]
    #[token(r"readonly", |_| KeywordKind::Readonly)]
    #[token(r"public", |_| KeywordKind::Public)]
    #[token(r"protected", |_| KeywordKind::Protected)]
    #[token(r"private", |_| KeywordKind::Private)]
    #[token(r"final", |_| KeywordKind::Final)]
    #[token(r"new", |_| KeywordKind::New)]
    #[token(r"clone", |_| KeywordKind::Clone)]
    #[token(r"yield", |_| KeywordKind::Yield)]
    #[token(r"yield from", |_| KeywordKind::YieldFrom)]
    Keyword(KeywordKind),

    #[token(r"<?")]
    #[token(r"<%")]
    #[token(r"<?php")]
    OpenTag,
    #[token(r"<?=")]
    #[token(r"<%=")]
    OpenEchoTag,
    #[token(r"?>")]
    #[token(r"%>")]
    EndTag,

    #[token("#[", priority = 12)]
    Attribute,
    
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token(r"->")]
    ObjectOperator,

    #[token(r"?->")]
    NullSafeObjectOperator,

    #[token(r"=>")]
    Arrow,

    #[token(r",")]
    Comma,

    #[token(r":")]
    Colon,

    #[token(r";")]
    Semicolon,
    
    #[token(r"::")]
    PaamayimNekudotayim,

    // PHP style variables with $ prefix
    #[regex(r"\$[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*", |lex| lex.slice()[1..].to_string())]
    Variable(String),

    // TODO Identifiers
    #[regex(r"\\?[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*(\\[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*)*", |lex| {
        let token = lex.slice();
        if token.starts_with("\\") {
            IdentifierKind::FullyQualifiedNamespace(lex.slice().to_string())
        } else if token.contains("\\") {
            IdentifierKind::RelativeNamespace(lex.slice().to_string())
        } else {
            IdentifierKind::Other(lex.slice().to_string())
        }
    })]
    Identifier(IdentifierKind),


    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().unwrap_or(0.0))]
    Double(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse().unwrap_or(0))]
    #[regex(r"0b[0-1]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2).unwrap_or(0))]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16).unwrap_or(0))]
    #[regex(r"0o[0-7]+", |lex| i64::from_str_radix(&lex.slice()[2..], 8).unwrap_or(0))]
    Integer(i64),

    // Double quoted string both ' and " are supported
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    String(String),


    #[regex(r"//.*", |lex| lex.slice().to_string())]
    #[regex(r"/\*([^*]|\*[^/])*\*/", |lex| lex.slice().to_string())]
    // Rewrite above regex to exclude #[ while still matching #.*
    #[regex(r"#[^\[].*", |lex| lex.slice().to_string())]
        
    Comment(String),
}


pub fn lexerize(input: &str) -> Lexer<TokenKind> {
    TokenKind::lexer(input)
}