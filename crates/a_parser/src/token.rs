use logos::{Logos, Lexer};
use a_regex_macro::regex_ignore_case;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    AndAlt,
    BitAnd,
    BitOr,
    Or,
    OrAlt,
    Xor,
    XorAlt,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOpKind {
    Not,
    BitNot,
    Plus,
    Minus,
    Inc,
    Dec,
    ArrayCast,
    BoolCast,
    DoubleCast,
    IntCast,
    ObjectCast,
    StringCast,
    BinaryStringCast,
    NullCast,
    // Used by parser.
    PreInc,
    PreDec,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum KeywordKind {
    // I will abuse this one
    Var,
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
    EndSwitch,
    EndDeclare,
    EndWhile,
    EndFor,
    EndForeach,
    EndIf,
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
    List,
    Array,
    Echo,
    Print,
    Eval,
    Unset,
    Isset,
    Die,
    Empty,
    // Why PHP why?
    HaltCompiler,
    Insteadof,
    Instanceof,
    Require,
    RequireOnce,
    Include,
    IncludeOnce,
    Declare,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentifierKind {
    FullyQualifiedNamespace(String),
    RelativeNamespace(String),
    Other(String),
}

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Clone)]
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
    #[token(r"(int)", |_| UnaryOpKind::IntCast)]
    #[token(r"(float)", |_| UnaryOpKind::DoubleCast)]
    #[token(r"(double)", |_| UnaryOpKind::DoubleCast)]
    #[token(r"(real)", |_| UnaryOpKind::DoubleCast)]
    #[token(r"(string)", |_| UnaryOpKind::StringCast)]
    #[token(r"(array)", |_| UnaryOpKind::ArrayCast)]
    #[token(r"(object)", |_| UnaryOpKind::ObjectCast)]
    #[token(r"(bool)", |_| UnaryOpKind::BoolCast)]
    #[token(r"(boolean)", |_| UnaryOpKind::BoolCast)]
    #[token(r"(binary)", |_| UnaryOpKind::BinaryStringCast)]
    // Technically deprecated but it is so fun i will keep it.
    #[token(r"(unset)", |_| UnaryOpKind::NullCast)]
    UnaryOp(UnaryOpKind),

    #[token(r"var", |_| KeywordKind::Var)]
    #[regex(r"[Tt][Rr][Uu][Ee]", |_| KeywordKind::True)]
    #[regex(r"[Ff][Aa][Ll][Ss][Ee]", |_| KeywordKind::False)]
    #[regex(r"[Nn][Uu][Ll][Ll]", |_| KeywordKind::Null)]
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
    #[token(r"endswitch", |_| KeywordKind::EndSwitch)]
    #[token(r"enddeclare", |_| KeywordKind::EndDeclare)]
    #[token(r"endwhile", |_| KeywordKind::EndWhile)]
    #[token(r"endfor", |_| KeywordKind::EndFor)]
    #[token(r"endforeach", |_| KeywordKind::EndForeach)]
    #[token(r"endif", |_| KeywordKind::EndIf)]
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
    #[token(r"list", |_| KeywordKind::List)]
    #[token(r"array", |_| KeywordKind::Array)]
    #[token(r"echo", |_| KeywordKind::Echo)]
    #[token(r"print", |_| KeywordKind::Print)]
    #[token(r"eval", |_| KeywordKind::Eval)]
    #[token(r"unset", |_| KeywordKind::Unset)]
    #[token(r"isset", |_| KeywordKind::Isset)]
    #[token(r"die", |_| KeywordKind::Die)]
    #[token(r"exit", |_| KeywordKind::Die)]
    #[token(r"empty", |_| KeywordKind::Empty)]
    #[token(r"__halt_compiler", |_| KeywordKind::HaltCompiler)]
    #[token(r"insteadof", |_| KeywordKind::Insteadof)]
    #[token(r"instanceof", |_| KeywordKind::Instanceof)]
    #[token(r"require", |_| KeywordKind::Require)]
    #[token(r"require_once", |_| KeywordKind::RequireOnce)]
    #[token(r"include", |_| KeywordKind::Include)]
    #[token(r"include_once", |_| KeywordKind::IncludeOnce)]
    #[token(r"declare", |_| KeywordKind::Declare)]
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

    #[token("#[")]
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


    // Match 2.0, 256.4, 10.358, 7.64E+5, 5.56E-5
    #[regex(r"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().unwrap_or(0.0))]
    Double(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse().unwrap_or(0))]
    #[regex(r"0b[0-1]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2).unwrap_or(0))]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16).unwrap_or(0))]
    #[regex(r"0o[0-7]+", |lex| i64::from_str_radix(&lex.slice()[2..], 8).unwrap_or(0))]
    Integer(i64),

    // Double quoted string both ' and " are supported
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    // Also support b'' and b"" but treat input as ascii 0-255 without utf8 support
    #[regex(r#"b"([^"\\]|\\.)*""#, |lex| lex.slice()[2..lex.slice().len()-1].to_string())]
    #[regex(r#"b'([^'\\]|\\.)*'"#, |lex| lex.slice()[2..lex.slice().len()-1].to_string())]
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

#[cfg(test)]
mod tests {
    use core::panic;

    use super::*;

    // Write test macro to replace Some(Ok()) and assert_eq!()
    macro_rules! assert_single_token {
        ($source: expr, $token: expr) => {
            assert_eq!(TokenKind::lexer($source).next(), Some(Ok($token)));
        };
    }    

    macro_rules! assert_tokens {
        ($source: expr, $tokens: expr) => {
            let mut lexer = TokenKind::lexer($source);
            for token in $tokens {
                match lexer.next() {
                    Some(Ok(t)) => assert_eq!(t, *token),
                    Some(Err(_)) => panic!("Error"),
                    None => panic!("Unexpected end of input"),
                }
            }
        };
    }

    #[test]
    fn test() {
        assert_single_token!("b\"\"", TokenKind::String("".to_string()));
    }

    #[test]
    fn test_binary_op() {
        assert_single_token!("+", TokenKind::BinaryOp(BinaryOpKind::Add));
        assert_single_token!("-", TokenKind::BinaryOp(BinaryOpKind::Sub));
        assert_single_token!("*", TokenKind::BinaryOp(BinaryOpKind::Mul));
        assert_single_token!("/", TokenKind::BinaryOp(BinaryOpKind::Div));
        assert_single_token!("%", TokenKind::BinaryOp(BinaryOpKind::Mod));
        assert_single_token!("**", TokenKind::BinaryOp(BinaryOpKind::Pow));
        assert_single_token!("&", TokenKind::BinaryOp(BinaryOpKind::BitAnd));
        assert_single_token!("|", TokenKind::BinaryOp(BinaryOpKind::BitOr));
        assert_single_token!("^", TokenKind::BinaryOp(BinaryOpKind::Xor));
        assert_single_token!("<<", TokenKind::BinaryOp(BinaryOpKind::Shl));
        assert_single_token!(">>", TokenKind::BinaryOp(BinaryOpKind::Shr));
        assert_single_token!(">", TokenKind::BinaryOp(BinaryOpKind::Greater));
        assert_single_token!("<", TokenKind::BinaryOp(BinaryOpKind::Less));
        assert_single_token!(">=", TokenKind::BinaryOp(BinaryOpKind::GreaterEq));
        assert_single_token!("<=", TokenKind::BinaryOp(BinaryOpKind::LessEq));
        assert_single_token!("==", TokenKind::BinaryOp(BinaryOpKind::Eq));
        assert_single_token!("!=", TokenKind::BinaryOp(BinaryOpKind::NotEq));
        assert_single_token!("<>", TokenKind::BinaryOp(BinaryOpKind::NotEq));
        assert_single_token!("<=>", TokenKind::BinaryOp(BinaryOpKind::Spaceship));
        assert_single_token!("&&", TokenKind::BinaryOp(BinaryOpKind::And));
        assert_single_token!("||", TokenKind::BinaryOp(BinaryOpKind::Or));
        assert_single_token!(".", TokenKind::BinaryOp(BinaryOpKind::Concat));
        assert_single_token!("=", TokenKind::BinaryOp(BinaryOpKind::Assign));
        assert_single_token!("+=", TokenKind::BinaryOp(BinaryOpKind::AddAssign));
        assert_single_token!("-=", TokenKind::BinaryOp(BinaryOpKind::SubAssign));
        assert_single_token!("*=", TokenKind::BinaryOp(BinaryOpKind::MulAssign));
        assert_single_token!("/=", TokenKind::BinaryOp(BinaryOpKind::DivAssign));
        assert_single_token!("%=", TokenKind::BinaryOp(BinaryOpKind::ModAssign));
        assert_single_token!("**=", TokenKind::BinaryOp(BinaryOpKind::PowAssign));
        assert_single_token!("&=", TokenKind::BinaryOp(BinaryOpKind::BitAndAssign));
        assert_single_token!("&&=", TokenKind::BinaryOp(BinaryOpKind::AndAssign));
        assert_single_token!("|=", TokenKind::BinaryOp(BinaryOpKind::BitOrAssign));
        assert_single_token!("||=", TokenKind::BinaryOp(BinaryOpKind::OrAssign));
        assert_single_token!("^=", TokenKind::BinaryOp(BinaryOpKind::XorAssign));
        assert_single_token!("<<=", TokenKind::BinaryOp(BinaryOpKind::ShlAssign));
        assert_single_token!(">>=", TokenKind::BinaryOp(BinaryOpKind::ShrAssign));
        assert_single_token!(".=", TokenKind::BinaryOp(BinaryOpKind::ConcatAssign));
        assert_single_token!("??", TokenKind::BinaryOp(BinaryOpKind::Coalesce));
        assert_single_token!("??=", TokenKind::BinaryOp(BinaryOpKind::CoalesceAssign));
    }

    #[test]
    fn test_unary_op() {
        assert_single_token!("~", TokenKind::UnaryOp(UnaryOpKind::BitNot));
        assert_single_token!("!", TokenKind::UnaryOp(UnaryOpKind::Not));
        assert_single_token!("++", TokenKind::UnaryOp(UnaryOpKind::Inc));
        assert_single_token!("--", TokenKind::UnaryOp(UnaryOpKind::Dec));
    }

    #[test]
    fn test_keyword() {
        assert_single_token!("true", TokenKind::Keyword(KeywordKind::True));
        assert_single_token!("false", TokenKind::Keyword(KeywordKind::False));
        assert_single_token!("null", TokenKind::Keyword(KeywordKind::Null));
        assert_single_token!("self", TokenKind::Keyword(KeywordKind::Self_));
        assert_single_token!("parent", TokenKind::Keyword(KeywordKind::Parent));
        assert_single_token!("global", TokenKind::Keyword(KeywordKind::Global));
        assert_single_token!("for", TokenKind::Keyword(KeywordKind::For));
        assert_single_token!("foreach", TokenKind::Keyword(KeywordKind::ForEach));
        assert_single_token!("while", TokenKind::Keyword(KeywordKind::While));
        assert_single_token!("if", TokenKind::Keyword(KeywordKind::If));
        assert_single_token!("else", TokenKind::Keyword(KeywordKind::Else));
        assert_single_token!("elseif", TokenKind::Keyword(KeywordKind::ElseIf));
        assert_single_token!("fn", TokenKind::Keyword(KeywordKind::Fn));
        assert_single_token!("goto", TokenKind::Keyword(KeywordKind::Goto));
        assert_single_token!("return", TokenKind::Keyword(KeywordKind::Return));
        assert_single_token!("break", TokenKind::Keyword(KeywordKind::Break));
        assert_single_token!("continue", TokenKind::Keyword(KeywordKind::Continue));
        assert_single_token!("function", TokenKind::Keyword(KeywordKind::Function));
        assert_single_token!("throw", TokenKind::Keyword(KeywordKind::Throw));
        assert_single_token!("trait", TokenKind::Keyword(KeywordKind::Trait));
        assert_single_token!("class", TokenKind::Keyword(KeywordKind::Class));
        assert_single_token!("extends", TokenKind::Keyword(KeywordKind::Extends));
        assert_single_token!("interface", TokenKind::Keyword(KeywordKind::Interface));
        assert_single_token!("implements", TokenKind::Keyword(KeywordKind::Implements));
        assert_single_token!("enum", TokenKind::Keyword(KeywordKind::Enum));
        assert_single_token!("namespace", TokenKind::Keyword(KeywordKind::Namespace));
        assert_single_token!("use", TokenKind::Keyword(KeywordKind::Use));
        assert_single_token!("as", TokenKind::Keyword(KeywordKind::As));
        assert_single_token!("match", TokenKind::Keyword(KeywordKind::Match));
        assert_single_token!("switch", TokenKind::Keyword(KeywordKind::Switch));
        assert_single_token!("case", TokenKind::Keyword(KeywordKind::Case));
        assert_single_token!("default", TokenKind::Keyword(KeywordKind::Default));
        assert_single_token!("try", TokenKind::Keyword(KeywordKind::Try));
        assert_single_token!("catch", TokenKind::Keyword(KeywordKind::Catch));
        assert_single_token!("finally", TokenKind::Keyword(KeywordKind::Finally));
        assert_single_token!("do", TokenKind::Keyword(KeywordKind::Do));
        assert_single_token!("const", TokenKind::Keyword(KeywordKind::Const));
        assert_single_token!("abstract", TokenKind::Keyword(KeywordKind::Abstract));
        assert_single_token!("static", TokenKind::Keyword(KeywordKind::Static));
        assert_single_token!("readonly", TokenKind::Keyword(KeywordKind::Readonly));
        assert_single_token!("public", TokenKind::Keyword(KeywordKind::Public));
        assert_single_token!("protected", TokenKind::Keyword(KeywordKind::Protected));
        assert_single_token!("private", TokenKind::Keyword(KeywordKind::Private));
        assert_single_token!("final", TokenKind::Keyword(KeywordKind::Final));
        assert_single_token!("new", TokenKind::Keyword(KeywordKind::New));
        assert_single_token!("clone", TokenKind::Keyword(KeywordKind::Clone));
        assert_single_token!("yield", TokenKind::Keyword(KeywordKind::Yield));
        assert_single_token!("yield from", TokenKind::Keyword(KeywordKind::YieldFrom));

    }

    #[test]

    fn test_literals() {
        assert_single_token!("1", TokenKind::Integer(1));
        assert_single_token!("0b1010", TokenKind::Integer(10));
        assert_single_token!("0x1a", TokenKind::Integer(26));
        assert_single_token!("0o10", TokenKind::Integer(8));
        assert_single_token!("2.0", TokenKind::Double(2.0));
        assert_single_token!("256.4", TokenKind::Double(256.4));
        assert_single_token!("10.358", TokenKind::Double(10.358));
        assert_single_token!("7.64E+5", TokenKind::Double(764000.0));
        assert_single_token!("5.56E-5", TokenKind::Double(0.0000556));
        assert_single_token!("\"Hello World\"", TokenKind::String("Hello World".to_string()));
        assert_single_token!("'Hello World'", TokenKind::String("Hello World".to_string()));
        assert_single_token!("b\"Hello World\"", TokenKind::String("Hello World".to_string()));
        assert_single_token!("b'Hello World'", TokenKind::String("Hello World".to_string()));
    }

    #[test]
    fn test_combined_expr() {

        assert_tokens!("1 + 2 * 4 - 1;", &[
            TokenKind::Integer(1),
            TokenKind::BinaryOp(BinaryOpKind::Add),
            TokenKind::Integer(2),
            TokenKind::BinaryOp(BinaryOpKind::Mul),
            TokenKind::Integer(4),
            TokenKind::BinaryOp(BinaryOpKind::Sub),
            TokenKind::Integer(1),
            TokenKind::Semicolon,
        ]);
    }

    #[test]
    fn test_combined_comments() {
        assert_tokens!("<?php

        // This is a comment
        /* This is a comment */
        # This is a comment
        /**
         * This is a comment
         * This is a comment
         * This is a comment
         */

        #[ThisIsAnAttribute]
        class Foo {
            // This is a comment
            /* This is a comment */
            # This is a comment
            /**
             * This is a comment
             * This is a comment
             * This is a comment
             */
            #[ThisIsAnAttribute]
            public $bar;
        }

        ?>", &[
            TokenKind::OpenTag,
            TokenKind::Comment("// This is a comment".to_string()),
            TokenKind::Comment("/* This is a comment */".to_string()),
            TokenKind::Comment("# This is a comment".to_string()),
            TokenKind::Comment("/**\n         * This is a comment\n         * This is a comment\n         * This is a comment\n         */".to_string()),
            TokenKind::Attribute,
            TokenKind::Identifier(IdentifierKind::Other("ThisIsAnAttribute".to_string())),
            TokenKind::RBracket,
            TokenKind::Keyword(KeywordKind::Class),
            TokenKind::Identifier(IdentifierKind::Other("Foo".to_string())),
            TokenKind::LBrace,
            TokenKind::Comment("// This is a comment".to_string()),
            TokenKind::Comment("/* This is a comment */".to_string()),
            TokenKind::Comment("# This is a comment".to_string()),
            TokenKind::Comment("/**\n             * This is a comment\n             * This is a comment\n             * This is a comment\n             */".to_string()),
            TokenKind::Attribute,
            TokenKind::Identifier(IdentifierKind::Other("ThisIsAnAttribute".to_string())),
            TokenKind::RBracket,
            TokenKind::Keyword(KeywordKind::Public),
            TokenKind::Variable("bar".to_string()),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::EndTag,
        ]);
    }
}