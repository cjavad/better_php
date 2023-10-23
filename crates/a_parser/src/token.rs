use logos::{Logos, Lexer};

#[derive(Debug, PartialEq, Clone)]
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
    ObjectOperator,
    NullSafeObjectOperator,
    PaamayimNekudotayim,
    Arrow,
    Insteadof,
    As,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    Integer(i64),
    Double(f64),
    String(String),
}

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token(r"=>", |_| BinaryOpKind::Arrow)]
    #[token(r"->", |_| BinaryOpKind::ObjectOperator)]
    #[token(r"?->", |_| BinaryOpKind::NullSafeObjectOperator)]
    #[token(r"::", |_| BinaryOpKind::PaamayimNekudotayim)]
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
    #[token(r"insteadof", |_| BinaryOpKind::Insteadof, ignore(case))]
    #[token(r"as", |_| BinaryOpKind::As, ignore(case))]
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

    #[token(r"var", |_| KeywordKind::Var, ignore(case))]
    #[regex(r"true", |_| KeywordKind::True, ignore(case))]
    #[regex(r"false", |_| KeywordKind::False, ignore(case))]
    #[regex(r"null", |_| KeywordKind::Null, ignore(case))]
    #[token(r"self", |_| KeywordKind::Self_, ignore(case))]
    #[token(r"parent", |_| KeywordKind::Parent, ignore(case))]
    #[token(r"global", |_| KeywordKind::Global, ignore(case))]
    #[token(r"for", |_| KeywordKind::For, ignore(case))]
    #[token(r"foreach", |_| KeywordKind::ForEach, ignore(case))]
    #[token(r"while", |_| KeywordKind::While, ignore(case))]
    #[token(r"if", |_| KeywordKind::If, ignore(case))]
    #[token(r"else", |_| KeywordKind::Else, ignore(case))]
    #[token(r"elseif", |_| KeywordKind::ElseIf, ignore(case))]
    #[token(r"fn", |_| KeywordKind::Fn, ignore(case))]
    #[token(r"goto", |_| KeywordKind::Goto, ignore(case))]
    #[token(r"return", |_| KeywordKind::Return, ignore(case))]
    #[token(r"break", |_| KeywordKind::Break, ignore(case))]
    #[token(r"continue", |_| KeywordKind::Continue, ignore(case))]
    #[token(r"function", |_| KeywordKind::Function, ignore(case))]
    #[token(r"throw", |_| KeywordKind::Throw, ignore(case))]
    #[token(r"trait", |_| KeywordKind::Trait, ignore(case))]
    #[token(r"class", |_| KeywordKind::Class, ignore(case))]
    #[token(r"extends", |_| KeywordKind::Extends, ignore(case))]
    #[token(r"interface", |_| KeywordKind::Interface, ignore(case))]
    #[token(r"implements", |_| KeywordKind::Implements, ignore(case))]
    #[token(r"enum", |_| KeywordKind::Enum, ignore(case))]
    #[token(r"namespace", |_| KeywordKind::Namespace, ignore(case))]
    #[token(r"use", |_| KeywordKind::Use, ignore(case))]
    #[token(r"match", |_| KeywordKind::Match, ignore(case))]
    #[token(r"switch", |_| KeywordKind::Switch, ignore(case))]
    #[token(r"case", |_| KeywordKind::Case, ignore(case))]
    #[token(r"default", |_| KeywordKind::Default, ignore(case))]
    #[token(r"try", |_| KeywordKind::Try, ignore(case))]
    #[token(r"catch", |_| KeywordKind::Catch, ignore(case))]
    #[token(r"finally", |_| KeywordKind::Finally, ignore(case))]
    #[token(r"endswitch", |_| KeywordKind::EndSwitch, ignore(case))]
    #[token(r"enddeclare", |_| KeywordKind::EndDeclare, ignore(case))]
    #[token(r"endwhile", |_| KeywordKind::EndWhile, ignore(case))]
    #[token(r"endfor", |_| KeywordKind::EndFor, ignore(case))]
    #[token(r"endforeach", |_| KeywordKind::EndForeach, ignore(case))]
    #[token(r"endif", |_| KeywordKind::EndIf, ignore(case))]
    #[token(r"do", |_| KeywordKind::Do, ignore(case))]
    #[token(r"const", |_| KeywordKind::Const, ignore(case))]
    #[token(r"abstract", |_| KeywordKind::Abstract, ignore(case))]
    #[token(r"static", |_| KeywordKind::Static, ignore(case))]
    #[token(r"readonly", |_| KeywordKind::Readonly, ignore(case))]
    #[token(r"public", |_| KeywordKind::Public, ignore(case))]
    #[token(r"protected", |_| KeywordKind::Protected, ignore(case))]
    #[token(r"private", |_| KeywordKind::Private, ignore(case))]
    #[token(r"final", |_| KeywordKind::Final, ignore(case))]
    #[token(r"new", |_| KeywordKind::New, ignore(case))]
    #[token(r"clone", |_| KeywordKind::Clone, ignore(case))]
    #[token(r"yield", |_| KeywordKind::Yield, ignore(case))]
    #[token(r"yield from", |_| KeywordKind::YieldFrom, ignore(case))]
    #[token(r"list", |_| KeywordKind::List, ignore(case))]
    #[token(r"array", |_| KeywordKind::Array, ignore(case))]
    #[token(r"echo", |_| KeywordKind::Echo, ignore(case))]
    #[token(r"print", |_| KeywordKind::Print, ignore(case))]
    #[token(r"eval", |_| KeywordKind::Eval, ignore(case))]
    #[token(r"unset", |_| KeywordKind::Unset, ignore(case))]
    #[token(r"isset", |_| KeywordKind::Isset, ignore(case))]
    #[token(r"die", |_| KeywordKind::Die, ignore(case))]
    #[token(r"exit", |_| KeywordKind::Die, ignore(case))]
    #[token(r"empty", |_| KeywordKind::Empty, ignore(case))]
    #[token(r"__halt_compiler", |_| KeywordKind::HaltCompiler, ignore(case))]
    #[token(r"instanceof", |_| KeywordKind::Instanceof, ignore(case))]
    #[token(r"require", |_| KeywordKind::Require, ignore(case))]
    #[token(r"require_once", |_| KeywordKind::RequireOnce, ignore(case))]
    #[token(r"include", |_| KeywordKind::Include, ignore(case))]
    #[token(r"include_once", |_| KeywordKind::IncludeOnce, ignore(case))]
    #[token(r"declare", |_| KeywordKind::Declare, ignore(case))]
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

    #[token(r",")]
    Comma,

    #[token(r":")]
    Colon,

    #[token(r";")]
    Semicolon,

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
            // This could be a class name, function name, constant
            // Some property etc, parse it later.
            IdentifierKind::Other(lex.slice().to_string())
        }
    })]
    Identifier(IdentifierKind),


    // Match 2.0, 256.4, 10.358, 7.64E+5, 5.56E-5
    #[regex(r"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?", |lex| LiteralKind::Double(lex.slice().parse().unwrap_or(0.0)))]
    #[regex(r"[0-9]+", |lex| LiteralKind::Integer(lex.slice().parse().unwrap_or(0)))]
    #[regex(r"0b[0-1]+", |lex| LiteralKind::Integer(i64::from_str_radix(&lex.slice()[2..], 2).unwrap_or(0)))]
    #[regex(r"0x[0-9a-fA-F]+", |lex| LiteralKind::Integer(i64::from_str_radix(&lex.slice()[2..], 16).unwrap_or(0)))]
    #[regex(r"0o[0-7]+", |lex| LiteralKind::Integer(i64::from_str_radix(&lex.slice()[2..], 8).unwrap_or(0)))]
    // Double quoted string both ' and " are supported
    #[regex(r#""([^"\\]|\\.)*""#, |lex| LiteralKind::String(lex.slice()[1..lex.slice().len()-1].to_string()))]
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| LiteralKind::String(lex.slice()[1..lex.slice().len()-1].to_string()))]
    // Also support b'' and b"" but treat input as ascii 0-255 without utf8 support
    #[regex(r#"b"([^"\\]|\\.)*""#, |lex| LiteralKind::String(lex.slice()[2..lex.slice().len()-1].to_string()))]
    #[regex(r#"b'([^'\\]|\\.)*'"#, |lex| LiteralKind::String(lex.slice()[2..lex.slice().len()-1].to_string()))]
    Literal(LiteralKind),



    #[regex(r"//.*", |lex| lex.slice().to_string())]
    #[regex(r"/\*([^*]|\*[^/])*\*/", |lex| lex.slice().to_string())]
    // Rewrite above regex to exclude #[ while still matching #.*
    #[regex(r"#[^\[].*", |lex| lex.slice().to_string())]
        
    Comment(String),
}


pub fn lexerize(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}

#[cfg(test)]
mod tests {
    use core::panic;

    use super::*;

    macro_rules! literal_string {
        ($string: expr) => {
            Token::Literal(LiteralKind::String($string.to_string()))
        };
    }

    macro_rules! literal_integer {
        ($integer: expr) => {
            Token::Literal(LiteralKind::Integer($integer))
        };
    }

    macro_rules! literal_double {
        ($double: expr) => {
            Token::Literal(LiteralKind::Double($double))
        };
    }

    // Write test macro to replace Some(Ok()) and assert_eq!()
    macro_rules! assert_single_token {
        ($source: expr, $token: expr) => {
            assert_eq!(Token::lexer($source).next(), Some(Ok($token)));
        };
    }    

    macro_rules! assert_tokens {
        ($source: expr, $tokens: expr) => {
            let mut lexer = Token::lexer($source);
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
        assert_single_token!("b\"\"", literal_string!(""));
    }

    #[test]
    fn test_binary_op() {
        assert_single_token!("+", Token::BinaryOp(BinaryOpKind::Add));
        assert_single_token!("-", Token::BinaryOp(BinaryOpKind::Sub));
        assert_single_token!("*", Token::BinaryOp(BinaryOpKind::Mul));
        assert_single_token!("/", Token::BinaryOp(BinaryOpKind::Div));
        assert_single_token!("%", Token::BinaryOp(BinaryOpKind::Mod));
        assert_single_token!("**", Token::BinaryOp(BinaryOpKind::Pow));
        assert_single_token!("&", Token::BinaryOp(BinaryOpKind::BitAnd));
        assert_single_token!("|", Token::BinaryOp(BinaryOpKind::BitOr));
        assert_single_token!("^", Token::BinaryOp(BinaryOpKind::Xor));
        assert_single_token!("<<", Token::BinaryOp(BinaryOpKind::Shl));
        assert_single_token!(">>", Token::BinaryOp(BinaryOpKind::Shr));
        assert_single_token!(">", Token::BinaryOp(BinaryOpKind::Greater));
        assert_single_token!("<", Token::BinaryOp(BinaryOpKind::Less));
        assert_single_token!(">=", Token::BinaryOp(BinaryOpKind::GreaterEq));
        assert_single_token!("<=", Token::BinaryOp(BinaryOpKind::LessEq));
        assert_single_token!("==", Token::BinaryOp(BinaryOpKind::Eq));
        assert_single_token!("!=", Token::BinaryOp(BinaryOpKind::NotEq));
        assert_single_token!("<>", Token::BinaryOp(BinaryOpKind::NotEq));
        assert_single_token!("<=>", Token::BinaryOp(BinaryOpKind::Spaceship));
        assert_single_token!("&&", Token::BinaryOp(BinaryOpKind::And));
        assert_single_token!("||", Token::BinaryOp(BinaryOpKind::Or));
        assert_single_token!(".", Token::BinaryOp(BinaryOpKind::Concat));
        assert_single_token!("=", Token::BinaryOp(BinaryOpKind::Assign));
        assert_single_token!("+=", Token::BinaryOp(BinaryOpKind::AddAssign));
        assert_single_token!("-=", Token::BinaryOp(BinaryOpKind::SubAssign));
        assert_single_token!("*=", Token::BinaryOp(BinaryOpKind::MulAssign));
        assert_single_token!("/=", Token::BinaryOp(BinaryOpKind::DivAssign));
        assert_single_token!("%=", Token::BinaryOp(BinaryOpKind::ModAssign));
        assert_single_token!("**=", Token::BinaryOp(BinaryOpKind::PowAssign));
        assert_single_token!("&=", Token::BinaryOp(BinaryOpKind::BitAndAssign));
        assert_single_token!("&&=", Token::BinaryOp(BinaryOpKind::AndAssign));
        assert_single_token!("|=", Token::BinaryOp(BinaryOpKind::BitOrAssign));
        assert_single_token!("||=", Token::BinaryOp(BinaryOpKind::OrAssign));
        assert_single_token!("^=", Token::BinaryOp(BinaryOpKind::XorAssign));
        assert_single_token!("<<=", Token::BinaryOp(BinaryOpKind::ShlAssign));
        assert_single_token!(">>=", Token::BinaryOp(BinaryOpKind::ShrAssign));
        assert_single_token!(".=", Token::BinaryOp(BinaryOpKind::ConcatAssign));
        assert_single_token!("??", Token::BinaryOp(BinaryOpKind::Coalesce));
        assert_single_token!("??=", Token::BinaryOp(BinaryOpKind::CoalesceAssign));

        assert_single_token!("->", Token::BinaryOp(BinaryOpKind::ObjectOperator));
        assert_single_token!("?->", Token::BinaryOp(BinaryOpKind::NullSafeObjectOperator));
        assert_single_token!("::", Token::BinaryOp(BinaryOpKind::PaamayimNekudotayim));
        assert_single_token!("=>", Token::BinaryOp(BinaryOpKind::Arrow));
        assert_single_token!("as", Token::BinaryOp(BinaryOpKind::As));
        assert_single_token!("insteadof", Token::BinaryOp(BinaryOpKind::Insteadof));
    }

    #[test]
    fn test_unary_op() {
        assert_single_token!("~", Token::UnaryOp(UnaryOpKind::BitNot));
        assert_single_token!("!", Token::UnaryOp(UnaryOpKind::Not));
        assert_single_token!("++", Token::UnaryOp(UnaryOpKind::Inc));
        assert_single_token!("--", Token::UnaryOp(UnaryOpKind::Dec));
    }

    #[test]
    fn test_keyword() {
        assert_single_token!("true", Token::Keyword(KeywordKind::True));
        assert_single_token!("false", Token::Keyword(KeywordKind::False));
        assert_single_token!("null", Token::Keyword(KeywordKind::Null));
        assert_single_token!("self", Token::Keyword(KeywordKind::Self_));
        assert_single_token!("parent", Token::Keyword(KeywordKind::Parent));
        assert_single_token!("global", Token::Keyword(KeywordKind::Global));
        assert_single_token!("for", Token::Keyword(KeywordKind::For));
        assert_single_token!("foreach", Token::Keyword(KeywordKind::ForEach));
        assert_single_token!("while", Token::Keyword(KeywordKind::While));
        assert_single_token!("if", Token::Keyword(KeywordKind::If));
        assert_single_token!("else", Token::Keyword(KeywordKind::Else));
        assert_single_token!("elseif", Token::Keyword(KeywordKind::ElseIf));
        assert_single_token!("fn", Token::Keyword(KeywordKind::Fn));
        assert_single_token!("goto", Token::Keyword(KeywordKind::Goto));
        assert_single_token!("return", Token::Keyword(KeywordKind::Return));
        assert_single_token!("break", Token::Keyword(KeywordKind::Break));
        assert_single_token!("continue", Token::Keyword(KeywordKind::Continue));
        assert_single_token!("function", Token::Keyword(KeywordKind::Function));
        assert_single_token!("throw", Token::Keyword(KeywordKind::Throw));
        assert_single_token!("trait", Token::Keyword(KeywordKind::Trait));
        assert_single_token!("class", Token::Keyword(KeywordKind::Class));
        assert_single_token!("extends", Token::Keyword(KeywordKind::Extends));
        assert_single_token!("interface", Token::Keyword(KeywordKind::Interface));
        assert_single_token!("implements", Token::Keyword(KeywordKind::Implements));
        assert_single_token!("enum", Token::Keyword(KeywordKind::Enum));
        assert_single_token!("namespace", Token::Keyword(KeywordKind::Namespace));
        assert_single_token!("use", Token::Keyword(KeywordKind::Use));
        assert_single_token!("match", Token::Keyword(KeywordKind::Match));
        assert_single_token!("switch", Token::Keyword(KeywordKind::Switch));
        assert_single_token!("case", Token::Keyword(KeywordKind::Case));
        assert_single_token!("default", Token::Keyword(KeywordKind::Default));
        assert_single_token!("try", Token::Keyword(KeywordKind::Try));
        assert_single_token!("catch", Token::Keyword(KeywordKind::Catch));
        assert_single_token!("finally", Token::Keyword(KeywordKind::Finally));
        assert_single_token!("do", Token::Keyword(KeywordKind::Do));
        assert_single_token!("const", Token::Keyword(KeywordKind::Const));
        assert_single_token!("abstract", Token::Keyword(KeywordKind::Abstract));
        assert_single_token!("static", Token::Keyword(KeywordKind::Static));
        assert_single_token!("readonly", Token::Keyword(KeywordKind::Readonly));
        assert_single_token!("public", Token::Keyword(KeywordKind::Public));
        assert_single_token!("protected", Token::Keyword(KeywordKind::Protected));
        assert_single_token!("private", Token::Keyword(KeywordKind::Private));
        assert_single_token!("final", Token::Keyword(KeywordKind::Final));
        assert_single_token!("new", Token::Keyword(KeywordKind::New));
        assert_single_token!("clone", Token::Keyword(KeywordKind::Clone));
        assert_single_token!("yield", Token::Keyword(KeywordKind::Yield));
        assert_single_token!("yield from", Token::Keyword(KeywordKind::YieldFrom));

    }

    #[test]

    fn test_literals() {
        assert_single_token!("1", literal_integer!(1));
        assert_single_token!("0b1010", literal_integer!(10));
        assert_single_token!("0x1a", literal_integer!(26));
        assert_single_token!("0o10", literal_integer!(8));
        assert_single_token!("2.0", literal_double!(2.0));
        assert_single_token!("256.4", literal_double!(256.4));
        assert_single_token!("10.358", literal_double!(10.358));
        assert_single_token!("7.64E+5", literal_double!(764000.0));
        assert_single_token!("5.56E-5", literal_double!(0.0000556));
        assert_single_token!("\"Hello World\"", literal_string!("Hello World".to_string()));
        assert_single_token!("'Hello World'", literal_string!("Hello World".to_string()));
        assert_single_token!("b\"Hello World\"", literal_string!("Hello World".to_string()));
        assert_single_token!("b'Hello World'", literal_string!("Hello World".to_string()));
    }

    #[test]
    fn test_combined_expr() {

        assert_tokens!("1 + 2 * 4 - 1;", &[
            literal_integer!(1),
            Token::BinaryOp(BinaryOpKind::Add),
            literal_integer!(2),
            Token::BinaryOp(BinaryOpKind::Mul),
            literal_integer!(4),
            Token::BinaryOp(BinaryOpKind::Sub),
            literal_integer!(1),
            Token::Semicolon,
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
            Token::OpenTag,
            Token::Comment("// This is a comment".to_string()),
            Token::Comment("/* This is a comment */".to_string()),
            Token::Comment("# This is a comment".to_string()),
            Token::Comment("/**\n         * This is a comment\n         * This is a comment\n         * This is a comment\n         */".to_string()),
            Token::Attribute,
            Token::Identifier(IdentifierKind::Other("ThisIsAnAttribute".to_string())),
            Token::RBracket,
            Token::Keyword(KeywordKind::Class),
            Token::Identifier(IdentifierKind::Other("Foo".to_string())),
            Token::LBrace,
            Token::Comment("// This is a comment".to_string()),
            Token::Comment("/* This is a comment */".to_string()),
            Token::Comment("# This is a comment".to_string()),
            Token::Comment("/**\n             * This is a comment\n             * This is a comment\n             * This is a comment\n             */".to_string()),
            Token::Attribute,
            Token::Identifier(IdentifierKind::Other("ThisIsAnAttribute".to_string())),
            Token::RBracket,
            Token::Keyword(KeywordKind::Public),
            Token::Variable("bar".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::EndTag,
        ]);
    }
}