use core::panic;
use std::default;

use crate::ast::AST;

use logos::Lexer;

use crate::token::Token;

pub fn parse(tokens: &mut Lexer<Token>) -> Result<(), ()> {
    let mut inside_php_tag = false;
    let mut ast = AST::new();

    while let Some(token) = tokens.next() {
        if token.is_err() {
            let span = tokens.span();
            panic!("Error at character {} to {}", span.start, span.end);
        }

        let token = token.unwrap();

        inside_php_tag = match (inside_php_tag, token) {
            (false, Token::OpenTag) => true,
            (false, Token::OpenEchoTag) => true,
            (true, Token::EndTag) => false,
            _ => inside_php_tag,
        };

        if !inside_php_tag {
            continue;
        }
    }

    Ok(())
}