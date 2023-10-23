use core::panic;
use std::default;

use crate::{ast::{AST, Function, Expression, KeywordBlock, FromToken}, token::KeywordKind};

use logos::Lexer;

use crate::token::Token;

pub struct ParserState {
    pub allow_anonymous_functions: bool,
    pub inside_php_tag: bool,
}

impl ParserState {
    pub fn new() -> ParserState {
        ParserState {
            inside_php_tag: false,
            allow_anonymous_functions: false,
        }
    }
}

pub struct Parser<'a> {
    ast: AST,
    tokens: Lexer<'a, Token>,
    pub state: ParserState,
}

impl Parser<'_> {
    pub fn new(tokens: Lexer<Token>) -> Parser {
        Parser {
            ast: AST::new(),
            tokens,
            state: ParserState::new(),
        }
    }

    pub fn parse(&mut self) -> Result<(), ()> {
        while let Some(token) = self.tokens.next() {
            if token.is_err() {
                let span = self.tokens.span();
                panic!("Error at character {} to {}", span.start, span.end);
            }
    
            let token = token.unwrap();
    
            self.state.inside_php_tag = match (self.state.inside_php_tag, &token) {
                (false, Token::OpenTag) => true,
                (false, Token::OpenEchoTag) => true,
                (true, Token::EndTag) => false,
                _ => self.state.inside_php_tag,
            };
    
            if !self.state.inside_php_tag {
                continue;
            }

            match Expression::from_token_stream(&mut (&mut self.tokens).peekable(), &mut self.state, None) {
                Ok(expr) => {
                    self.ast.push(expr);
                },
                Err(_) => {
                    panic!("Error at character {} to {}", self.tokens.span().start, self.tokens.span().end);
                }
            }

            println!("{:?} ", token);
        }

        println!("{:?}", self.ast);
    
        Ok(())
    }
}