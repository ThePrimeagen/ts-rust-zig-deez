mod error;
#[cfg(test)]
mod test;

use crate::{
    ast::AST,
    lex::{Lexer, Token},
};
use std::mem;

pub use error::{Error, Result};

pub trait Parse<'a>
where
    Self: Sized,
{
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> Result<Self>;
}

// pub trait Parse<'a>
// where
//     Self: Sized,
// {
//     fn parse(parser: &mut Parser<'a>) -> Self;
// }

// TODO: aggregate parser-time errors and report in bulk after parsing,
// rather than failing on error
// TODO: implement Iterator for Parser and Lexer
#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    pub current_token: Token,
    pub next_token: Token,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self> {
        let current_token = lexer.next_token()?;
        let next_token = lexer.next_token()?;

        Ok(Self {
            lexer,
            current_token,
            next_token,
            errors: vec![],
        })
    }

    pub fn reset(&mut self) -> Result<()> {
        self.lexer.reset();

        // More efficient that stepping twice
        self.current_token = self.lexer.next_token()?;
        self.next_token = self.lexer.next_token()?;

        Ok(())
    }

    pub fn step(&mut self) -> Result<()> {
        self.current_token = self.lexer.next_token()?;
        mem::swap(&mut self.current_token, &mut self.next_token);

        Ok(())
    }

    pub fn parse(&mut self) -> Result<AST> {
        AST::parse(self, None)
    }

    // TODO: perform only type checking
    pub fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    pub fn current_precedence(&self) -> Precedence {
        Precedence::from(&self.current_token)
    }

    pub fn next_precedence(&self) -> Precedence {
        Precedence::from(&self.next_token)
    }

    // TODO: perform only type checking
    pub fn next_token_is(&self, token: Token) -> bool {
        self.next_token == token
    }

    pub fn expect_next(&mut self, token: Token) -> Result<bool> {
        let is = self.next_token_is(token);
        if is {
            self.step()?;
        }

        Ok(is)
    }
}

#[derive(Debug)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LessThan | Token::GreaterThan => Self::LessGreater,
            Token::Plus | Token::Minus => Self::Sum,
            Token::ForwardSlash | Token::Asterisk => Self::Product,
            Token::Lparen => Self::Call,
            _ => Self::Lowest,
        }
    }
}
