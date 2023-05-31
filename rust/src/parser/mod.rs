mod error;
#[cfg(test)]
mod test;

use std::mem;

use crate::{
    ast::AST,
    lexer::{Lexer, Token},
};
use error::Result;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self> {
        let current_token = lexer.next_token()?;
        let next_token = lexer.next_token()?;

        Ok(Self {
            lexer,
            current_token,
            next_token,
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
        todo!()
    }
}
