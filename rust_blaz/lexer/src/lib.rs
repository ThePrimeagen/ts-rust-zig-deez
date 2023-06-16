#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code)]

mod lexer;
mod token;

pub use lexer::Lexer;
pub use token::Token;
