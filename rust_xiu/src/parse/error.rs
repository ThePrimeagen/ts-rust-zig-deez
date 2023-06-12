use std::num::ParseIntError;

use crate::lex::{self, Token};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Int(#[from] ParseIntError),
    #[error("unexpected token: `{0:?}`")]
    UnexpectedToken(Token),
    #[error("expected AST to contain {expected} statements, but instead found {actual}")]
    IncorrectStatementCount { expected: usize, actual: usize },
    #[error(transparent)]
    Lex(#[from] lex::Error),
}
