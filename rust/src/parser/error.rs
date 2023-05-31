use crate::lexer;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected AST to contain {expected} statements, but instead found {actual}")]
    IncorrectStatementCount { expected: usize, actual: usize },
    #[error(transparent)]
    Lex(#[from] lexer::Error),
}
