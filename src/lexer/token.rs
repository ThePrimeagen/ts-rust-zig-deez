
pub enum Token {
    Ident(String),
    Integer(String),
    Illegal,
    Eof,
    Int(String),
    Assign(String),
    Plus(String),
    Comma(String),
    Semicolon(String),
    Lparen(String),
    Rparen(String),
    Lbrace(String),
    Rbrace(String),
    Function(String),
    Let(String),
}

