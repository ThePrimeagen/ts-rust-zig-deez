use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    // Identifiers + literals
    Ident(String),
    Int(String),
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, ""),
            Token::Eof => write!(f, "\0"),
            Token::Ident(value) | Token::Int(value) => write!(f, "{value}"),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}

impl From<u8> for Token {
    fn from(ch: u8) -> Self {
        match ch {
            b'=' => Self::Assign,
            b';' => Self::Semicolon,
            b'(' => Self::LParen,
            b')' => Self::RParen,
            b',' => Self::Comma,
            b'+' => Self::Plus,
            b'-' => Self::Minus,
            b'!' => Self::Bang,
            b'*' => Self::Asterisk,
            b'/' => Self::Slash,
            b'<' => Self::LessThan,
            b'>' => Self::GreaterThan,
            b'{' => Self::LBrace,
            b'}' => Self::RBrace,
            b'\0' => Self::Eof,
            _ => Self::Illegal,
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "==" => Self::Equal,
            "!=" => Self::NotEqual,
            "fn" => Self::Function,
            "let" => Self::Let,
            "true" => Self::True,
            "false" => Self::False,
            "if" => Self::If,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => {
                if value.bytes().all(|b| b.is_ascii_digit()) {
                    Self::Int(value)
                } else {
                    Self::Ident(value)
                }
            }
        }
    }
}
