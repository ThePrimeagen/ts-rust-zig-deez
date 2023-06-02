mod error;
#[cfg(test)]
mod test;

pub use error::{Error, Result};

pub enum TokenKind {
    Ident,
    Int,

    Illegal,
    Eof,

    Lparen,
    Rparen,
    LSquirly,
    RSquirly,

    Assign,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Not,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

struct TokenTemp<'a> {
    kind: TokenKind,
    literal: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Int(String),

    Illegal,
    Eof,

    Lparen,
    Rparen,
    LSquirly,
    RSquirly,

    Assign,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Not,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug)]
pub struct Lexer {
    position: usize,
    read_position: usize,
    current_character: u8,
    input: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            current_character: 0,
            input: input.into_bytes(),
        };
        lex.read_char();

        lex
    }

    pub fn reset(&mut self) {
        self.position = 0;
        self.read_position = 0;
        self.current_character = 0;
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.current_character {
            b'{' => Token::LSquirly,
            b'}' => Token::RSquirly,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b'<' => Token::LessThan,
            b'>' => Token::GreaterThan,
            b'=' => match self.peek_char() {
                b'=' => {
                    self.read_char();

                    Token::Equal
                }
                _ => Token::Assign,
            },
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::ForwardSlash,
            b'!' => match self.peek_char() {
                b'=' => {
                    self.read_char();

                    Token::NotEqual
                }
                _ => Token::Not,
            },

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "true" => Token::True,
                    "false" => Token::False,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            0 => Token::Eof,
            _ => todo!("we need to implement this...."),
        };

        self.read_char();
        Ok(tok)
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_character = 0;
        } else {
            self.current_character = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }

        self.input[self.read_position]
    }

    fn skip_whitespace(&mut self) {
        while self.current_character.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;
        while self.current_character.is_ascii_alphabetic() || self.current_character == b'_' {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;
        while self.current_character.is_ascii_digit() {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }
}
