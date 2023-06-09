use std::{iter::Peekable, str::Bytes};

use crate::token::Token;

pub struct Lexer<'a> {
    /// Source code string
    input: Peekable<Bytes<'a>>,
    /// Current position in `input` (points to current char)
    // position: usize,
    /// Current reading position in `input` (after current char)
    // read_position: usize,

    /// Current char under examination
    ch: u8,
}

impl Default for Lexer<'_> {
    fn default() -> Self {
        Self {
            input: "".bytes().peekable(),
            // position: Default::default(),
            // read_position: Default::default(),
            ch: Default::default(),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input: input.bytes().peekable(),
            ..Default::default()
        };
        lexer.read_char();
        lexer
    }
}

impl Lexer<'_> {
    /// Store the next character and advance
    /// the position in the input string.
    fn read_char(&mut self) {
        self.ch = match self.input.peek() {
            Some(ch) => *ch,
            None => b'\0',
        };

        self.input.next();
    }

    /// Yield the next character without advancing
    /// the position in the input string.
    fn peek_char(&mut self) -> u8 {
        match self.input.peek() {
            Some(ch) => *ch,
            None => b'\0',
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let b'a'..=b'z' | b'A'..=b'Z' | b'_' = self.ch {
            identifier.push(self.ch as char);
            self.read_char();
        }
        identifier
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();
        while let b'0'..=b'9' = self.ch {
            number.push(self.ch as char);
            self.read_char();
        }
        number
    }

    fn skip_whitespace(&mut self) {
        while let b' ' | b'\t' | b'\n' | b'\r' = self.ch {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                // early return is necessary here so we don't call
                // `self.read_char()` and advance past the last
                // character of the current identifier
                return Token::from(self.read_identifier());
            }
            b'0'..=b'9' => {
                // early return is necessary here so we don't call
                // `self.read_char()` and advance past the last
                // character of the current number
                return Token::from(self.read_number());
            }
            _ => {
                // TODO
                Token::from(self.ch)
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
";
        let tests = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::GreaterThan,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(String::from("10")),
            Token::Equal,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::NotEqual,
            Token::Int(String::from("9")),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for (i, expected) in tests.into_iter().enumerate() {
            let actual = lexer.next_token();
            assert_eq!(
                expected, actual,
                "tests[{i}] - tokentype wrong. expected={expected:?}, got={actual:?}"
            );
            match (expected, actual) {
                (Token::Ident(expected_literal), Token::Ident(actual_literal)) | (Token::Int(expected_literal), Token::Int(actual_literal)) => assert_eq!(expected_literal, actual_literal, "tests[{i}] - literal wrong. expected={expected_literal:?}, got={actual_literal:?}"),
                _ => {}
            }
        }
    }
}
