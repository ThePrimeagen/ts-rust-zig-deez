#![cfg(test)]

use anyhow::Result;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(&'a str),
    Int(&'a str),

    Eof,
    Equal,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    LSquirly,
    RSquirly,
    Function,
    Let,
}

pub struct Lexer<'a> {
    position: usize,
    read_position: usize,
    ch: u8,
    input: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            input,
        };

        lex.read_char();
        return lex;
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        self.skip_whitespace();

        let tok = match self.ch {
            b'{' => Token::LSquirly,
            b'}' => Token::RSquirly,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'+' => Token::Plus,
            b'=' => Token::Equal,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    _ => Token::Ident(ident),
                });
            },
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            0 => Token::Eof,
            _ => todo!("we need to implement this....")
        };

        self.read_char();
        return Ok(tok);
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> &'a str {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        return &self.input[pos..self.position];
    }

    fn read_int(&mut self) -> &'a str {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        return &self.input[pos..self.position];
    }
}

mod test {
    use anyhow::Result;

    use super::{Token, Lexer};

    #[test]
    fn get_next_token() -> Result<()> {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Equal,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::LSquirly,
            Token::RSquirly,
            Token::Comma,
            Token::Semicolon,
        ];

        for token in tokens {
            let next_token = lexer.next_token()?;
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }


        return Ok(());
    }

    #[test]
    fn get_next_complete() -> Result<()> {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);"# ;

        let mut lex = Lexer::new(input.into());

        let tokens = vec![
            Token::Let,
            Token::Ident("five"),
            Token::Equal,
            Token::Int("5"),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten"),
            Token::Equal,
            Token::Int("10"),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add"),
            Token::Equal,
            Token::Function,
            Token::Lparen,
            Token::Ident("x"),
            Token::Comma,
            Token::Ident("y"),
            Token::Rparen,
            Token::LSquirly,
            Token::Ident("x"),
            Token::Plus,
            Token::Ident("y"),
            Token::Semicolon,
            Token::RSquirly,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result"),
            Token::Equal,
            Token::Ident("add"),
            Token::Lparen,
            Token::Ident("five"),
            Token::Comma,
            Token::Ident("ten"),
            Token::Rparen,
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token()?;
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        return Ok(());
    }
}
