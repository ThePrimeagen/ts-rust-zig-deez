use anyhow::Result;

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    Int(&'a str),

    Illegal,
    Eof,
    Assign,

    Bang,
    Dash,
    ForwardSlash,
    Asterisk,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    LSquirly,
    RSquirly,

    Function,
    Let,

    If,
    Else,
    Return,
    True,
    False,
}

pub struct Lexer<'a> {
    position: usize,
    read_position: usize,
    ch: u8,
    input: &'a [u8],
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            input: input.as_bytes(),
        };
        lex.read_char();

        return lex;
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.ch {
            b'{' => Token::LSquirly,
            b'}' => Token::RSquirly,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'+' => Token::Plus,
            b'-' => Token::Dash,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            b'>' => Token::GreaterThan,
            b'<' => Token::LessThan,
            b'*' => Token::Asterisk,
            b'/' => Token::ForwardSlash,
            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "if" => Token::If,
                    "false" => Token::False,
                    "true" => Token::True,
                    "return" => Token::Return,
                    "else" => Token::Else,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            0 => Token::Eof,
            _ => Token::Illegal,
        };

        self.read_char();
        return Ok(tok);
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> &str {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        return unsafe { std::str::from_utf8_unchecked(&self.input[pos..self.position]) };
    }

    fn read_int(&mut self) -> &str {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        return unsafe { std::str::from_utf8_unchecked(&self.input[pos..self.position]) };
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use super::{Lexer, Token};

    #[test]
    fn get_next_token() -> Result<()> {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Assign,
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
            let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;^
        "#;

        let mut lex = Lexer::new(input);

        use Token::*;

        let tokens = [
            Let,
            Ident("five"),
            Assign,
            Int("5"),
            Semicolon,
            Let,
            Ident("ten"),
            Assign,
            Int("10"),
            Semicolon,
            Let,
            Ident("add"),
            Assign,
            Function,
            Lparen,
            Ident("x"),
            Comma,
            Ident("y"),
            Rparen,
            LSquirly,
            Ident("x"),
            Plus,
            Ident("y"),
            Semicolon,
            RSquirly,
            Semicolon,
            Let,
            Ident("result"),
            Assign,
            Ident("add"),
            Lparen,
            Ident("five"),
            Comma,
            Ident("ten"),
            Rparen,
            Semicolon,
            Bang,
            Dash,
            ForwardSlash,
            Asterisk,
            Int("5"),
            Semicolon,
            Int("5"),
            LessThan,
            Int("10"),
            GreaterThan,
            Int("5"),
            Semicolon,
            If,
            Lparen,
            Int("5"),
            LessThan,
            Int("10"),
            Rparen,
            LSquirly,
            Return,
            True,
            Semicolon,
            RSquirly,
            Else,
            LSquirly,
            Return,
            False,
            Semicolon,
            RSquirly,
            Int("10"),
            Equal,
            Int("10"),
            Semicolon,
            Int("10"),
            NotEqual,
            Int("9"),
            Semicolon,
            Illegal,
            Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token()?;
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        return Ok(());
    }
}
