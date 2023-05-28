use anyhow::Result;
use nom::{
    branch::alt,
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::map,
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Int(String),

    Illegal,
    Eof,
    Equal,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Function,
    Let,
}

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer { input }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        let (input, _) = multispace0::<&str, nom::error::Error<&str>>(self.input).unwrap();
        self.input = input;

        if self.input.is_empty() {
            return Ok(Token::Eof);
        }

        let (input, token) = alt((Self::one_char_token, Self::ident, Self::integer))(self.input)
            .map_err(|_| "Invalid token".to_string())?;

        self.input = input;

        Ok(token)
    }

    fn one_char_token(input: &str) -> IResult<&str, Token> {
        alt((
            map(char('{'), |_| Token::LSquirly),
            map(char('}'), |_| Token::RSquirly),
            map(char('('), |_| Token::LParen),
            map(char(')'), |_| Token::RParen),
            map(char(','), |_| Token::Comma),
            map(char(';'), |_| Token::Semicolon),
            map(char('+'), |_| Token::Plus),
            map(char('='), |_| Token::Equal),
        ))(input)
    }

    fn ident(input: &str) -> IResult<&str, Token> {
        map(alpha1, |s: &str| match s {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Ident(s.into()),
        })(input)
    }

    fn integer(input: &str) -> IResult<&str, Token> {
        map(digit1, |s: &str| Token::Int(s.into()))(input)
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};
    use anyhow::Result;

    #[test]
    fn get_next_token() -> Result<()> {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Equal,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
            Token::Comma,
            Token::Semicolon,
        ];

        for token in tokens {
            let next_token = lexer.next_token().unwrap();
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
            let result = add(five, ten);"#;

        let mut lex = Lexer::new(input);

        let tokens = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Equal,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Equal,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Equal,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LSquirly,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RSquirly,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Equal,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token().unwrap();
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        return Ok(());
    }
}
