use std::fmt::{Display, Formatter};

use nom::{
    branch::alt,
    bytes::complete::{tag},
    character::complete::{digit1, multispace0, alpha1, alphanumeric0},
    combinator::{map, recognize},
    error::Error,
    IResult, sequence::pair, multi::many1,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    Int(String),

    Illegal,

    Eof,
    Assign,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,

    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
}

pub struct Lexer<'a> {
    input: &'a [u8],
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "Ident[{}]", s),
            Token::Int(s) => write!(f, "Int[{}]", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input.as_bytes(),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, &'static str> {
        let (rest, _) = multispace0::<_, Error<_>>(self.input).unwrap_or((self.input, &[]));
        self.input = rest;

        if self.input.is_empty() {
            return Ok(Token::Eof);
        }

        match alt((
            Self::parse_two_char_token,
            Self::parse_one_char_token,
            Self::parse_ident,
            Self::parse_integer,
        ))(self.input)
        {
            Ok((rest, token)) => {
                self.input = rest;
                Ok(token)
            }
            Err(_) => {
                self.input = self.input.split_at(1).1;
                Ok(Token::Illegal)
            },
        }
    }

    fn parse_one_char_token(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        alt((
            map(tag(b","), |_| Token::Comma),
            map(tag(b";"), |_| Token::Semicolon),
            map(tag(b"("), |_| Token::LParen),
            map(tag(b")"), |_| Token::RParen),
            map(tag(b"{"), |_| Token::LSquirly),
            map(tag(b"}"), |_| Token::RSquirly),

            map(tag(b"="), |_| Token::Assign),
            map(tag(b"!"), |_| Token::Bang),
            map(tag(b"+"), |_| Token::Plus),
            map(tag(b"-"), |_| Token::Minus),
            map(tag(b"*"), |_| Token::Asterisk),
            map(tag(b"/"), |_| Token::Slash),

            map(tag(b"<"), |_| Token::LessThan),
            map(tag(b">"), |_| Token::GreaterThan),
        ))(input)
    }

    fn parse_two_char_token(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        alt((
            map(tag(b"=="), |_| Token::Eq),
            map(tag(b"!="), |_| Token::NotEq),
        ))(input)
    }

    fn parse_ident(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        let (rest, ident) = recognize(pair(
            alt((alpha1, tag(b"_"))),
            alt((alphanumeric0, recognize(many1(tag(b"_"))))),
        ))(input)?;

        let token = match ident {
            b"fn" => Token::Function,
            b"let" => Token::Let,
            b"true" => Token::True,
            b"false" => Token::False,
            b"if" => Token::If,
            b"else" => Token::Else,
            b"return" => Token::Return,
            _ => Token::Ident(String::from_utf8(ident.to_vec()).unwrap()),
        };
        Ok((rest, token))
    }

    fn parse_integer(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        let (rest, int) = digit1(input)?;
        Ok((rest, Token::Int(String::from_utf8(int.to_vec()).unwrap())))
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};

    #[test]
    fn get_next_token() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Assign,
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
    }

    #[test]
    fn get_next_complete() {
        let input = r#"let five = 5;
            let ten = 10;

            let _ten = 10;

            let _ten1 = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5; 5 < 10 > 5

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;"#;

        let mut lex = Lexer::new(input);

        let tokens = vec![
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
            Token::Ident(String::from("_ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,

            Token::Let,
            Token::Ident(String::from("_ten1")),
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
            Token::LSquirly,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RSquirly,
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
            Token::If,
            Token::LParen,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RSquirly,
            Token::Else,
            Token::LSquirly,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RSquirly,
            Token::Int(String::from("10")),
            Token::Eq,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::NotEq,
            Token::Int(String::from("9")),
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token().unwrap();
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }
    }
}
