use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit1, multispace0},
    combinator::map,
    error::Error,
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident(&'a [u8]),
    Int(&'a [u8]),

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
    input: &'a [u8],
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
            Self::parse_one_char_token,
            Self::parse_ident,
            Self::parse_integer,
        ))(self.input)
        {
            Ok((rest, token)) => {
                self.input = rest;
                Ok(token)
            }
            Err(_) => Err("Unexpected error when parsing token"),
        }
    }

    fn parse_one_char_token(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        alt((
            map(tag(b"="), |_| Token::Equal),
            map(tag(b"+"), |_| Token::Plus),
            map(tag(b","), |_| Token::Comma),
            map(tag(b";"), |_| Token::Semicolon),
            map(tag(b"("), |_| Token::LParen),
            map(tag(b")"), |_| Token::RParen),
            map(tag(b"{"), |_| Token::LSquirly),
            map(tag(b"}"), |_| Token::RSquirly)
        ))(input)
    }

    fn parse_ident(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        let (rest, ident) = take_while1(|c: u8| c.is_ascii_alphabetic())(input)?;
        let token = match ident {
            b"fn" => Token::Function,
            b"let" => Token::Let,
            _ => Token::Ident(ident),
        };
        Ok((rest, token))
    }

    fn parse_integer(input: &'a [u8]) -> IResult<&'a [u8], Token> {
        let (rest, int) = digit1(input)?;
        Ok((rest, Token::Int(int)))
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
    }

    #[test]
    fn get_next_complete() {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);"#;

        let mut lex = Lexer::new(input);

        let tokens = vec![
            Token::Let,
            Token::Ident("five".as_bytes()),
            Token::Equal,
            Token::Int("5".as_bytes()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".as_bytes()),
            Token::Equal,
            Token::Int("10".as_bytes()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".as_bytes()),
            Token::Equal,
            Token::Function,
            Token::LParen,
            Token::Ident("x".as_bytes()),
            Token::Comma,
            Token::Ident("y".as_bytes()),
            Token::RParen,
            Token::LSquirly,
            Token::Ident("x".as_bytes()),
            Token::Plus,
            Token::Ident("y".as_bytes()),
            Token::Semicolon,
            Token::RSquirly,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".as_bytes()),
            Token::Equal,
            Token::Ident("add".as_bytes()),
            Token::LParen,
            Token::Ident("five".as_bytes()),
            Token::Comma,
            Token::Ident("ten".as_bytes()),
            Token::RParen,
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
