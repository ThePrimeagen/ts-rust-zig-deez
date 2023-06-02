// use anyhow::Result;
use super::{Lexer, Token};

type DynResult<T> = Result<T, Box<dyn std::error::Error>>;

#[test]
fn get_next_token() -> DynResult<()> {
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

    Ok(())
}

#[test]
fn get_next_complete() -> DynResult<()> {
    let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);"#;

    let mut lex = Lexer::new(input.into());

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
        Token::Ident(String::from("add")),
        Token::Assign,
        Token::Function,
        Token::Lparen,
        Token::Ident(String::from("x")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::Rparen,
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
        Token::Lparen,
        Token::Ident(String::from("five")),
        Token::Comma,
        Token::Ident(String::from("ten")),
        Token::Rparen,
        Token::Semicolon,
        Token::Eof,
    ];

    for token in tokens {
        let next_token = lex.next_token()?;
        println!("expected: {:?}, received {:?}", token, next_token);
        assert_eq!(token, next_token);
    }

    Ok(())
}

#[test]
fn gen_all_tokens() -> DynResult<()> {
    let input = r#"let rust = true;
let go = false;

let equals = fn(x, y) {
    if (!(x == y)) {
        return x;
    }

    if (x != y) {
        return y;
    } else {
        return false;
    }
};


let n1 = 9;
let n2 = 6
let n3 = (n1 > n2);
let n4 = (n2 < n1);

let result = equals(rust, go);
let pos_and_neg_inf = 6 + 9 - 6 * 9 / 0;"#;

    let mut lex = Lexer::new(input.to_owned());

    let expected = [
        Token::Let,
        Token::Ident("rust".to_owned()),
        Token::Assign,
        Token::True,
        Token::Semicolon,
        Token::Let,
        Token::Ident("go".to_owned()),
        Token::Assign,
        Token::False,
        Token::Semicolon,
        Token::Let,
        Token::Ident("equals".to_owned()),
        Token::Assign,
        Token::Function,
        Token::Lparen,
        Token::Ident("x".to_owned()),
        Token::Comma,
        Token::Ident("y".to_owned()),
        Token::Rparen,
        Token::LSquirly,
        Token::If,
        Token::Lparen,
        Token::Not,
        Token::Lparen,
        Token::Ident("x".to_owned()),
        Token::Equal,
        Token::Ident("y".to_owned()),
        Token::Rparen,
        Token::Rparen,
        Token::LSquirly,
        Token::Return,
        Token::Ident("x".to_owned()),
        Token::Semicolon,
        Token::RSquirly,
        Token::If,
        Token::Lparen,
        Token::Ident("x".to_owned()),
        Token::NotEqual,
        Token::Ident("y".to_owned()),
        Token::Rparen,
        Token::LSquirly,
        Token::Return,
        Token::Ident("y".to_owned()),
        Token::Semicolon,
        Token::RSquirly,
        Token::Else,
        Token::LSquirly,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RSquirly,
        Token::RSquirly,
        Token::Semicolon,
        Token::Let,
        Token::Ident("n".to_owned()),
        Token::Int("1".to_owned()),
        Token::Assign,
        Token::Int("9".to_owned()),
        Token::Semicolon,
        Token::Let,
        Token::Ident("n".to_owned()),
        Token::Int("2".to_owned()),
        Token::Assign,
        Token::Int("6".to_owned()),
        Token::Let,
        Token::Ident("n".to_owned()),
        Token::Int("3".to_owned()),
        Token::Assign,
        Token::Lparen,
        Token::Ident("n".to_owned()),
        Token::Int("1".to_owned()),
        Token::GreaterThan,
        Token::Ident("n".to_owned()),
        Token::Int("2".to_owned()),
        Token::Rparen,
        Token::Semicolon,
        Token::Let,
        Token::Ident("n".to_owned()),
        Token::Int("4".to_owned()),
        Token::Assign,
        Token::Lparen,
        Token::Ident("n".to_owned()),
        Token::Int("2".to_owned()),
        Token::LessThan,
        Token::Ident("n".to_owned()),
        Token::Int("1".to_owned()),
        Token::Rparen,
        Token::Semicolon,
        Token::Let,
        Token::Ident("result".to_owned()),
        Token::Assign,
        Token::Ident("equals".to_owned()),
        Token::Lparen,
        Token::Ident("rust".to_owned()),
        Token::Comma,
        Token::Ident("go".to_owned()),
        Token::Rparen,
        Token::Semicolon,
        Token::Let,
        Token::Ident("pos_and_neg_inf".to_owned()),
        Token::Assign,
        Token::Int("6".to_owned()),
        Token::Plus,
        Token::Int("9".to_owned()),
        Token::Minus,
        Token::Int("6".to_owned()),
        Token::Asterisk,
        Token::Int("9".to_owned()),
        Token::ForwardSlash,
        Token::Int("0".to_owned()),
        Token::Semicolon,
        Token::Eof,
    ];

    for token in expected {
        let next_token = lex.next_token()?;
        println!("expected: {:?}, received {:?}", token, next_token);
        assert_eq!(token, next_token);
    }

    Ok(())
}
