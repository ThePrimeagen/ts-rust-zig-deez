use brainfuck::{
    program::Program,
    tape::{ArrayTape, ModArrayTape},
    Interpreter,
};
use std::io::{self, Cursor, Read, Write};
use stringreader::StringReader;

#[derive(Debug, PartialEq)]
enum Token {
    EOF,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semi,
    Plus,
    Equal,
    Ident(String),
    Number(String),
    Illegal(char),
}

const LEXER: &'static str = include_str!("../lexer.b");
const PARSER: &'static str = include_str!("../parser.b");

macro_rules! expect_tokens {
    ($input: expr, $expected: expr) => {{
        let mut stdin = StringReader::new($input);
        let mut stdout = Vec::new();

        let program = Program::parse(LEXER).unwrap();
        let mut interp = Interpreter::<ModArrayTape>::new(program, &mut stdin, &mut stdout);
        interp.run().unwrap();

        let mut output_tokens = Vec::new();
        let mut iter = stdout.iter();

        while let Some(v) = iter.next() {
            output_tokens.push(match v {
                0 => Token::EOF,
                1 => Token::LeftParen,
                2 => Token::RightParen,
                3 => Token::LeftBrace,
                4 => Token::RightBrace,
                5 => Token::Comma,
                6 => Token::Semi,
                7 => Token::Plus,
                8 => Token::Equal,
                9 => {
                    let mut ident = String::new();
                    while let Some(&v) = iter.next() {
                        if v == 0 {
                            break;
                        }
                        ident.push(v.into());
                    }
                    Token::Ident(ident)
                }
                10 => {
                    let mut num = String::new();
                    while let Some(&v) = iter.next() {
                        if v == 0 {
                            break;
                        }
                        num.push(v.into());
                    }
                    Token::Number(num)
                }
                11 => Token::Illegal((*iter.next().unwrap()).into()),
                other => panic!("unexpected code: {}", other),
            });
        }

        assert_eq!(output_tokens, $expected);
    }};
}

#[test]
fn test_lexer() {
    expect_tokens!("123\0", [Token::Number("123".into()), Token::EOF]);

    expect_tokens!(
        "123 + 5\0",
        [
            Token::Number("123".into()),
            Token::Plus,
            Token::Number("5".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "123+5\0",
        [
            Token::Number("123".into()),
            Token::Plus,
            Token::Number("5".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "ABD+05\0",
        [
            Token::Ident("ABD".into()),
            Token::Plus,
            Token::Number("05".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "ABD+05\0",
        [
            Token::Ident("ABD".into()),
            Token::Plus,
            Token::Number("05".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "a23+05\0",
        [
            Token::Ident("a23".into()),
            Token::Plus,
            Token::Number("05".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "aB5 + 05() 95\0",
        [
            Token::Ident("aB5".into()),
            Token::Plus,
            Token::Number("05".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Number("95".into()),
            Token::EOF
        ]
    );

    expect_tokens!(
        "   ();[] asdf(\0",
        [
            Token::LeftParen,
            Token::RightParen,
            Token::Semi,
            Token::Illegal('['),
            Token::Illegal(']'),
            Token::Ident("asdf".into()),
            Token::LeftParen,
            Token::EOF
        ]
    );

    expect_tokens!(
        "a123 B123 bA9 Z929\0",
        [
            Token::Ident("a123".into()),
            Token::Ident("B123".into()),
            Token::Ident("bA9".into()),
            Token::Ident("Z929".into()),
            Token::EOF,
        ]
    );

    expect_tokens!(
        "Ao oA oZ Zo ao oa zo oz fdsa\0",
        [
            Token::Ident("Ao".into()),
            Token::Ident("oA".into()),
            Token::Ident("oZ".into()),
            Token::Ident("Zo".into()),
            Token::Ident("ao".into()),
            Token::Ident("oa".into()),
            Token::Ident("zo".into()),
            Token::Ident("oz".into()),
            Token::Ident("fdsa".into()),
            Token::EOF,
        ]
    );
}

fn main() {
    let mut stdin: Cursor<&str> = Cursor::new("(\0");
    let mut tokens = Vec::new();

    let lexer = Program::parse(LEXER).unwrap();
    let mut interp = Interpreter::<ModArrayTape>::new(lexer, &mut stdin, &mut tokens);
    interp.run().unwrap();

    println!("tokens: {:?}", tokens);

    let mut tokens = Cursor::new(tokens);
    let mut nodes = Vec::new();

    let program = Program::parse(PARSER).unwrap();
    let mut interp = Interpreter::<ModArrayTape>::new(program, &mut tokens, &mut nodes);
    interp.run().unwrap();

    println!(" nodes: {:?}", nodes);
}
