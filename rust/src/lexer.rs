use anyhow::Result;

#[derive(Debug, PartialEq)]
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

pub struct Lexer {
    position: usize,
    read_position: usize,
    ch: u8,
    input: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            position: 0,
            read_position: 0,
            ch: 0,
            input: input.into_bytes(),
        };
        lex.read_char();

        lex
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.ch {
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
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
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
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
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

        Ok(())
    }

    #[test]
    fn get_next_complete() -> Result<()> {
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
    fn gen_all_tokens() -> Result<()> {
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
}
