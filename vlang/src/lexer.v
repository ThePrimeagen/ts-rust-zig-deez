module lexer

import anyhow

pub enum Token {
    Ident(str),
    Int(str),

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

pub struct Lexer {
    position usize,
    read_position usize,
    ch u8,
    input []u8,
}

pub fn new_lexer(input string) Lexer {
    let mut lex := Lexer{
        position: 0,
        read_position: 0,
        ch: 0,
        input: input.bytes(),
    }
    lex.read_char()
    return lex
}

pub fn next_token(mut lex &Lexer) ?Token {
    lex.skip_whitespace()

    let tok := match lex.ch {
        b'{' => Token.LSquirly,
        b'}' => Token.RSquirly,
        b'(' => Token.Lparen,
        b')' => Token.Rparen,
        b',' => Token.Comma,
        b';' => Token.Semicolon,
        b'+' => Token.Plus,
        b'-' => Token.Dash,
        b'!' => {
            if lex.peek() == b'=' {
                lex.read_char()
                Token.NotEqual
            } else {
                Token.Bang
            }
        }
        b'>' => Token.GreaterThan,
        b'<' => Token.LessThan,
        b'*' => Token.Asterisk,
        b'/' => Token.ForwardSlash,
        b'=' => {
            if lex.peek() == b'=' {
                lex.read_char()
                Token.Equal
            } else {
                Token.Assign
            }
        }
        b'a'..=b'z', b'A'..=b'Z', b'_' => {
            let ident := lex.read_ident()
            match ident {
                "fn" => Token.Function,
                "let" => Token.Let,
                "if" => Token.If,
                "false" => Token.False,
                "true" => Token.True,
                "return" => Token.Return,
                "else" => Token.Else,
                _ => Token.Ident(ident)
            }
        }
        b'0'..=b'9' => Token.Int(lex.read_int()),
        0 => Token.Eof,
        _ => any_err('we need to implement this....')
    }

    lex.read_char()
    return tok
}

fn (mut lex &Lexer) peek() u8 {
    if lex.read_position >= len(lex.input) {
        return 0
    } else {
        return lex.input[lex.read_position]
    }
}

fn (mut lex &Lexer) read_char() {
    if lex.read_position >= len(lex.input) {
        lex.ch = 0
    } else {
        lex.ch = lex.input[lex.read_position]
    }

    lex.position = lex.read_position
    lex.read_position += 1
}

fn (mut lex &Lexer) skip_whitespace() {
    while lex.ch.is_ascii_whitespace() {
        lex.read_char()
    }
}

fn (mut lex &Lexer) read_ident() str {
    let pos := lex.position
    while lex.ch.is_ascii_alphabetic() || lex.ch == b'_' {
        lex.read_char()
    }

    return string(lex.input[pos..lex.position])
}

fn (mut lex &Lexer) read_int() str {
    let pos := lex.position
    while lex.ch.is_ascii_digit() {
        lex.read_char()
    }

    return string(lex.input[pos..lex.position])
}
