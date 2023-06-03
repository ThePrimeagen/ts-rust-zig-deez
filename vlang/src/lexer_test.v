module lexer_test

import lexer

fn test_get_next_token() ?bool {
    let input := "=+(){},;"
    let mut lexer := lexer.new_lexer(input)

    let tokens := [
        Token.Assign,
        Token.Plus,
        Token.Lparen,
        Token.Rparen,
        Token.LSquirly,
        Token.RSquirly,
        Token.Comma,
        Token.Semicolon
    ]

    for token in tokens {
        let next_token := lexer.next_token()?
        println('expected: $token, received $next_token')
        if token != next_token {
            return false
        }
    }

    return true
}

fn test_get_next_complete() ?bool {
    let input := `let five = 5;
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
    `

    let mut lex := lexer.new_lexer(input)

    let tokens := [
        Token.Let,
        Token.Ident("five"),
        Token.Assign,
        Token.Int("5"),
        Token.Semicolon,
        Token.Let,
        Token.Ident("ten"),
        Token.Assign,
        Token.Int("10"),
        Token.Semicolon,
        Token.Let,
        Token.Ident("add"),
        Token.Assign,
        Token.Function,
        Token.Lparen,
        Token.Ident("x"),
        Token.Comma,
        Token.Ident("y"),
        Token.Rparen,
        Token.LSquirly,
        Token.Ident("x"),
        Token.Plus,
        Token.Ident("y"),
        Token.Semicolon,
        Token.RSquirly,
        Token.Semicolon,
        Token.Let,
        Token.Ident("result"),
        Token.Assign,
        Token.Ident("add"),
        Token.Lparen,
        Token.Ident("five"),
        Token.Comma,
        Token.Ident("ten"),
        Token.Rparen,
        Token.Semicolon,
        Token.Bang,
        Token.Dash,
        Token.ForwardSlash,
        Token.Asterisk,
        Token.Int("5"),
        Token.Semicolon,
        Token.Int("5"),
        Token.LessThan,
        Token.Int("10"),
        Token.GreaterThan,
        Token.Int("5"),
        Token.Semicolon,
        Token.If,
        Token.Lparen,
        Token.Int("5"),
        Token.LessThan,
        Token.Int("10"),
        Token.Rparen,
        Token.LSquirly,
        Token.Return,
        Token.True,
        Token.Semicolon,
        Token.RSquirly,
        Token.Else,
        Token.LSquirly,
        Token.Return,
        Token.False,
        Token.Semicolon,
        Token.RSquirly,
        Token.Int("10"),
        Token.Equal,
        Token.Int("10"),
        Token.Semicolon,
        Token.Int("10"),
        Token.NotEqual,
        Token.Int("9"),
        Token.Semicolon
    ]

    for token in tokens {
        let next_token := lex.next_token()?
        println('expected: $token, received $next_token')
        if token != next_token {
            return false
        }
    }

    return true
}

fn main() {
    if test_get_next_token()? {
        println('test_get_next_token passed')
    } else {
        println('test_get_next_token failed')
    }

    if test_get_next_complete()? {
        println('test_get_next_complete passed')
    } else {
        println('test_get_next_complete failed')
    }
}
