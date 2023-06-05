open Assert
open Test

let testTokenSequence = (input, expected) => {
  let rec loop = (lexer, expected, index) => {
    switch expected {
    | list{} => ()
    | list{eToken, ...rest} => {
        let (nextLexer, token) = Lexer.nextToken(lexer)

        assertTokensEqual(token, eToken)
        loop(nextLexer, rest, index + 1)
      }
    }
  }
  loop(Lexer.init(input), expected, 0)
}

test("Test next token symbols", () => {
  let input = "=+(){},;"

  let expected = list{
    Token.Assign,
    Token.Plus,
    Token.LParen,
    Token.RParen,
    Token.LSquirly,
    Token.RSquirly,
    Token.Comma,
    Token.Semicolon,
  }

  testTokenSequence(input, expected)
})

test("Test next token but more", () => {
  let input = `
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
  `

  let expected = list{
    Token.Let,
    Token.Ident("five"),
    Token.Assign,
    Token.Integer("5"),
    Token.Semicolon,
    Token.Let,
    Token.Ident("ten"),
    Token.Assign,
    Token.Integer("10"),
    Token.Semicolon,
    Token.Let,
    Token.Ident("add"),
    Token.Assign,
    Token.Function,
    Token.LParen,
    Token.Ident("x"),
    Token.Comma,
    Token.Ident("y"),
    Token.RParen,
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
    Token.LParen,
    Token.Ident("five"),
    Token.Comma,
    Token.Ident("ten"),
    Token.RParen,
    Token.Semicolon,
    Token.EOF,
  }

  testTokenSequence(input, expected)
})
