import gleeunit
import gleeunit/should
import token/token
import lexer/lexer

pub fn main() {
  gleeunit.main()
}

pub fn example_from_book_test() {
  "let five = 5;
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
  10 != 9;"
  |> lexer.init()
  |> should.equal([
    token.Let,
    token.Ident("five"),
    token.Assign,
    token.Int("5"),
    token.Semicolon,
    token.Let,
    token.Ident("ten"),
    token.Assign,
    token.Int("10"),
    token.Semicolon,
    token.Let,
    token.Ident("add"),
    token.Assign,
    token.Function,
    token.LeftParen,
    token.Ident("x"),
    token.Comma,
    token.Ident("y"),
    token.RightParen,
    token.LeftBrace,
    token.Ident("x"),
    token.Plus,
    token.Ident("y"),
    token.Semicolon,
    token.RightBrace,
    token.Semicolon,
    token.Let,
    token.Ident("result"),
    token.Assign,
    token.Ident("add"),
    token.LeftParen,
    token.Ident("five"),
    token.Comma,
    token.Ident("ten"),
    token.RightParen,
    token.Semicolon,
    token.Bang,
    token.Minus,
    token.Slash,
    token.Asterisk,
    token.Int("5"),
    token.Semicolon,
    token.Int("5"),
    token.LessThan,
    token.Int("10"),
    token.GreaterThan,
    token.Int("5"),
    token.Semicolon,
    token.If,
    token.LeftParen,
    token.Int("5"),
    token.LessThan,
    token.Int("10"),
    token.RightParen,
    token.LeftBrace,
    token.Return,
    token.True,
    token.Semicolon,
    token.RightBrace,
    token.Else,
    token.LeftBrace,
    token.Return,
    token.False,
    token.Semicolon,
    token.RightBrace,
    token.Int("10"),
    token.Equal,
    token.Int("10"),
    token.Semicolon,
    token.Int("10"),
    token.NotEqual,
    token.Int("9"),
    token.Semicolon,
  ])
}
