import Deez.Lexer
import Deez.Token

def getNextToken (input : String) (solution : Array Token) : Bool := Id.run do
  let mut l := Lexer.new input
  let mut lexxed := #[]

  for _ in solution do
    let (l', tok) := l.nextToken
    l := l'
    lexxed := lexxed.push tok

    if tok == Token.eof then
      return lexxed == solution
  lexxed == solution

def getNextComplete (input : String) (solution : Array Token) : Bool := Id.run do
  let mut l := Lexer.new input
  let mut lexxed := #[]

  for _ in solution do
    let (l', tok) := l.nextToken
    l := l'
    lexxed := lexxed.push tok

    if tok == Token.eof then
      return lexxed == solution
  lexxed == solution


def main : IO Unit := do
  IO.println "Testing Lexer..."
  let input := "=+(){},;"
  let ans := #[.assign, .plus, .lParen, .rParen, .lBrace, .rBrace, .comma, .semicolon]
  IO.println s!"getNextToken: {if getNextToken input ans then "PASSED" else "FAIL"}"

  let input := "let five = 5;
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
  let ans := #[
      Token.let,
      Token.ident "five",
      Token.assign,
      Token.int "5",
      Token.semicolon,
      Token.let,
      Token.ident "ten",
      Token.assign,
      Token.int "10",
      Token.semicolon,
      Token.let,
      Token.ident "add",
      Token.assign,
      Token.function,
      Token.lParen,
      Token.ident "x",
      Token.comma,
      Token.ident "y",
      Token.rParen,
      Token.lBrace,
      Token.ident "x",
      Token.plus,
      Token.ident "y",
      Token.semicolon,
      Token.rBrace,
      Token.semicolon,
      Token.let,
      Token.ident "result",
      Token.assign,
      Token.ident "add",
      Token.lParen,
      Token.ident "five",
      Token.comma,
      Token.ident "ten",
      Token.rParen,
      Token.semicolon,

      Token.bang,
      Token.minus,
      Token.slash,
      Token.asterisk,
      Token.int "5",
      Token.semicolon,
      Token.int "5",
      Token.lessThan,
      Token.int "10",
      Token.greaterThan,
      Token.int "5",
      Token.semicolon,
      Token.if,
      Token.lParen,
      Token.int "5",
      Token.lessThan,
      Token.int "10",
      Token.rParen,
      Token.lBrace,
      Token.return,
      Token.true,
      Token.semicolon,
      Token.rBrace,
      Token.else,
      Token.lBrace,
      Token.return,
      Token.false,
      Token.semicolon,
      Token.rBrace,

      Token.int "10",
      Token.equal,
      Token.int "10",
      Token.semicolon,
      Token.int "10",
      Token.notEqual,
      Token.int "9",
      Token.semicolon,

      Token.eof
  ]
  IO.println s!"getNextComplete: {if getNextComplete input ans then "PASSED" else "FAIL"}"

#eval main
