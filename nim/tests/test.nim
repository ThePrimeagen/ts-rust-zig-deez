import std/[unittest, sugar]
import ../src/lexer

suite "lexer tests":
  test "lexemes match":
    let source = """
      let five = 5;
      let ten = 10;
      let add = fn(x, y) {
          x + y;
      };
      let result = add(five, ten);
    """
    let lexemes = collect:
      for val in lex(source): val.kind
    let expected = @[
      tkLet, tkIdent, tkEqual, tkInt, tkSemicolon,
      tkLet, tkIdent, tkEqual, tkInt, tkSemicolon,
      tkLet, tkIdent, tkEqual, tkFunction, tkLParen, tkIdent, tkComma, tkIdent, tkRParen, tkLBrace,
      tkIdent, tkPlus, tkIdent, tkSemicolon,
      tkRBrace, tkSemicolon,
      tkLet, tkIdent, tkEqual, tkIdent, tkLParen, tkIdent, tkComma, tkIdent, tkRParen, tkSemicolon,
    ]
    check lexemes == expected
