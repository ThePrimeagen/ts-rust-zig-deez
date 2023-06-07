import std/[unittest, sugar, sequtils]
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
    var lexer = newLexer(source)
    let lexemes = collect:
      for a in toSeq(lexer.tokens()): a.kind
    let expected = @[
      tkLet, tkIdent, tkAssign, tkInt, tkSemicolon,
      tkLet, tkIdent, tkAssign, tkInt, tkSemicolon,
      tkLet, tkIdent, tkAssign, tkFunction, tkLParen, tkIdent, tkComma, tkIdent, tkRParen, tkLBrace,
      tkIdent, tkPlus, tkIdent, tkSemicolon,
      tkRBrace, tkSemicolon,
      tkLet, tkIdent, tkAssign, tkIdent, tkLParen, tkIdent, tkComma, tkIdent, tkRParen, tkSemicolon, tkEof
    ]
    check lexemes == expected
