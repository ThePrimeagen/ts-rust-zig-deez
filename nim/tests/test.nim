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
      t_let, ident, equal, t_int, semicolon,
      t_let, ident, equal, t_int, semicolon,
      t_let, ident, equal, function, leftparen, ident, comma, ident, rightparen, leftsquirrel,
      ident, plus, ident, semicolon,
      rightsquirrel, semicolon,
      t_let, ident, equal, ident, leftparen, ident, comma, ident, rightparen, semicolon,
    ]
    check lexemes == expected
