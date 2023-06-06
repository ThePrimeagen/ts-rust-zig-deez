import 'package:dart_deez/dart_deez.dart';
import 'package:test/test.dart';

void main() {
  group("Test Lexer", () {
    test("test nextToken()", () {
      const input = "== = != ! + - / * <= >= < > , ; ( ) { }";

      const tokens = [
        Token.equal(),
        Token.assign(),
        Token.notEqual(),
        Token.bang(),
        Token.plus(),
        Token.minus(),
        Token.slash(),
        Token.asterisk(),
        Token.lte(),
        Token.gte(),
        Token.lt(),
        Token.gt(),
        Token.comma(),
        Token.semicolon(),
        Token.lParen(),
        Token.rParen(),
        Token.lSquirly(),
        Token.rSquirly(),
        Token.eof(),
      ];

      final lexer = Lexer(input);

      for (final token in tokens) {
        expect(lexer.nextToken(), token);
      }
    });

    test("test nextToken() complete", () {
      const input = "let five = 5;"
          "let ten = 10;"
          "let add = fn(x, y) {"
          "x + y;"
          "};"
          "let result = add(five, ten);"
          "!-/*5;"
          "5 < 10 > 5;"
          "if (5 < 10) {"
          "return true;"
          "} else {"
          "return false;"
          "}"
          "10 == 10;"
          "10 != 9;";

      final lex = Lexer(input);

      const tokens = [
        Token.let(),
        Token.ident("five"),
        Token.assign(),
        Token.int("5"),
        Token.semicolon(),
        Token.let(),
        Token.ident("ten"),
        Token.assign(),
        Token.int("10"),
        Token.semicolon(),
        Token.let(),
        Token.ident("add"),
        Token.assign(),
        Token.fn(),
        Token.lParen(),
        Token.ident("x"),
        Token.comma(),
        Token.ident("y"),
        Token.rParen(),
        Token.lSquirly(),
        Token.ident("x"),
        Token.plus(),
        Token.ident("y"),
        Token.semicolon(),
        Token.rSquirly(),
        Token.semicolon(),
        Token.let(),
        Token.ident("result"),
        Token.assign(),
        Token.ident("add"),
        Token.lParen(),
        Token.ident("five"),
        Token.comma(),
        Token.ident("ten"),
        Token.rParen(),
        Token.semicolon(),
        Token.bang(),
        Token.minus(),
        Token.slash(),
        Token.asterisk(),
        Token.int("5"),
        Token.semicolon(),
        Token.int("5"),
        Token.lt(),
        Token.int("10"),
        Token.gt(),
        Token.int("5"),
        Token.semicolon(),
        Token.if_(),
        Token.lParen(),
        Token.int("5"),
        Token.lt(),
        Token.int("10"),
        Token.rParen(),
        Token.lSquirly(),
        Token.return_(),
        Token.true_(),
        Token.semicolon(),
        Token.rSquirly(),
        Token.else_(),
        Token.lSquirly(),
        Token.return_(),
        Token.false_(),
        Token.semicolon(),
        Token.rSquirly(),
        Token.int("10"),
        Token.equal(),
        Token.int("10"),
        Token.semicolon(),
        Token.int("10"),
        Token.notEqual(),
        Token.int("9"),
        Token.semicolon(),
        Token.eof(),
      ];

      for (final token in tokens) {
        expect(lex.nextToken(), token);
      }
    });
  });
}
