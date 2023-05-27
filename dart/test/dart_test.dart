import 'package:dart_deez/dart_deez.dart';
import 'package:test/test.dart';

void main() {
  test("test nextToken()", () {
    const input = "=+(){},;";

    const tokens = [
      Token.equal(),
      Token.plus(),
      Token.lParen(),
      Token.rParen(),
      Token.lSquirly(),
      Token.rSquirly(),
      Token.comma(),
      Token.semicolon(),
    ];

    final lexer = Tokenizer(input);

    for (final token in tokens) {
      expect(lexer.nextToken(), token);
    }
  });

  test("test nextToken() complete", () {
    const input = "let five = 5;"
        "let ten = 10;"
        "let add = fn(x, y) {"
        "    x + y;"
        "};"
        "let result = add(five, ten);";

    final lex = Tokenizer(input);

    const tokens = [
      Token.let(),
      Token.ident("five"),
      Token.equal(),
      Token.int("5"),
      Token.semicolon(),
      Token.let(),
      Token.ident("ten"),
      Token.equal(),
      Token.int("10"),
      Token.semicolon(),
      Token.let(),
      Token.ident("add"),
      Token.equal(),
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
      Token.equal(),
      Token.ident("add"),
      Token.lParen(),
      Token.ident("five"),
      Token.comma(),
      Token.ident("ten"),
      Token.rParen(),
      Token.semicolon(),
      Token.eof(),
    ];

    for (final token in tokens) {
      expect(lex.nextToken(), token);
    }
  });
}
