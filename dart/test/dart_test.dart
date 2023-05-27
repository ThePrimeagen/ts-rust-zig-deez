import 'package:dart_deez/dart_deez.dart';
import 'package:test/test.dart';

void main() {
  test("test nextToken()", () {
    const input = "=+(){},;";

    const tokens = [
      Token(TokenType.equal, "="),
      Token(TokenType.plus, "+"),
      Token(TokenType.lParen, "("),
      Token(TokenType.rParen, ")"),
      Token(TokenType.lSquirly, "{"),
      Token(TokenType.rSquirly, "}"),
      Token(TokenType.comma, ","),
      Token(TokenType.semicolon, ";"),
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
      Token(TokenType.let, "let"),
      Token(TokenType.ident, "five"),
      Token(TokenType.equal, "="),
      Token(TokenType.int, "5"),
      Token(TokenType.semicolon, ";"),
      Token(TokenType.let, "let"),
      Token(TokenType.ident, "ten"),
      Token(TokenType.equal, "="),
      Token(TokenType.int, "10"),
      Token(TokenType.semicolon, ";"),
      Token(TokenType.let, "let"),
      Token(TokenType.ident, "add"),
      Token(TokenType.equal, "="),
      Token(TokenType.function, "fn"),
      Token(TokenType.lParen, "("),
      Token(TokenType.ident, "x"),
      Token(TokenType.comma, ","),
      Token(TokenType.ident, "y"),
      Token(TokenType.rParen, ")"),
      Token(TokenType.lSquirly, "{"),
      Token(TokenType.ident, "x"),
      Token(TokenType.plus, "+"),
      Token(TokenType.ident, "y"),
      Token(TokenType.semicolon, ";"),
      Token(TokenType.rSquirly, "}"),
      Token(TokenType.semicolon, ";"),
      Token(TokenType.let, "let"),
      Token(TokenType.ident, "result"),
      Token(TokenType.equal, "="),
      Token(TokenType.ident, "add"),
      Token(TokenType.lParen, "("),
      Token(TokenType.ident, "five"),
      Token(TokenType.comma, ","),
      Token(TokenType.ident, "ten"),
      Token(TokenType.rParen, ")"),
      Token(TokenType.semicolon, ";"),
      Token(TokenType.eof, ""),
    ];

    for (final token in tokens) {
      expect(lex.nextToken(), token);
    }
  });
}
