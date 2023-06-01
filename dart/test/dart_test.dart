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

  test("test nextToken() complete for realz !-/*5;", () {
    const input = "let five = 5;"
        "let ten = 10;"
        "let add = fn(x, y) {"
        "    x + y;"
        "};"
        "let result = add(five, ten);"
        "!-/*5;"
        "5 < 10 > 5;"
        "if (5 < 10) {"
        "return true;"
        "} else {"
        " return false;"
        "}"
        "10 == 10; "
        "10 != 9;";

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
      Token.bang(),
      Token.dash(),
      Token.slash(),
      Token.asterisk(),
      Token.int('5'),
      Token.semicolon(),
      Token.int('5'),
      Token.lt(),
      Token.int('10'),
      Token.gt(),
      Token.int('5'),
      Token.semicolon(),
      Token.if_(),
      Token.lParen(),
      Token.int('5'),
      Token.lt(),
      Token.int('10'),
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
      // 10 == 10;
      Token.int('10'),
      Token.equal(),
      Token.int('10'),
      Token.semicolon(),
      // 10 != 9;
      Token.int('10'),
      Token.neq(),
      Token.int('9'),
      Token.semicolon(),
      Token(TokenType.eof, ""),
    ];

    for (final token in tokens) {
      expect(lex.nextToken(), token);
    }
  });
}
