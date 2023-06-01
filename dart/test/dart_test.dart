import 'package:dart_deez/dart_deez.dart';
import 'package:test/test.dart';

bool testLetStatement(Statement stmt, String name) {
  if (stmt.tokenLiteral() != 'let') {
    return false;
  }
  if (stmt is! LetStatement) {
    return false;
  }
  if (stmt.name.value != name) {
    return false;
  }
  if (stmt.name.tokenLiteral() != name) {
    return false;
  }
  return true;
}

void checkParserErrors(Parser parser) {
  expect(parser.errors, isEmpty);
}

void main() {
  group('Lexer/Tokenizer', () {
    test("test Lexer nextToken()", () {
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

    test("test Lexer nextToken() complete (premature)", () {
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
  });

  group('Parser', () {
    group('Let Statement', () {
      test(
        'should return 3 statements, no errors, and the expected identifiers',
        () async {
          // arrange
          const input = "let x = 5;"
              "let y = 10;"
              "let foobar = 838383;";
          const expectedIdentifiers = [
            "x",
            "y",
            "foobar",
          ];
          final lex = Tokenizer(input);
          final parser = Parser(lex);

          // act
          final program = parser.parseProgram();

          // assert
          checkParserErrors(parser);
          expect(program, isNotNull);
          expect(program, isA<Program>());
          expect(parser.errors, isEmpty);
          expect(program.statements.length, 3);
          for (var i = 0; i < expectedIdentifiers.length; i++) {
            print(program.statements[i]);
            expect(
                testLetStatement(program.statements[i], expectedIdentifiers[i]),
                isTrue);
          }
        },
      );

      test(
        'should throw errors when Let Staement is incomplete/invalid',
        () async {
          // arrange
          const input = "let x 5;"
              "let = 10;"
              "let 838383;";
          final lex = Tokenizer(input);
          final parser = Parser(lex);

          // act
          final _ = parser.parseProgram();

          // assert
          expect(parser.errors.length, 3);
        },
      );
    });

    group('Return Statement', () {
      test(
        'should return 3 statements, no errors, and the expected identifiers',
        () async {
          // arrange
          const input = "return 5;"
              "return 10;"
              "return 993322;";
          final lex = Tokenizer(input);
          final parser = Parser(lex);

          // act
          final program = parser.parseProgram();

          // assert
          checkParserErrors(parser);
          expect(program, isNotNull);
          expect(program, isA<Program>());
          expect(parser.errors, isEmpty);
          expect(program.statements.length, 3);
          for (final stmt in program.statements) {
            print(stmt);
            expect(stmt, isA<ReturnStatement>());
            expect(stmt.tokenLiteral(), 'return');
          }
        },
      );
    });

    group('Identifier Expression', () {
      test(
        'should return 1 statements, no errors, and output = input',
        () async {
          // arrange
          const input = "foobar;";
          final lex = Tokenizer(input);
          final parser = Parser(lex);

          // act
          final program = parser.parseProgram();

          // assert
          expect(program.statements.length, 1);
          final stmt = program.statements[0];
          expect(stmt, isA<ExpressionStatement>());
          final expr = (stmt as ExpressionStatement).expression;
          expect(expr, isA<Identifier>());
          expect(expr.tokenLiteral(), input.replaceFirst(';', ''));
        },
      );
    });
  });
}
