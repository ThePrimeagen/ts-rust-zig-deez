// ignore_for_file: missing_whitespace_between_adjacent_strings

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

List<Token> lexTester(String input) {
  final lexer = Lexer(input);
  final tokens = <Token>[];
  var token = nextToken(lexer);
  while (token.$2.type != TokenType.eof) {
    tokens.add(token.$2);
    token = nextToken(token.$1);
  }
  if (tokens.last != const Token.eof() &&
      tokens.last != const Token.illegal()) {
    tokens.add(const Token.eof());
  }

  return tokens;
}

void main() {
  group('Lexer - base functions', () {
    test(
      'should return a Lexer when instantiated',
      () async {
        // arrange
        const input = 'let five = 5;';

        // act
        final lexer = Lexer(input);

        // assert
        expect(lexer, isA<Lexer>());
        expect(lexer.source, input);
        expect(lexer.position, 0);
        expect(lexer.ch, 'l');
      },
    );
    test(
      'should return an empty lexer when calling eatWhitspace',
      () async {
        // arrange
        const input = 'let five = 5;';
        const inputEx = '$whitespace$input';

        // act
        final lexer = eatWhitespace(Lexer(inputEx));

        // assert
        expect(lexer, isA<Lexer>());
        expect(lexer.source, inputEx);
        expect(lexer.position, 4);
        expect(lexer.ch, 'l');
      },
    );
  });

  group('Lexer samples', () {
    late Logger logger;
    setUp(() {
      logger = Logger();
    });
    test(
      'should appropriate List<Token> when calling =+(){},;',
      () async {
        // arrange
        const input = '=+(){},;';
        const expected = [
          Token.assign(),
          Token.plus(),
          Token.lParen(),
          Token.rParen(),
          Token.lSquirly(),
          Token.rSquirly(),
          Token.comma(),
          Token.semicolon(),
          Token.eof(),
        ];

        // act
        final tokens = lexTester(input);

        // assert
        expect(tokens, isA<List<Token>>());
        expect(tokens, expected);
      },
    );
    test(
      'should appropriate List<Token> when calling complex;',
      () async {
        // arrange
        const input = 'let five = 5;'
            'let ten = 10;'
            'let add = fn(x, y) {'
            'x + y;'
            '};'
            'let result = add(five, ten);'
            '!-/*5;'
            '5 < 10 > 5;'
            'if (5 < 10) {'
            'return true;'
            '} else {'
            'return false;'
            '}'
            '10 == 10;'
            '10 != 9;';
        const expected = [
          Token.let(),
          Token.ident('five'),
          Token.assign(),
          Token.int('5'),
          Token.semicolon(),
          Token.let(),
          Token.ident('ten'),
          Token.assign(),
          Token.int('10'),
          Token.semicolon(),
          Token.let(),
          Token.ident('add'),
          Token.assign(),
          Token.function(),
          Token.lParen(),
          Token.ident('x'),
          Token.comma(),
          Token.ident('y'),
          Token.rParen(),
          Token.lSquirly(),
          Token.ident('x'),
          Token.plus(),
          Token.ident('y'),
          Token.semicolon(),
          Token.rSquirly(),
          Token.semicolon(),
          Token.let(),
          Token.ident('result'),
          Token.assign(),
          Token.ident('add'),
          Token.lParen(),
          Token.ident('five'),
          Token.comma(),
          Token.ident('ten'),
          Token.rParen(),
          Token.semicolon(),
          Token.bang(),
          Token.dash(),
          Token.slash(),
          Token.asterisk(),
          Token.int('5'),
          Token.semicolon(),
          Token.int('5'),
          Token.lessThan(),
          Token.int('10'),
          Token.greaterThan(),
          Token.int('5'),
          Token.semicolon(),
          Token.if_(),
          Token.lParen(),
          Token.int('5'),
          Token.lessThan(),
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
          Token.notEqual(),
          Token.int('9'),
          Token.semicolon(),
          Token.eof(),
        ];

        // act
        final tokens = lexTester(input);

        // assert
        expect(tokens, isA<List<Token>>());
        expect(tokens, expected);
      },
    );
    test(
      'should return illegal token when calling nextToken on ~',
      () async {
        // arrange
        const input = '~';
        // act
        final tokens = lexTester(input);

        // assert
        expect(tokens, isA<List<Token>>());
        expect(tokens, const [Token.illegal()]);
      },
    );
    test(
      'should return a valid lexer using copyWith and position',
      () async {
        // arrange
        const input = 'let five = 5;';
        final lexer = Lexer(input);

        // act
        final lexer2 = lexer.copyWith(position: 4);
        final lexer3 = lexer2.copyWith(source: 'fred is dead');

        // assert
        expect(lexer2, isA<Lexer>());
        expect(lexer2.source, input);
        expect(lexer2.position, 4);
        expect(lexer2.ch, 'f');
        expect(lexer3, isA<Lexer>());
      },
    );
  });
}
