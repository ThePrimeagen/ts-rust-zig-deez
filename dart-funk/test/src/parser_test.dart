// ignore_for_file: missing_whitespace_between_adjacent_strings

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

void main() {
  group('Parser - constructors', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'let five = 5;';
    });
    test(
      'should return a Parser when instantiated with tokens',
      () async {
        // arrange
        final tokens = tokenGenerator(input);

        // act
        final parser = Parser(tokens);
        logger.detail('parser: $parser');

        // assert
        expect(parser, isA<Parser>());
        expect(parser.currToken, isA<Token>());
        expect(parser.currToken, equals(const Token.let()));
        expect(parser.peekToken, isA<Token>());
        expect(parser.peekToken, equals(const Token.ident('five')));
      },
    );
    test(
      'should return a Parser when instantiated with a Lexer',
      () async {
        // arrange
        final lexer = Lexer(input);

        // act
        final parser = Parser.fromLexer(lexer);
        logger.detail('parser: $parser');

        // assert
        expect(parser, isA<Parser>());
        expect(parser.currToken, isA<Token>());
        expect(parser.currToken, equals(const Token.let()));
        expect(parser.peekToken, isA<Token>());
        expect(parser.peekToken, equals(const Token.ident('five')));
      },
    );
    test(
      'should return a Parser when instantiated with a source string',
      () async {
        // arrange
        final source = input;

        // act
        final parser = Parser.fromSource(source);
        logger.detail('parser: $parser');

        // assert
        expect(parser, isA<Parser>());
        expect(parser.currToken, isA<Token>());
        expect(parser.currToken, equals(const Token.let()));
        expect(parser.peekToken, isA<Token>());
        expect(parser.peekToken, equals(const Token.ident('five')));
      },
    );
    test(
      'should return a Parser w EOF PeekToken when '
      'instantiated with an 1 token source string',
      () async {
        // arrange
        input = ';';

        // act
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // assert
        expect(parser.peekToken, equals(const Token.eof()));
      },
    );
    test(
      'should return a Parser w EOF CurrToken and '
      'illegal peekToken instantiated with an empty source string',
      () async {
        // arrange
        input = '';

        // act
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // assert
        expect(parser.currToken, equals(const Token.eof()));
        expect(parser.peekToken, equals(const Token.illegal()));
      },
    );
  });

  group('Parser - Copy', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'let five = 5;';
    });
    test(
      'should return a Parser when calling copyWith',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // act
        final copy = parser.copyWith();

        // assert
        expect(copy, isA<Parser>());
        expect(copy.currToken, parser.currToken);
        expect(copy.peekToken, parser.peekToken);
        expect(copy.errors, parser.errors);
        expect(copy.tokenIndex, parser.tokenIndex);
      },
    );
    test(
      'should return a NullParser when calling copyWith on NullParser ',
      () async {
        // arrange
        const parser = NullParser();

        // act
        final copy = parser.copyWith();

        // assert
        expect(copy, isA<Parser>());
        expect(copy.currToken, parser.currToken);
        expect(copy.peekToken, parser.peekToken);
        expect(copy.errors, parser.errors);
        expect(copy.tokenIndex, parser.tokenIndex);
      },
    );
  });

  group('Parser - advance', () {
    late Logger logger;
    late String input;
    late Parser parser;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'let five = 5;';
      parser = Parser.fromSource(input);
    });
    test(
      'should return a Parser when calling advanceParser',
      () async {
        // arrange
        logger.detail('parser: $parser');

        // act
        final advParser = advanceParser(parser);
        logger.detail('new Parser: $advParser');

        // assert
        expect(advParser, isA<Parser>());
        expect(advParser.currToken, isA<Token>());
        expect(advParser.currToken, equals(const Token.ident('five')));
        expect(advParser.peekToken, equals(const Token.assign()));
      },
    );
    test(
      'should return a ParserException when calling '
      'advanceParser on last Token',
      () async {
        // arrange
        logger.detail('parser: $parser');
        while (parser.currToken != const Token.eof()) {
          parser = advanceParser(parser);
        }
        final advParser = advanceParser(parser);

        logger.detail('advParser: $advParser');

        // assert
        expect(advParser, isA<Parser>());
        expect(advParser.errors.length, equals(1));
        expect(advParser.errors[0].message, equals('Unexpected end of input'));
      },
    );
  });

  group('Parser - LetStatement', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'let five = 5;'
          'let ten = 10;'
          'let result = add;';
    });

    test(
      'should return a Program with 3 Let Statements',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.info('Tokens: ${parser.tokens}');

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(3));
        expect(program.statements[0], isA<LetStatement>());
        expect(program.statements[1], isA<LetStatement>());
        expect(program.statements[2], isA<LetStatement>());
        expect(program.errors.length, equals(0));
      },
    );
    test(
      'should return ParserException when using invalid Let input',
      () async {
        // arrange
        const input = 'let x 5;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(0));
        expect(program.errors.length, equals(1));
        expect(
          program.errors[0].message,
          equals('Expected next token to be TokenType.assign, '
              'got TokenType.int instead'),
        );
      },
    );
  });

  group('Parser - ReturnStatement', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'return 5;'
          'return 10;'
          'return 993322;';
    });
    test(
      'should return a program with 3 ReturnStatements',
      () async {
        // arrange
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(3));
        expect(program.statements[0], isA<ReturnStatement>());
      },
    );
  });

  group('Parser - Identifier Expression', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = 'foobar;';
    });
    test(
      'should return an Identifier Expression',
      () async {
        // arrange
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<Identifier>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as Identifier)
              .value,
          equals('foobar'),
        );
      },
    );
    test(
      'should return a IntegerLiteral Expression',
      () async {
        // arrange
        const input = '5;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<IntegerLiteral>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IntegerLiteral)
              .value,
          equals(5),
        );
      },
    );
  });

  group('Parser - Prefix tests', () {
    late Logger logger;
    late List<Record> input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = [
        (input: '-5;', rator: '-', rand: '5'),
        (input: '!5;', rator: '!', rand: '5'),
        (input: '-15;', rator: '-', rand: '15'),
        // (input: '!true;', rator: '!', rand: 'true'),
        // (input: '!false;', rator: '!', rand: 'false')
      ];
    });
    test(
      'should return a Prefix Expression',
      () async {
        // arrange - act - assert
        for (final (element as ({String input, String rator, String rand}))
            in input) {
          final parser = Parser.fromSource(element.input);
          final program = parse(parser);
          logger.info('Program: $program');

          expect(program, isA<Program>());
          expect(program.statements.length, equals(1));
          expect(program.statements[0], isA<ExpressionStatement>());
          expect(
            (program.statements[0] as ExpressionStatement).expression,
            isA<PrefixExpression>(),
          );
          expect(
            ((program.statements[0] as ExpressionStatement).expression
                    as PrefixExpression)
                .operator,
            equals(element.rator),
          );
          expect(
            ((program.statements[0] as ExpressionStatement).expression
                    as PrefixExpression)
                .right,
            isA<IntegerLiteral>(),
          );
          expect(
            (((program.statements[0] as ExpressionStatement).expression
                        as PrefixExpression)
                    .right as IntegerLiteral)
                .value,
            equals(int.parse(element.rand)),
          );
        }
      },
    );
  });

  group('Parser - Infix tests', () {
    late Logger logger;
    late List<Record> input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = [
        (input: '5 + 5;', lrand: '5', op: '+', rrand: '5'),
        (input: '5 - 5;', lrand: '5', op: '-', rrand: '5'),
        (input: '5 * 5;', lrand: '5', op: '*', rrand: '5'),
        (input: '5 / 5;', lrand: '5', op: '/', rrand: '5'),
        (input: '5 > 5;', lrand: '5', op: '>', rrand: '5'),
        (input: '5 < 5;', lrand: '5', op: '<', rrand: '5'),
        (input: '5 == 5;', lrand: '5', op: '==', rrand: '5'),
        (input: '5 != 5;', lrand: '5', op: '!=', rrand: '5'),
        // (input: 'true == true', lrand: 'true', op: '==', rrand: 'true'),
        // (input: 'true != false', lrand: 'true', op: '!=', rrand: 'false'),
        // (input: 'false == false', lrand: 'false', op: '==', rrand: 'false'),
      ];
    });
    test(
      'should return a Infix Expression',
      () async {
        for (final (element as ({
              String input,
              String lrand,
              String op,
              String rrand
            })) in input) {
          final parser = Parser.fromSource(element.input);
          final program = parse(parser);
          logger.info('Program: $program');

          expect(program, isA<Program>());
          expect(program.statements.length, equals(1));
          expect(program.statements[0], isA<ExpressionStatement>());
          expect(
            (program.statements[0] as ExpressionStatement).expression,
            isA<InfixExpression>(),
          );
          expect(
            ((program.statements[0] as ExpressionStatement).expression
                    as InfixExpression)
                .operator,
            equals(element.op),
          );
          expect(
            ((program.statements[0] as ExpressionStatement).expression
                    as InfixExpression)
                .left,
            isA<IntegerLiteral>(),
          );
          expect(
            (((program.statements[0] as ExpressionStatement).expression
                        as InfixExpression)
                    .left as IntegerLiteral)
                .value,
            equals(int.parse(element.lrand)),
          );
          expect(
            ((program.statements[0] as ExpressionStatement).expression
                    as InfixExpression)
                .right,
            isA<IntegerLiteral>(),
          );
          expect(
            (((program.statements[0] as ExpressionStatement).expression
                        as InfixExpression)
                    .right as IntegerLiteral)
                .value,
            equals(int.parse(element.rrand)),
          );
        }
      },
    );
  });

  group('Parser - order of precedence', () {
    late Logger logger;
    late Map<String, String> input;
    setUp(() {
      logger = Logger(level: Level.debug);
      input = {
        '!true': '(!true)',
        '1 + 2 + 3;': '((1 + 2) + 3)',
        '-a * b;': '((-a) * b)',
        '!-a': '(!(-a))',
        'a + b + c': '((a + b) + c)',
        'a + b - c': '((a + b) - c)',
        'a * b * c': '((a * b) * c)',
        'a * b / c': '((a * b) / c)',
        'a + b / c': '(a + (b / c))',
        'a + b * c + d / e - f': '(((a + (b * c)) + (d / e)) - f)',
        // '3 + 4; -5 * 5': '(3 + 4)((-5) * 5)',
        // '5 > 4 == 3 < 4': '((5 > 4) == (3 < 4))',
        // '5 < 4 != 3 > 4': '((5 < 4) != (3 > 4))',
        // '3 + 4 * 5 == 3 * 1 + 4 * 5':
        // '((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))',
        'true': 'true',
        'false': 'false',
        '3 > 5 == false': '((3 > 5) == false)',
        '3 < 5 == true': '((3 < 5) == true)',
        'true == true': '(true == true)',
        '1 + (2 + 3) + 4': '((1 + (2 + 3)) + 4)',
        '(5 + 5) * 2': '((5 + 5) * 2)',
        '2 / (5 + 5)': '(2 / (5 + 5))',
        '-(5 + 5)': '(-(5 + 5))',
        '!(true == true)': '(!(true == true))',
        // 'a + add(b * c) + d': '((a + add((b * c))) + d)',
        // 'add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))':
        //     'add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))',
        // 'add(a + b + c * d / f + g)': 'add((((a + b) + ((c * d) / f)) + g))',
        // 'a * [1, 2, 3, 4][b * c] * d': '((a * ([1, 2, 3, 4][(b * c)])) * d)',
        // 'add(a * b[2], b[1], 2 * [1, 2][1])':
        //     'add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))',
      };
    });
    test(
      'should return a Program with crrect order of precedence',
      () async {
        // arrange - act - assert
        for (final item in input.entries) {
          final parser = Parser.fromSource(item.key);
          final program = parse(parser);
          logger.info('Program: $program');

          expect(program, isA<Program>());
          expect(program.statements.length, equals(1));
          expect(program.statements[0], isA<ExpressionStatement>());
          expect(
            program.statements[0].toString(),
            equals(item.value),
          );
        }
      },
    );
  });
}

bool testLiteralExpression(Expression exp, dynamic expected) {
  if (expected is int) {
    return testIntegerLiteral(exp, expected);
  } else if (expected is String) {
    return testIdentifier(exp, expected);
  } else if (expected is bool) {
    return testBooleanLiteral(exp, expected);
  }
  throw Exception('type of exp not handled. got=${exp.runtimeType}');
}

// ignore: avoid_positional_boolean_parameters
bool testBooleanLiteral(Expression exp, bool expected) {
  if (exp is! Boolean) {
    throw Exception('exp not Boolean. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
        'exp.value not $expected. got=${exp.value} ${exp.runtimeType}');
  }
  if (exp.tokenLiteral() != '$expected') {
    throw Exception(
        'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}');
  }
  return true;
}

bool testIdentifier(Expression exp, String expected) {
  if (exp is! Identifier) {
    throw Exception('exp not Identifier. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
        'exp.value not $expected. got=${exp.value} ${exp.runtimeType}');
  }
  if (exp.tokenLiteral() != expected) {
    throw Exception(
        'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}');
  }
  return true;
}

bool testIntegerLiteral(Expression exp, int expected) {
  if (exp is! IntegerLiteral) {
    throw Exception('exp not IntegerLiteral. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
        'exp.value not $expected. got=${exp.value} ${exp.runtimeType}');
  }
  if (exp.tokenLiteral() != '$expected') {
    throw Exception(
        'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}');
  }
  return true;
}
