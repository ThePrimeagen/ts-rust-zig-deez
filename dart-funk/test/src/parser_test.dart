// ignore_for_file: missing_whitespace_between_adjacent_strings

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

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
  if (exp is! BooleanLiteral) {
    throw Exception('exp not Boolean. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
      'exp.value not $expected. got=${exp.value} ${exp.runtimeType}',
    );
  }
  if (exp.tokenLiteral() != '$expected') {
    throw Exception(
      'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}',
    );
  }
  return true;
}

bool testIdentifier(Expression exp, String expected) {
  if (exp is! Identifier) {
    throw Exception('exp not Identifier. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
      'exp.value not $expected. got=${exp.value} ${exp.runtimeType}',
    );
  }
  if (exp.tokenLiteral() != expected) {
    throw Exception(
      'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}',
    );
  }
  return true;
}

bool testIntegerLiteral(Expression exp, int expected) {
  if (exp is! IntegerLiteral) {
    throw Exception('exp not IntegerLiteral. got=${exp.runtimeType}');
  }
  if (exp.value != expected) {
    throw Exception(
      'exp.value not $expected. got=${exp.value} ${exp.runtimeType}',
    );
  }
  // if (exp.tokenLiteral() != '$expected') {
  //   throw Exception(
  //     'exp.tokenLiteral not $expected. got=${exp.tokenLiteral()}',
  //   );
  // }
  return true;
}

bool testInfixExpression(
  Expression expression,
  int left,
  String operator,
  int right,
) {
  if (expression is! InfixExpression) {
    throw Exception('expression is not InfixExpression. got=$expression');
  }
  if (expression.operator != operator) {
    throw Exception(
      'expression.operator is not $operator. got=${expression.operator}',
    );
  }
  if (!testLiteralExpression(expression.left, left)) {
    throw Exception(
      'expression.left is not $left. got=${expression.left}',
    );
  }
  if (!testLiteralExpression(expression.right, right)) {
    throw Exception(
      'expression.right is not $right. got=${expression.right}',
    );
  }
  return true;
}

void main() {
  group('Parser - expectPeek', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'let five = 5;';
    });

    test(
      'should return an advanced Parser when expectPeek is call on init state',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // act
        final (actual, ok) = expectPeek(parser, TokenType.ident);

        // assert
        expect(actual, isA<Parser>());
        expect(ok, isTrue);
        expect(actual.currToken, isA<Token>());
        expect(actual.currToken, equals(const Token.ident('five')));
      },
    );

    test(
      'should return same parser with error when expectPeek is call on '
      'initial state with tes being assign',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // act
        final (actual, ok) = expectPeek(parser, TokenType.assign);

        // assert
        expect(actual, isA<Parser>());
        expect(ok, isFalse);
        expect(actual.currToken, isA<Token>());
        expect(actual.currToken, equals(const Token.let()));
        expect(actual.errors.length, equals(1));
        expect(actual.errors[0].message,
            equals('Expected next token to be assign, got ident instead'));
      },
    );

    test(
      'should return same parser with error when expectPeek is call on '
      'last token',
      () async {
        // arrange
        const input = ';';
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // act
        final (actual, ok) = expectPeek(parser, TokenType.ident);

        // assert
        expect(actual, isA<Parser>());
        expect(ok, isFalse);
        expect(actual.currToken, isA<Token>());
        expect(actual.currToken, equals(const Token.semicolon()));
        expect(actual.errors.length, equals(1));
        expect(
          actual.errors[0].message,
          equals('No peek token available'),
        );
      },
    );

    test(
      'should return same parser with 2 errors when expectPeek is call on '
      'last token twice',
      () async {
        // arrange
        const input = ';';
        final parser = Parser.fromSource(input);
        logger.detail('parser: $parser');

        // act
        final (actual, _) = expectPeek(parser, TokenType.ident);
        final (actual2, ok2) = expectPeek(actual, TokenType.ident);

        // assert
        expect(actual2, isA<Parser>());
        expect(ok2, isFalse);
        expect(actual2.currToken, isA<Token>());
        expect(actual2.currToken, equals(const Token.semicolon()));
        expect(actual2.errors.length, equals(2));
        expect(
          actual2.errors[0].message,
          equals('No peek token available'),
        );
        expect(
          actual2.errors[1].message,
          equals('No peek token available'),
        );
      },
    );
  });

  group('Parser - constructors', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
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
      logger = Logger(level: Level.warning);
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
      logger = Logger(level: Level.warning);
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
      logger = Logger(level: Level.warning);
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
        logger.info(program.toString());

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
      'should return ParserException when missing ident token',
      () async {
        // arrange
        const input = 'let = 10;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(0));
        expect(program.errors.length, equals(1));
        expect(
          program.errors[0].message,
          equals('Expected token to be ident, '
              'got assign instead'),
        );
      },
    );

    test(
      'should return ParserException when missing ident token => int',
      () async {
        // arrange
        const input = 'let 838383;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(0));
        expect(program.errors.length, equals(1));
        expect(
          program.errors[0].message,
          equals('Expected token to be ident, '
              'got int instead'),
        );
      },
    );

    test(
      'should return ParserException when missing assign token',
      () async {
        // arrange
        const input = 'let x 5;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(0));
        expect(program.errors.length, equals(1));
        expect(
          program.errors[0].message,
          equals('Expected next token to be assign, '
              'got int instead'),
        );
      },
    );

    test(
      'should return LetStatements with correct values',
      () async {
        // arrange
        final things = [
          (input: 'let x = 5;', id: 'x', val: 5),
          (input: 'let y = true;', id: 'y', val: true),
          (input: 'let foobar = y;', id: 'foobar', val: 'y'),
        ];

        // act - assert
        for (final thing in things) {
          final parser = Parser.fromSource(thing.input);
          final program = parse(parser);
          logger.info(program.toString());

          expect(program, isA<Program>());
          expect(program.statements.length, equals(1));
          expect(program.statements[0], isA<LetStatement>());
          expect(
            (program.statements[0] as LetStatement).name,
            isA<Identifier>(),
          );
          expect(
            (program.statements[0] as LetStatement).name.value,
            equals(thing.id),
          );
          expect(
            (program.statements[0] as LetStatement).value,
            isA<Expression>(),
          );
          final value = (program.statements[0] as LetStatement).value;
          switch (value) {
            case _ when value is IntegerLiteral:
              expect(
                ((program.statements[0] as LetStatement).value
                        as IntegerLiteral)
                    .value,
                equals(thing.val),
              );
            case _ when value is BooleanLiteral:
              expect(
                ((program.statements[0] as LetStatement).value
                        as BooleanLiteral)
                    .value,
                equals(thing.val),
              );
            case _ when value is Identifier:
              expect(
                ((program.statements[0] as LetStatement).value as Identifier)
                    .value,
                equals(thing.val),
              );
            case _:
              logger.alert('Unexpected value type: ${value.runtimeType}');
          }
        }
      },
    );

    test(
      'should return a valid let statement with complex grouping',
      () async {
        // arrange
        const input = 'let x = 1 * 2 * 3 * 4 * 5';
        const expected = 'let x = ((((1 * 2) * 3) * 4) * 5);';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<LetStatement>());
        expect(
          (program.statements[0] as LetStatement).toString(),
          equals(expected),
        );
      },
    );

    test(
      'should return an error when using invalid Let input',
      () async {
        // arrange
        const input = 'let x 12 * 3;';
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.errors.length, equals(1));
      },
    );
  });

  group('Parser - ReturnStatement', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'return 5;'
          'return 10;'
          // 'return add(15);'
          'return 993322;';
    });
    test(
      'should return a program with 4 ReturnStatements',
      () async {
        // arrange
        final parser = Parser.fromSource(input);

        // act
        final program = parse(parser);
        logger.info(program.toString());

        // assert
        expect(program, isA<Program>());
        expect(program.statements.length, equals(3));
        expect(program.errors.isEmpty, isTrue);
        expect(program.statements[0], isA<ReturnStatement>());
      },
    );
  });

  group('Parser - Identifier Expression', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
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
      logger = Logger(level: Level.warning);
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
      logger = Logger(level: Level.warning);
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

    test(
      'should return a Infix Expression with precedence',
      () async {
        // arrange
        input = [
          (input: 'true == true', lrand: 'true', op: '==', rrand: 'true'),
          (input: 'true != false', lrand: 'true', op: '!=', rrand: 'false'),
          (input: 'false == false', lrand: 'false', op: '==', rrand: 'false'),
        ];

        // act - assert
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
            isA<BooleanLiteral>(),
          );
        }
      },
    );
  });

  group('Parser - order of precedence', () {
    late Logger logger;
    late Map<String, String> input;
    setUp(() {
      logger = Logger(level: Level.warning);
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

        // '3 + 4; -5 * 5': '(3 + 4)((-5) * 5)',
        '5 > 4 == 3 < 4': '((5 > 4) == (3 < 4))',
        '5 < 4 != 3 > 4': '((5 < 4) != (3 > 4))',
        '3 + 4 * 5 == 3 * 1 + 4 * 5': '((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))',
        'a + add(b * c) + d': '((a + add((b * c))) + d)',
        // Nested & Array Indecies
        'add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));':
            'add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))',
        'add(a + b + c * d / f + g)': 'add((((a + b) + ((c * d) / f)) + g))',
        'a * [1, 2, 3, 4][b * c] * d;': '((a * ([1, 2, 3, 4][(b * c)])) * d)',
        'add(a * b[2], b[1], 2 * [1, 2][1])':
            'add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))',
      };
    });
    test(
      'should return a Program with correct order of precedence',
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

  group('Parser - If statements', () {
    late Logger logger;
    late String input;
    late String inputElse;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'if (x < y) { x };';
      inputElse = 'if (x < y) { xone } else { yone };';
    });
    test(
      'should return a Program with an IfStatement',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.info('Tokens: ${parser.tokens}');

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<IfExpression>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .condition,
          isA<InfixExpression>(),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .left,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .condition as InfixExpression)
                  .left as Identifier)
              .value,
          equals('x'),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .operator,
          equals('<'),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .right,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .condition as InfixExpression)
                  .right as Identifier)
              .value,
          equals('y'),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .consequence,
          isA<BlockStatement>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .consequence
              .statements[0],
          isA<ExpressionStatement>(),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .consequence
                  .statements[0] as ExpressionStatement)
              .expression,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .consequence
                      .statements[0] as ExpressionStatement)
                  .expression as Identifier)
              .value,
          equals('x'),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .alternative,
          isNull,
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .alternative,
          isNull,
        );
      },
    );
    test(
      'should return a Program with an IfStatement and Else',
      () async {
        // arrange
        final parser = Parser.fromSource(inputElse);
        logger.detail('Tokens: ${parser.tokens.join('\n\t')}');

        // act
        final program = parse(parser);
        logger.detail('Program: $program');

        // assert
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<IfExpression>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .condition,
          isA<InfixExpression>(),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .left,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .condition as InfixExpression)
                  .left as Identifier)
              .value,
          equals('x'),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .operator,
          equals('<'),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .condition as InfixExpression)
              .right,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .condition as InfixExpression)
                  .right as Identifier)
              .value,
          equals('y'),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .consequence,
          isA<BlockStatement>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .consequence
              .statements[0],
          isA<ExpressionStatement>(),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as IfExpression)
                  .consequence
                  .statements[0] as ExpressionStatement)
              .expression,
          isA<Identifier>(),
        );
        expect(
          ((((program.statements[0] as ExpressionStatement).expression
                          as IfExpression)
                      .consequence
                      .statements[0] as ExpressionStatement)
                  .expression as Identifier)
              .value,
          equals('xone'),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as IfExpression)
              .alternative,
          isA<BlockStatement>(),
        );

        expect(
          // ignore: cast_nullable_to_non_nullable
          (((((program.statements[0] as ExpressionStatement).expression
                              as IfExpression)
                          .alternative as BlockStatement)
                      .statements[0] as ExpressionStatement)
                  .expression as Identifier)
              .value,
          equals('yone'),
        );
      },
    );
  });

  group('Parser - def function', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'fn(x, y) { x + y; }';
    });
    // test(
    //   'should return a Program with a FunctionLiteral',
    //   () async {
    //     // arrange
    //     final parser = Parser.fromSource(input);
    //     logger.info('Tokens: ${parser.tokens}');

    //     // act
    //     final program = parse(parser);
    //     logger.info('Program: $program');

    //     // assert
    //     expect(program.statements.length, equals(1));
    //     expect(program.statements[0], isA<ExpressionStatement>());
    //     expect(
    //       (program.statements[0] as ExpressionStatement).expression,
    //       isA<FunctionLiteral>(),
    //     );
    //     expect(
    //       ((program.statements[0] as ExpressionStatement).expression
    //               as FunctionLiteral)
    //           .parameters
    //           .length,
    //       equals(2),
    //     );
    //     expect(
    //       ((program.statements[0] as ExpressionStatement).expression
    //               as FunctionLiteral)
    //           .parameters[0]
    //           .value,
    //       equals('x'),
    //     );
    //     expect(
    //       ((program.statements[0] as ExpressionStatement).expression
    //               as FunctionLiteral)
    //           .parameters[1]
    //           .value,
    //       equals('y'),
    //     );
    //     expect(
    //       ((program.statements[0] as ExpressionStatement).expression
    //               as FunctionLiteral)
    //           .body,
    //       isA<BlockStatement>(),
    //     );
    //     expect(
    //       ((program.statements[0] as ExpressionStatement).expression
    //               as FunctionLiteral)
    //           .body
    //           .statements
    //           .length,
    //       equals(1),
    //     );
    //     expect(
    //       (((program.statements[0] as ExpressionStatement).expression
    //                   as FunctionLiteral)
    //               .body
    //               .statements[0] as ExpressionStatement)
    //           .expression,
    //       isA<InfixExpression>(),
    //     );
    //     expect(
    //       ((((program.statements[0] as ExpressionStatement).expression
    //                       as FunctionLiteral)
    //                   .body
    //                   .statements[0] as ExpressionStatement)
    //               .expression as InfixExpression)
    //           .left,
    //       isA<Identifier>(),
    //     );
    //     expect(
    //       (((((program.statements[0] as ExpressionStatement).expression
    //                           as FunctionLiteral)
    //                       .body
    //                       .statements[0] as ExpressionStatement)
    //                   .expression as InfixExpression)
    //               .left as Identifier)
    //           .value,
    //       equals('x'),
    //     );
    //     expect(
    //       ((((program.statements[0] as ExpressionStatement).expression
    //                       as FunctionLiteral)
    //                   .body
    //                   .statements[0] as ExpressionStatement)
    //               .expression as InfixExpression)
    //           .operator,
    //       equals('+'),
    //     );
    //     expect(
    //       ((((program.statements[0] as ExpressionStatement).expression
    //                       as FunctionLiteral)
    //                   .body
    //                   .statements[0] as ExpressionStatement)
    //               .expression as InfixExpression)
    //           .right,
    //       isA<Identifier>(),
    //     );
    //     expect(
    //       (((((program.statements[0] as ExpressionStatement).expression
    //                           as FunctionLiteral)
    //                       .body
    //                       .statements[0] as ExpressionStatement)
    //                   .expression as InfixExpression)
    //               .right as Identifier)
    //           .value,
    //       equals('y'),
    //     );
    //   },
    // );

    test(
      'should return correct parameters',
      () async {
        // arrange
        final inputMap = {
          'fn() {}': <String>[],
          'fn(x) {}': ['x'],
          'fn(x, y, z) {}': ['x', 'y', 'z'],
        };

        // act - assert
        for (final item in inputMap.entries) {
          final parser = Parser.fromSource(item.key);
          final program = parse(parser);
          logger.info('$program');
          final function = (program.statements[0] as ExpressionStatement)
              .expression as FunctionLiteral;
          expect(function.parameters.length, equals(item.value.length));
          for (var i = 0; i < item.value.length; i++) {
            expect(function.parameters[i].value, equals(item.value[i]));
          }
        }
      },
    );
  });

  group('Parser - call expressions', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'add(1, 2 * 3, 4 + 5);';
    });
    test(
      'should return a CallExpression',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.info('Tokens: ${parser.tokens}');

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        final args = ((program.statements[0] as ExpressionStatement).expression
                as CallExpression)
            .arguments;

        // assert
        testLiteralExpression(args[0], 1);
        testInfixExpression(args[1], 2, '*', 3);
        testInfixExpression(args[2], 4, '+', 5);

        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<CallExpression>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as CallExpression)
              .function,
          isA<Identifier>(),
        );
        expect(
          (((program.statements[0] as ExpressionStatement).expression
                      as CallExpression)
                  .function as Identifier)
              .value,
          equals('add'),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as CallExpression)
              .arguments
              .length,
          equals(3),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as CallExpression)
              .arguments[0],
          isA<IntegerLiteral>(),
        );
      },
    );

    test(
      'should return a StringExpression for hello world',
      () async {
        // arrange
        input = '"hello world";';

        // act
        final parser = Parser.fromSource(input);
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program.statements.length, equals(1));
        expect(program.statements[0], isA<ExpressionStatement>());
        expect(
          (program.statements[0] as ExpressionStatement).expression,
          isA<StringLiteral>(),
        );
        expect(
          ((program.statements[0] as ExpressionStatement).expression
                  as StringLiteral)
              .value,
          equals('hello world'),
        );
      },
    );
    test(
      'should return a list of arguments',
      () async {
        // arrange
        final inputMap = {
          '();': <Expression>[],
          '(1);': [1],
          '(1, 2 * 3, 4 + 5);': [1, (2 * 3), (4 + 5)],
        };

        // act
        for (final item in inputMap.entries) {
          final parser = Parser.fromSource(item.key);
          final args = parseCallArguments(parser, []);
          logger.info('args: $args');
        }

        // assert
      },
    );
  });

  group('Parser - Array Literal', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = '[1, 2 * 2, 3 + 3]';
    });

    test(
      'should return an array literal',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.info('Tokens: ${parser.tokens}');

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program.statements.length, equals(1));
        final stmt = program.statements[0] as ExpressionStatement;
        expect(stmt, isA<ExpressionStatement>());
        expect(stmt.expression, isA<ArrayLiteral>());
        final array = stmt.expression as ArrayLiteral;
        expect(array.elements.length, equals(3));
        testLiteralExpression(array.elements[0], 1);
        testInfixExpression(array.elements[1], 2, '*', 2);
        testInfixExpression(array.elements[2], 3, '+', 3);
      },
    );
  });

  group('Parser - Index Expressions', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'myArray[1 + 1]';
    });

    test(
      'should return an index expression',
      () async {
        // arrange
        final parser = Parser.fromSource(input);
        logger.info('Tokens: ${parser.tokens}');

        // act
        final program = parse(parser);
        logger.info('Program: $program');

        // assert
        expect(program.statements.length, equals(1));
        final stmt = program.statements[0] as ExpressionStatement;
        expect(stmt, isA<ExpressionStatement>());
        expect(stmt.expression, isA<IndexExpression>());
        final index = stmt.expression as IndexExpression;
        expect(index.left, isA<Identifier>());
        expect((index.left as Identifier).value, equals('myArray'));
        expect(index.index, isA<InfixExpression>());
        expect((index.index as InfixExpression).left, isA<IntegerLiteral>());
        expect((index.index as InfixExpression).operator, equals('+'));
        expect((index.index as InfixExpression).right, isA<IntegerLiteral>());
      },
    );
  });
}
