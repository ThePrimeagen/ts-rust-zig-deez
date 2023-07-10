// ignore: lines_longer_than_80_chars
// ignore_for_file: avoid_positional_boolean_parameters, missing_whitespace_between_adjacent_strings

import 'dart:mirrors';

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

Thing testEval(String input) {
  final parser = Parser.fromSource(input);
  final program = parse(parser);
  return eval(program, Environment.create());
}

void testInteger(Thing evaluated, int value) {
  expect(evaluated, isA<Integer>());
  expect((evaluated as Integer).value, value);
}

void testBoolean(Thing evaluated, bool value) {
  expect(evaluated, isA<Boolean>());
  expect((evaluated as Boolean).value, value);
}

void main() {
  group('Eval - Boolean', () {
    late Logger logger;
    late Map<String, bool> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'true': true,
        '!true': false,
        '!!true': true,
        '!5': false,
        '!!5': true,
        'false': false,
        '1 < 2': true,
        '1 > 2': false,
        '1 < 1': false,
        '1 > 1': false,
        '1 == 1': true,
        '1 != 1': false,
        '1 == 2': false,
        '1 != 2': true,
        'true == true': true,
        'false == false': true,
        'true == false': false,
        'true != false': true,
        'false != true': true,
        '(1 < 2) == true': true,
        '(1 < 2) == false': false,
        '(1 > 2) == true': false,
        '(1 > 2) == false': true,
      };
    });
    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testBoolean(evaluated, entry.value);
        }
      },
    );
  });

  group('Eval - Integer', () {
    late Logger logger;
    late Map<String, int> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        '5': 5,
        '10': 10,
        '-5': -5,
        '-10': -10,
        '5 + 5 + 5 + 5 - 10': 10,
        '2 * 2 * 2 * 2 * 2': 32,
        '-50 + 100 + -50': 0,
        '5 * 2 + 10': 20,
        '5 + 2 * 10': 25,
        '20 + 2 * -10': 0,
        '50 / 2 * 2 + 10': 60,
        '2 * (5 + 10)': 30,
        '3 * 3 * 3 + 10': 37,
        '3 * (3 * 3) + 10': 37,
        '(5 + 10 * 2 + 15 / 3) * 2 + -10': 50,
      };
    });

    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testInteger(evaluated, entry.value);
        }
      },
    );
  });

  group('Eval - IfElse', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'if (true) { 10 }': 10,
        'if (false) { 10 }': null,
        'if (1) { 10 }': 10,
        'if (1 < 2) { 10 }': 10,
        'if (1 > 2) { 10 }': null,
        'if (1 > 2) { 10 } else { 20 }': 20,
        'if (1 < 2) { 10 } else { 20 }': 10,
      };
    });

    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          if (entry.value == null) {
            expect(evaluated, const NullThing());
          } else {
            testInteger(evaluated, int.parse(entry.value.toString()));
          }
        }
      },
    );
  });

  group('Eval - Return', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'return 10;': 10,
        'return 10; 9;': 10,
        'return 2 * 5; 9;': 10,
        '9; return 2 * 5; 9;': 10,
      };
    });
    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testInteger(evaluated, int.parse(entry.value.toString()));
        }
      },
    );
  });

  group('Eval - Nested Return', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'if (10 > 1) {'
            'if (10 > 1) {'
            'return 10;'
            '}'
            'return 1;'
            '}': 10,
        'if (10 > 1) {'
            'if (10 > 1) {'
            'return 10;'
            '}'
            'return 1;'
            '} else {'
            'return 0;'
            '}': 10,
      };
    });
    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testInteger(evaluated, int.parse(entry.value.toString()));
        }
      },
    );
  });

  group('Eval - Error handling', () {
    late Logger logger;
    late Map<String, String> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        '5 + true;': 'type mismatch: INTEGER + BOOLEAN',
        '5 + true; 5;': 'type mismatch: INTEGER + BOOLEAN',
        '-true': 'unknown operator: -BOOLEAN',
        'true + false;': 'unknown operator: BOOLEAN + BOOLEAN',
        '5; true + false; 5': 'unknown operator: BOOLEAN + BOOLEAN',
        'if (10 > 1) { true + false; }': 'unknown operator: BOOLEAN + BOOLEAN',
        'if (10 > 1) {'
            'if (10 > 1) {'
            'return true + false;'
            '}'
            'return 1;'
            '}': 'unknown operator: BOOLEAN + BOOLEAN',
      };
    });

    test(
      'should return correct errors for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          expect(evaluated, isA<Error>());
          expect((evaluated as Error).message, entry.value);
        }
      },
    );
  });

  group('Eval - binding & the environment', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'let one = 1; one;': 1,
        'let one = 1; let two = 2; one + two;': 3,
        'let one = 1; let two = one + one; one + two;': 3,
        'let a = 5; let b = a; let c = a + b + 5; c;': 15,
      };
    });

    test(
      'should return the correct value for each input',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testInteger(evaluated, int.parse(entry.value.toString()));
        }
      },
    );

    test(
      'should return identifier not found error',
      () async {
        // arrange
        final input = {'foobar': 'identifier not found: foobar'};

        // act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          expect(evaluated, isA<Error>());
          expect((evaluated as Error).message, entry.value);
        }
      },
    );
  });

  group('Eval - first Function', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = 'fn(x) { x + 2; };';
    });

    test(
      'should return the correct value for each input',
      () async {
        // arrange

        // act
        final evaluated = testEval(input);
        logger.info('$input evaluated to $evaluated');

        // assert
        expect(evaluated, isA<Fun>());
        final fn = evaluated as Fun;
        expect(fn.parameters.length, 1);
        expect(fn.parameters.first.value, 'x');

        expect(fn.body.statements.length, 1);
        expect(fn.body.statements.first, isA<ExpressionStatement>());
        final body = fn.body.statements.first as ExpressionStatement;
        expect(body.expression, isA<InfixExpression>());
        final infix = body.expression as InfixExpression;
        expect(infix.operator, '+');
        expect(infix.left, isA<Identifier>());
        expect((infix.left as Identifier).value, 'x');
        expect(infix.right, isA<IntegerLiteral>());
        expect((infix.right as IntegerLiteral).value, 2);
      },
    );
  });

  group('Eval - Call Functions', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'let identity = fn(x) { x; }; identity(5);': 5,
        'let identity = fn(x) { return x; }; identity(5);': 5,
        'let double = fn(x) { x * 2; }; double(5);': 10,
        'let add = fn(x, y) { x + y; }; add(5, 5);': 10,
        'let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));': 20,
        'fn(x) { x; }(5);': 5,
        'fn(x) { x * 5; }(5);': 25,
      };
    });

    test(
      'should return the correct integer value for each call statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testInteger(evaluated, int.parse(entry.value.toString()));
        }
      },
    );

    test(
      'should return the correct boolean value for each call statement ',
      () async {
        // arrange
        input = {
          'fn(x) { x == 10 }(5)': false,
          'fn(x) { x == 10 }(10)': true,
        };
        // act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          testBoolean(evaluated, bool.parse(entry.value.toString()));
        }
      },
    );
  });

  group('Eval - String literals', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        '"Hello World!"': 'Hello World!',
        '"Hello" + " " + "World!"': 'Hello World!',
      };
    });

    test(
      'should return the correct string value for each eval statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');
          expect(evaluated, isA<Stringy>());
          expect((evaluated as Stringy).value, entry.value);
        }
      },
    );
  });

  group('Eval - BuiltIns len()', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'len("")': 0,
        'len("four")': 4,
        'len("hello world")': 11,
        'len(1)': 'invalid argument type; len accepts strings and arrays',
        'len("one", "two")': 'wrong number of monkeys. got=2, want=1',
      };
    });

    test(
      'should return the correct value for each eval statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');

          if (entry.value is int) {
            testInteger(evaluated, int.parse(entry.value.toString()));
          } else {
            expect(evaluated, isA<Error>());
            expect((evaluated as Error).message, entry.value);
          }
        }
      },
    );
  });

  group('Eval - Array Literals', () {
    late Logger logger;
    late String input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = '[1, 2 * 2, 3 + 3]';
    });

    test(
      'should return the correct value for each array eval statement',
      () async {
        // arrange - act
        final evaluated = testEval(input);
        logger.info('$input evaluated to $evaluated');

        // assert
        expect(evaluated, isA<Array>());
        final array = evaluated as Array;
        expect(array.elements.length, 3);
        testInteger(array.elements[0], 1);
        testInteger(array.elements[1], 4);
        testInteger(array.elements[2], 6);
      },
    );
  });

  group('Eval - Array Index Accessor', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        '[1, 2, 3][0]': 1,
        '[1, 2, 3][1]': 2,
        '[1, 2, 3][2]': 3,
        'let i = 0; [1][i];': 1,
        '[1, 2, 3][1 + 1];': 3,
        'let myArray = [1, 2, 3]; myArray[2];': 3,
        'let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];': 6,
        'let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]': 2,
        '[1, 2, 3][3]': null,
        '[1, 2, 3][-1]': null,
      };
    });

    test(
      'should return the correct value for each array index accessor statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to $evaluated');

          if (entry.value == null) {
            expect(evaluated, isA<NullThing>());
          } else {
            testInteger(evaluated, int.parse(entry.value.toString()));
          }
        }
      },
    );
  });

  group('Eval - Array cdr', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'cdr([1, 2, 3, 4, 5])': [2, 3, 4, 5],
        'cdr([1])': [],
        'cdr([])': const NullThing(),
        'cdr(1)': 'invalid argument type; cdr accepts arrays',
        'cdr([1], [2])': 'wrong number of monkeys. got=2, want=1',
      };
    });

    test(
      'should return the correct value for each array cdr statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to ${evaluated.inspect()}');

          if (entry.value is List) {
            final expected = entry.value as List;
            expect(evaluated, isA<Array>());
            final array = evaluated as Array;
            expect(array.elements.length, expected.length);
            // for (var i = 0; i < expected.length; i++) {
            //   testInteger(array.elements[i], expected[i]);
            // }
          } else if (entry.value is NullThing) {
            expect(evaluated, isA<NullThing>());
          } else {
            expect(evaluated, isA<Error>());
            expect((evaluated as Error).message, entry.value);
          }
        }
      },
    );
  });

  group('Eval - Array push', () {
    late Logger logger;
    late Map<String, dynamic> input;
    setUp(() {
      logger = Logger(level: Level.warning);
      input = {
        'push([1, 2, 3], 4)': [1, 2, 3, 4],
        'push([], 1)': [1],
        'push([1, 2], fn(x){ x * x;})': [1, 2, 'fn(x){ x * x;})'],
        'push([1, 2], "fred")': [1, 2, 'fred'],
        // 'push(1, 1)':
        //     'invalid argument type; push accepts arrays and new element',
        // 'push([1, 2, 3], 1, 2)': 'wrong number of monkeys. got=3, want=2',
      };
    });

    test(
      'should return the correct value for each array push statement',
      () async {
        // arrange - act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          logger.info('$entry evaluated to ${evaluated.inspect()}');

          if (entry.value is List) {
            final expected = entry.value as List;
            expect(evaluated, isA<Array>());
            final array = evaluated as Array;
            expect(array.elements.length, expected.length);
            // for (var i = 0; i < expected.length; i++) {
            //   testInteger(array.elements[i], expected[i]);
            // }
          } else if (entry.value is NullThing) {
            expect(evaluated, isA<NullThing>());
          } else {
            expect(evaluated, isA<Error>());
            expect((evaluated as Error).message, entry.value);
          }
        }
      },
    );
  });

  group('Eval - Hash tables', () {
    test(
      'should return the correct value for each hash table statement',
      () async {
        // arrange
        const input = 'let two = "two";'
            '{'
            '"one": 10 - 9,'
            '"two": 1 + 1,'
            '"thr" + "ee": 6 / 2,'
            '"four": 4 * 1,'
            '"five": 5,'
            '"six": 6'
            '}';

        // act
        final evaluated = testEval(input);

        // assert
        expect(evaluated, isA<Hash>());
        final hash = evaluated as Hash;
        expect(hash.pairs.length, 6);
        final expected = {
          // 99: 'nine',
          'one': 1,
          'two': 2,
          'three': 3,
          'four': 4,
          'five': 5,
          'six': 6,
        };
        for (final key in expected.keys) {
          final value = expected[key];
          final pairValue = hash.pairs.get(Stringy(key));
          expect(pairValue, isNotNull);
          expect(pairValue, isA<Integer>());
          testInteger(pairValue!, value!);
        }
      },
    );
  });

  group('Eval - Quotes', () {
    test(
      'should return the correct value for each quote statement',
      () async {
        // arrange
        const input = {
          'quote(5)': '5',
          'quote(5 + 8)': '(5 + 8)',
          'quote(foobar)': 'foobar',
          'quote(foobar + barfoo)': '(foobar + barfoo)',
        };

        // act - assert
        for (final entry in input.entries) {
          final evaluated = testEval(entry.key);
          expect(evaluated, isA<Quote>());
          final quote = evaluated as Quote;
          expect(quote.node.toString(), entry.value);
        }
      },
    );
  });

  // group('Eval - Unquote', () {
  //   test(
  //     'should return the correct value for each unquote statement',
  //     () async {
  //       // arrange
  //       const input = {
  //         // 'quote(unquote(4))': '4',
  //         // 'quote(unquote(4 + 4))': '8',
  //         'quote(8 + unquote(4 + 4))': '(8 + 8)',
  //       };

  //       // act - assert
  //       for (final entry in input.entries) {
  //         final evaluated = testEval(entry.key);
  //         expect(evaluated, isA<Quote>());
  //         final quote = evaluated as Quote;
  //         expect(quote.node.toString(), entry.value);
  //       }
  //     },
  //   );
  // });

  group('Eval - modify quote/unquote', () {
    test(
      'should return the correct value for each modify quote/unquote statement',
      () async {
        // arrange - act - assert

        Expression one() => IntegerLiteral(1);
        Expression two() => IntegerLiteral(2);

        IntegerLiteral turnOneIntoTwo(Node node) {
          if (node is IntegerLiteral && node.value != 1) {
            return node;
          }

          return IntegerLiteral(2);
        }

        final tests = {
          (first: one(), second: two()),
          (
            first: Program([ExpressionStatement(one())]),
            second: Program([ExpressionStatement(two())])
          ),
        };

        for (final test in tests) {
          final evaluated = modify(test.first, turnOneIntoTwo);
          expect(evaluated, equals(test.second));
        }
      },
    );
  });
}
