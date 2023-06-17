// ignore_for_file: avoid_positional_boolean_parameters, missing_whitespace_between_adjacent_strings

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

Thing testEval(String input) {
  final parser = Parser.fromSource(input);
  final program = parse(parser);
  return eval(program);
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
      logger = Logger(level: Level.debug);
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
      logger = Logger(level: Level.debug);
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
      logger = Logger(level: Level.debug);
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
      logger = Logger(level: Level.debug);
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
      logger = Logger(level: Level.debug);
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
}
