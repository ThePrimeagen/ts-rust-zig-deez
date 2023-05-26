/*
 * Project: ts-rust-zig-deez
 * Created Date: Thursday May 25th 2023 10:24:39 am
 * Author: Fa C Shus (paul@facshus.com)
 * -----
 * Last Modified: Thursday, 25th May 2023 10:24:40 am
 * Modified By: Fa C Shus (paul@facshus.com)
 * -----
 * Copyright (c) 2021 - 2023 FaCShus Systems
 * License: MIT
 */

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

void main() {
  group('lexer', () {
    late Logger logger;
    late Map<String, List<Token>> tests;

    setUp(() {
      logger = Logger();
      tests = {
        '=+(){},;': [
          const Token.assign(),
          const Token.plus(),
          const Token.lParen(),
          const Token.rParen(),
          const Token.lSquirly(),
          const Token.rSquirly(),
          const Token.comma(),
          const Token.semicolon()
        ],
        '''
let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
   let result = add(five, ten);''': [
          Token.let(),
          const Token.ident('five'),
          const Token.assign(),
          const Token.int('5'),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('ten'),
          const Token.assign(),
          const Token.int('10'),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('add'),
          const Token.assign(),
          Token.function(),
          const Token.lParen(),
          const Token.ident('x'),
          const Token.comma(),
          const Token.ident('y'),
          const Token.rParen(),
          const Token.lSquirly(),
          const Token.ident('x'),
          const Token.plus(),
          const Token.ident('y'),
          const Token.semicolon(),
          const Token.rSquirly(),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('result'),
          const Token.assign(),
          const Token.ident('add'),
          const Token.lParen(),
          const Token.ident('five'),
          const Token.comma(),
          const Token.ident('ten'),
          const Token.rParen(),
          const Token.semicolon(),
          Token.eof(),
        ],
        '''
let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
   let result = add(five, ten);
   !-/*5;
   5 < 10 > 5;
   
   if (5 < 10) {
       return true;
   } else {
       return false;
}

10 == 10; 
10 != 9;
''': [
          Token.let(),
          const Token.ident('five'),
          const Token.assign(),
          const Token.int('5'),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('ten'),
          const Token.assign(),
          const Token.int('10'),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('add'),
          const Token.assign(),
          Token.function(),
          const Token.lParen(),
          const Token.ident('x'),
          const Token.comma(),
          const Token.ident('y'),
          const Token.rParen(),
          const Token.lSquirly(),
          const Token.ident('x'),
          const Token.plus(),
          const Token.ident('y'),
          const Token.semicolon(),
          const Token.rSquirly(),
          const Token.semicolon(),
          Token.let(),
          const Token.ident('result'),
          const Token.assign(),
          const Token.ident('add'),
          const Token.lParen(),
          const Token.ident('five'),
          const Token.comma(),
          const Token.ident('ten'),
          const Token.rParen(),
          const Token.semicolon(),
          const Token.bang(),
          const Token.minus(),
          const Token.slash(),
          const Token.asterisk(),
          const Token.int('5'),
          const Token.semicolon(),
          const Token.int('5'),
          const Token.lessThan(),
          const Token.int('10'),
          const Token.greaterThan(),
          const Token.int('5'),
          const Token.semicolon(),
          Token.if_(),
          const Token.lParen(),
          const Token.int('5'),
          const Token.lessThan(),
          const Token.int('10'),
          const Token.rParen(),
          const Token.lSquirly(),
          Token.return_(),
          Token.true_(),
          const Token.semicolon(),
          const Token.rSquirly(),
          Token.else_(),
          const Token.lSquirly(),
          Token.return_(),
          Token.false_(),
          const Token.semicolon(),
          const Token.rSquirly(),
          // 10 == 10; 
          const Token.int('10'),
          const Token.equal(),
          const Token.int('10'),
          const Token.semicolon(),
          // 10 != 9;
          const Token.int('10'),
          const Token.notEqual(),
          const Token.int('9'),
          const Token.semicolon(),
          Token.eof(),
        ]
      };
    });

    test(
      'should return an array of Tokens given the input of tests map',
      () async {
        // arrange - act - assert
        tests.forEach((key, value) {
          final inputStream = key;
          final expected = value;
          final lexer = Lexer(inputStream);
          for (final expectation in expected) {
            final actual = lexer.nextToken();
            logger.info('expectation: $expectation  <=> actual: $actual');
            expect(actual, expectation);
          }
        });
      },
    );
  });
}
