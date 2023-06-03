/*
 * Project: src
 * Created Date: Friday June 2nd 2023 3:25:08 pm
 * Author: Fa C Shus (paul@facshus.com)
 * -----
 * Last Modified: Friday, 2nd June 2023 3:25:09 pm
 * Modified By: Fa C Shus (paul@facshus.com)
 * -----
 * Copyright (c) 2021 - 2023 FaCShus Systems
 * License: MIT
 */

import 'package:monkeydart/monkeydart.dart';
import 'package:test/test.dart';

void main() {
  group('StringExt', () {
    test('should return true when isAlpha is called with a string of 1 letter',
        () async {
      // arrange
      const str = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';

      // act & assert
      for (var i = 0; i < str.length; i++) {
        expect(str[i].isAlpha(), true);
      }
    });
    test(
      'should return false when isAlpha is called with a string ^ of 1 letter',
      () async {
        // arrange
        const str = [
          'aa',
          'ZZ',
          '2',
          '=',
        ];

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(str[i].isAlpha(), false);
        }
      },
    );

    test(
      'should return true when isDigit is called with a string of 1 digit 0..9',
      () async {
        // arrange
        const str = '0123456789';

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(str[i].isDigit(), true);
        }
      },
    );
    test(
      'should return false when isDigit is called with a string ^ of digit 0-9',
      () async {
        // arrange
        const str = [
          'aa',
          'ZZ',
          '22',
          '=',
        ];

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(str[i].isDigit(), false);
        }
      },
    );

    test(
      'should return #t when isWhitespace is called with a str of whitespace',
      () async {
        // arrange
        const str = whitespace;

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(str[i].isWhitespace(), true);
        }
      },
    );

    test(
      'should return true when A <= A|B',
      () async {
        // arrange
        const target = 'A';
        const str = 'AB';

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(target <= str[i], true);
        }
      },
    );
    test(
      'should return true when B >= A|B',
      () async {
        // arrange
        const target = 'B';
        const str = 'AB';

        // act & assert
        for (var i = 0; i < str.length; i++) {
          expect(target >= str[i], true);
        }
      },
    );
  });
}
