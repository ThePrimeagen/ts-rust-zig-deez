/*
 * Project: ts-rust-zig-deez
 * Created Date: Thursday May 25th 2023 10:21:46 am
 * Author: Fa C Shus (paul@facshus.com)
 * -----
 * Last Modified: Thursday, 25th May 2023 10:21:46 am
 * Modified By: Fa C Shus (paul@facshus.com)
 * -----
 * Copyright (c) 2021 - 2023 FaCShus Systems
 * License: MIT
 */

import 'package:monkeydart/monkeydart.dart';

final whitespaceRegex = RegExp(r'\s');

extension GreaterThanOrEqualTo on Comparable<String> {
  bool operator >=(String other) {
    return compareTo(other) >= 0;
  }
}

extension LessThanOrEqualTo on Comparable<String> {
  bool operator <=(String other) {
    return compareTo(other) <= 0;
  }
}

bool isDigit(String val) {
  return val.length == 1 && val >= '0' && val <= '9';
}

bool isAlpha(String val) {
  return val.length == 1 && (val >= 'a' && val <= 'z') ||
      (val >= 'A' && val <= 'Z') ||
      val == '_';
}

class Lexer {
  Lexer(this.source) {
    sourceLength = source.length;
    // prime the first character
    _readChar();
  }
  late String source;
  late int sourceLength;

  int position = 0;
  int readPosition = 0;
  String? currChar;

  Token nextToken() {
    Token retVal;
    _eatWhitespace();

    switch (currChar) {
      case null:
      case '\u0000':
        retVal = Token.eof();
      case '{':
        retVal = const Token.lSquirly();
      case '}':
        retVal = const Token.rSquirly();
      case '(':
        retVal = const Token.lParen();
      case ')':
        retVal = const Token.rParen();
      case ',':
        retVal = const Token.comma();
      case ';':
        retVal = const Token.semicolon();
      case '+':
        retVal = const Token.plus();
      case '-':
        retVal = const Token.minus();
      case '*':
        retVal = const Token.asterisk();
      case '/':
        retVal = const Token.slash();
      case '>':
        retVal = const Token.greaterThan();
      case '<':
        retVal = const Token.lessThan();
      case '!':
        if (_peekChar() == '=') {
          _readChar();
          retVal = const Token.notEqual();
        } else {
          retVal = const Token.bang();
        }
      case '=':
        if (_peekChar() == '=') {
          _readChar();
          retVal = const Token.equal();
        } else {
          retVal = const Token.assign();
        }

      case (>= 'a' && <= 'z') || (>= 'A' && <= 'Z') || ((>= '_' && <= '_')):
        // make me an identifier
        final ident = _readIdentifier();
        final tok = Token.keyword(ident);
        if (tok != null) {
          return tok;
        } else {
          return Token.ident(ident);
        }
      case (>= '0' && <= '9'):
        // make me a number
        final ynt = _readInteger();
        return Token.int(ynt);
      default:
        retVal = Token.illegal();
    }
    // eat the character
    _readChar();
    return retVal;
  }

  String _peekChar() {
    if (readPosition >= sourceLength) {
      return String.fromCharCode(0);
    } else {
      return source[readPosition];
    }
  }

  void _readChar() {
    if (readPosition >= sourceLength) {
      currChar = String.fromCharCode(0);
    } else {
      currChar = source[readPosition];
    }
    position = readPosition;
    readPosition++;
  }

  String _readIdentifier() {
    final startHere = position;
    var len = 0;

    while (isAlpha(currChar ?? '')) {
      _readChar();
      len++;
    }
    return source.substring(startHere, startHere + len);
  }

  String _readInteger() {
    final startHere = position;
    var len = 0;

    while (isDigit(currChar ?? String.fromCharCode(0))) {
      _readChar();
      len++;
    }
    return source.substring(startHere, startHere + len);
  }

  void _eatWhitespace() {
    // eat the whitespaces
    while (currChar != null && whitespaceRegex.hasMatch(currChar!)) {
      _readChar();
    }
  }
}
