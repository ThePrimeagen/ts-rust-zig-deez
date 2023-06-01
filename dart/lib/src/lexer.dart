// ignore_for_file: unnecessary_string_escapes, non_constant_identifier_names

import 'package:dart_deez/dart_deez.dart';

final _0 = "0".codeUnitAt(0);
final _9 = "9".codeUnitAt(0);

final _a = "a".codeUnitAt(0);
final _z = "z".codeUnitAt(0);

final _A = "A".codeUnitAt(0);
final _Z = "Z".codeUnitAt(0);

final _ = "_".codeUnitAt(0);

bool isLetter(String c) {
  final ch = c.codeUnitAt(0);
  return (_a <= ch && ch <= _z) || (_A <= ch && ch <= _Z) || ch == _;
}

bool isNumber(String c) {
  final ch = c.codeUnitAt(0);
  return _0 <= ch && ch <= _9;
}

const keywords = {
  "fn": Token(TokenType.function, "fn"),
  "let": Token(TokenType.let, "let"),
  "if": Token(TokenType.if_, "if"),
  "else": Token(TokenType.else_, "else"),
  "return": Token(TokenType.return_, "return"),
  "true": Token(TokenType.true_, "true"),
  "false": Token(TokenType.false_, "false"),
};

class Tokenizer {
  int position = 0;
  int readPosition = 0;
  String ch;

  final String input;

  Tokenizer(this.input) : ch = "\0" {
    readChar();
  }

  Token nextToken() {
    skipWhitespace();

    Token tok;
    switch (ch) {
      case "+":
        tok = Token.plus();
      case ",":
        tok = Token.comma();
      case ";":
        tok = Token.semicolon();
      case "(":
        tok = Token.lParen();
      case ")":
        tok = Token.rParen();
      case "{":
        tok = Token.lSquirly();
      case "}":
        tok = Token.rSquirly();
      case '-':
        tok = Token.dash();
      case '*':
        tok = Token.asterisk();
      case '/':
        tok = Token.slash();
      case '<':
        tok = Token.lt();
      case '>':
        tok = Token.gt();

      case "=":
        if (peekChar() == '=') {
          readChar();
          tok = Token.eq();
        } else {
          tok = Token.equal();
        }
        tok = Token.equal();
      case '!':
        if (peekChar() == '=') {
          readChar();
          tok = Token.neq();
        } else {
          tok = Token.bang();
        }

      case "\0":
        tok = Token.eof();
      case _:
        if (isLetter(ch)) {
          final literal = readIdentifier();
          final tokenType = lookupIdent(literal);
          return Token(tokenType, literal);
        } else if (isNumber(ch)) {
          return Token(TokenType.int, readNumber());
        } else {
          tok = Token(TokenType.illegal, ch);
        }
    }

    readChar();
    return tok;
  }

  TokenType lookupIdent(String ident) {
    return keywords[ident]?.type ?? TokenType.ident;
  }

  void skipWhitespace() {
    while (ch == " " || ch == "\t" || ch == "\n" || ch == "\r") {
      readChar();
    }
  }

  void readChar() {
    if (readPosition >= input.length) {
      ch = "\0";
    } else {
      ch = input[readPosition];
    }

    position = readPosition;
    readPosition += 1;
  }

  String readIdentifier() {
    final pos = position;
    while (isLetter(ch)) {
      readChar();
    }
    return input.substring(pos, position);
  }

  String readNumber() {
    final pos = position;
    while (isNumber(ch)) {
      readChar();
    }
    return input.substring(pos, position);
  }

  String peekChar() {
    if (readPosition >= input.length) {
      return "\0";
    } else {
      return input[readPosition];
    }
  }
}
