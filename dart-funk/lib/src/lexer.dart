import 'package:meta/meta.dart';
import 'package:monkeydart/monkeydart.dart';

@immutable
class Lexer {
  factory Lexer(String source, {int position = 0}) {
    if (source.isEmpty || position >= source.length) {
      return Lexer._(source, 0, stringNull);
    } else {
      return Lexer._(source, position, source[position]);
    }
  }
  const Lexer._(this.source, this.position, this.ch);

  final String source;
  final int position;
  final String ch;

  Lexer copyWith({
    String? source,
    int? position,
  }) {
    return Lexer(
      source ?? this.source,
      position: position ?? this.position,
    );
  }
}

/// The return is not optional, what might be null is illegal
(Lexer, Token) nextToken(Lexer lexer) {
  final innerLexer = eatWhitespace(lexer);
  var retVal = (innerLexer, const Token.illegal());

  switch (innerLexer.ch) {
    case stringNull:
      // no need to advance the position; we're done here
      retVal = (
        innerLexer,
        const Token.eof(),
      );
    case '{':
      retVal = (advance(innerLexer), const Token.lSquirly());
    case '}':
      retVal = (advance(innerLexer), const Token.rSquirly());
    case '(':
      retVal = (advance(innerLexer), const Token.lParen());
    case ')':
      retVal = (advance(innerLexer), const Token.rParen());
    case ',':
      retVal = (advance(innerLexer), const Token.comma());
    case ';':
      retVal = (advance(innerLexer), const Token.semicolon());
    case '+':
      retVal = (advance(innerLexer), const Token.plus());
    case '-':
      retVal = (advance(innerLexer), const Token.dash());
    case '*':
      retVal = (advance(innerLexer), const Token.asterisk());
    case '/':
      retVal = (advance(innerLexer), const Token.slash());
    case '>':
      retVal = (advance(innerLexer), const Token.greaterThan());
    case '<':
      retVal = (advance(innerLexer), const Token.lessThan());
    case '=' when peekChar(innerLexer) == '=':
      retVal = (advance(advance(innerLexer)), const Token.equal());
    case '=':
      retVal = (advance(innerLexer), const Token.assign());
    case '!' when peekChar(innerLexer) == '=':
      retVal = (advance(advance(innerLexer)), const Token.notEqual());
    case '!':
      retVal = (advance(innerLexer), const Token.bang());
    case _ when alphas.contains(innerLexer.ch):
      final read = readIdentifier(innerLexer);
      retVal = (read.$1, read.$2);
    case _ when digits.contains(innerLexer.ch):
      final read = readInteger(innerLexer);
      retVal = (read.$1, read.$2);
    case _:
      retVal = (
        innerLexer.copyWith(position: innerLexer.position + 1),
        const Token.illegal(),
      );
  }

  return retVal;
}

/// The return is not optional, what might be null is String Typed null \0
String peekChar(Lexer lexer) {
  if (lexer.position + 1 >= lexer.source.length) {
    return stringNull;
  }
  return lexer.source[lexer.position + 1];
}

/// advance the lexer until the function returns false
(Lexer, int) seekTo(Lexer lexer, bool Function(String) fn) {
  var retVal = lexer;
  while (fn(retVal.ch)) {
    retVal = advance(retVal);
  }
  return (retVal, retVal.position);
}

(Lexer, String) readWhile(Lexer lexer, bool Function(String) fn) {
  final seek = seekTo(lexer, fn);
  return (seek.$1, lexer.source.substring(lexer.position, seek.$2));
}

(Lexer, Token) readIdentifier(Lexer lexer) {
  final read = readWhile(lexer, isAlphy);
  return (read.$1, Token.identity(read.$2));
}

(Lexer, Token) readInteger(Lexer lexer) {
  final read = readWhile(lexer, isDiggy);
  return (read.$1, Token.int(read.$2));
}

Lexer advance(Lexer lexer) {
  return lexer.copyWith(
    position: lexer.position + 1,
  );
}

Lexer eatWhitespace(Lexer lexer) {
  while (lexer.ch.isWhitespace()) {
    return eatWhitespace(
      lexer.copyWith(
        position: lexer.position + 1,
      ),
    );
  }
  return lexer;
}
