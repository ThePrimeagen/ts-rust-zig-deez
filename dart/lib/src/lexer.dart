// ignore_for_file: unnecessary_string_escapes

import 'utils.dart';
import 'token.dart';

const $EOF = "\u0000";

class Tokenizer {
  int _pos = 0;
  int _readPos = 0;
  String ch = $EOF;

  final String input;

  Tokenizer(this.input) {
    _readChar();
  }

  Token nextToken() {
    _skipWhitespace();

    final tok = switch (ch) {
      "=" => Token.equal(),
      "+" => Token.plus(),
      "," => Token.comma(),
      ";" => Token.semicolon(),
      "(" => Token.lParen(),
      ")" => Token.rParen(),
      "{" => Token.lSquirly(),
      "}" => Token.rSquirly(),
      $EOF => Token.eof(),
      String c when c.isLetter() => _readIdentifier(),
      String c when c.isNumber() => _readNumber(),
      _ => Token(TokenType.illegal, ch)
    };

    _readChar();
    return tok;
  }

  Token _lookupIdent(String ident) {
    return switch (ident) {
      "let" => Token.let(),
      "fn" => Token.fn(),
      _ => Token.ident(ident),
    };
  }

  void _skipWhitespace() {
    while (ch.isWhitespace()) {
      _readChar();
    }
  }

  void _readChar() {
    ch = switch (_readPos) {
      int p when p >= input.length => $EOF,
      _ => input[_readPos],
    };

    _pos = _readPos;
    _readPos += 1;
  }

  void _goBack() {
    _pos -= 1;
    _readPos -= 1;
    ch = input[_pos];
  }

  Token _readIdentifier() {
    final pos = _pos;
    while (ch.isLetter()) {
      _readChar();
    }
    final id = input.substring(pos, _pos);
    _goBack();
    return _lookupIdent(id);
  }

  Token _readNumber() {
    final pos = _pos;
    while (ch.isNumber()) {
      _readChar();
    }
    final number = input.substring(pos, _pos);
    _goBack();
    return Token.int(number);
  }

  Iterable<Token> iter() sync* {
    var tok = nextToken();
    while (tok.type != TokenType.eof) {
      yield tok;
      tok = nextToken();
    }
  }
}
