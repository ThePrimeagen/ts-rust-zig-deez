// ignore_for_file: unnecessary_string_escapes

import 'utils.dart';
import 'token.dart';

class Tokenizer {
  int _pos = 0;
  int _readPos = 0;
  String ch = "\0";

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
      "\0" => Token.eof(),
      String c when c.isLetter() => _lookupIdent(
          _readIdentifier(),
        ),
      String c when c.isNumber() => Token(
          TokenType.int,
          _readNumber(),
        ),
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
    while (ch == " " || ch == "\t" || ch == "\n" || ch == "\r") {
      _readChar();
    }
  }

  void _readChar() {
    if (_readPos >= input.length) {
      ch = "\0";
    } else {
      ch = input[_readPos];
    }

    _pos = _readPos;
    _readPos += 1;
  }

  void _goBack() {
    _pos -= 1;
    _readPos -= 1;
    ch = input[_pos];
  }

  String _readIdentifier() {
    final pos = _pos;
    while (ch.isLetter()) {
      _readChar();
    }
    final res = input.substring(pos, _pos);
    _goBack();
    return res;
  }

  String _readNumber() {
    final pos = _pos;
    while (ch.isNumber()) {
      _readChar();
    }
    final res = input.substring(pos, _pos);
    _goBack();
    return res;
  }

  Iterable<Token> iter() sync* {
    var tok = nextToken();
    while (tok.type != TokenType.eof) {
      yield tok;
      tok = nextToken();
    }
  }
}
