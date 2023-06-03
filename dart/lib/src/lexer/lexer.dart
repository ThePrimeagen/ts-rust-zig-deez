import 'utils.dart';
import 'token.dart';

export 'token.dart';

const $EOF = "\u0000";

class Lexer {
  int _pos = 0;
  int _readPos = 0;
  String ch = $EOF;

  final String input;

  Lexer(this.input) {
    _readChar();
  }

  Token nextToken() {
    _skipWhitespace();

    final ch = this.ch;
    final nextCh = _peekChar();
    final tok = switch ((ch, nextCh)) {
      ("=", "=") => _skipNextAs(Token.equal()),
      ("=", _) => Token.assign(),
      ("!", "=") => _skipNextAs(Token.notEqual()),
      ("!", _) => Token.bang(),
      ("+", _) => Token.plus(),
      ("-", _) => Token.minus(),
      ("/", _) => Token.slash(),
      ("*", _) => Token.asterisk(),
      ("<", "=") => _skipNextAs(Token.lte()),
      (">", "=") => _skipNextAs(Token.gte()),
      ("<", _) => Token.lt(),
      (">", _) => Token.gt(),
      (",", _) => Token.comma(),
      (";", _) => Token.semicolon(),
      ("(", _) => Token.lParen(),
      (")", _) => Token.rParen(),
      ("{", _) => Token.lSquirly(),
      ("}", _) => Token.rSquirly(),
      ($EOF, _) => Token.eof(),
      (String c, _) when c.isLetter() => _readIdentifier(),
      (String c, _) when c.isNumber() => _readNumber(),
      _ => Token(TokenType.illegal, ch)
    };

    _readChar();
    return tok;
  }

  Token _lookupIdent(String ident) {
    return switch (ident) {
      "let" => Token.let(),
      "fn" => Token.fn(),
      "true" => Token.true_(),
      "false" => Token.false_(),
      "if" => Token.if_(),
      "else" => Token.else_(),
      "return" => Token.return_(),
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

  String _peekChar() {
    return switch (_readPos) {
      int p when p >= input.length => $EOF,
      _ => input[_readPos],
    };
  }

  void _rewind() {
    _pos -= 1;
    _readPos -= 1;
    ch = input[_pos];
  }

  T _skipNextAs<T>(T t) {
    _readChar();
    return t;
  }

  Token _readIdentifier() {
    final pos = _pos;
    while (ch.isLetter()) {
      _readChar();
    }
    final id = input.substring(pos, _pos);
    _rewind();
    return _lookupIdent(id);
  }

  Token _readNumber() {
    final pos = _pos;
    while (ch.isNumber()) {
      _readChar();
    }
    final number = input.substring(pos, _pos);
    _rewind();
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
