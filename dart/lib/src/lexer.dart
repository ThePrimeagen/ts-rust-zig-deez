// ignore_for_file: unnecessary_string_escapes

enum TokenType {
  illegal,
  eof,
  ident,
  int,
  equal,
  plus,
  comma,
  semicolon,
  lParen,
  rParen,
  lSquirly,
  rSquirly,
  function,
  let;

  const TokenType();
}

class Token {
  final TokenType type;
  final String literal;

  const Token(this.type, this.literal);

  @override
  String toString() => "Token.${type.name}('$literal')";

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Token && type == other.type && literal == other.literal;

  @override
  int get hashCode => type.hashCode ^ literal.hashCode;
}

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
      case "=":
        tok = Token(TokenType.equal, ch);
      case "+":
        tok = Token(TokenType.plus, ch);
      case ",":
        tok = Token(TokenType.comma, ch);
      case ";":
        tok = Token(TokenType.semicolon, ch);
      case "(":
        tok = Token(TokenType.lParen, ch);
      case ")":
        tok = Token(TokenType.rParen, ch);
      case "{":
        tok = Token(TokenType.lSquirly, ch);
      case "}":
        tok = Token(TokenType.rSquirly, ch);
      case "\0":
        tok = Token(TokenType.eof, "");
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
}
