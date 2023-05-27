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

  const Token.eof()
      : type = TokenType.eof,
        literal = "";

  const Token.illegal(this.literal) : type = TokenType.illegal;

  const Token.ident(this.literal) : type = TokenType.ident;

  const Token.int(this.literal) : type = TokenType.int;

  const Token.equal()
      : type = TokenType.equal,
        literal = "=";

  const Token.plus()
      : type = TokenType.plus,
        literal = "+";

  const Token.comma()
      : type = TokenType.comma,
        literal = ",";

  const Token.semicolon()
      : type = TokenType.semicolon,
        literal = ";";

  const Token.lParen()
      : type = TokenType.lParen,
        literal = "(";

  const Token.rParen()
      : type = TokenType.rParen,
        literal = ")";

  const Token.lSquirly()
      : type = TokenType.lSquirly,
        literal = "{";

  const Token.rSquirly()
      : type = TokenType.rSquirly,
        literal = "}";

  const Token.function()
      : type = TokenType.function,
        literal = "fn";

  const Token.let()
      : type = TokenType.let,
        literal = "let";

  @override
  String toString() => "Token.${type.name}('$literal')";

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Token && type == other.type && literal == other.literal;

  @override
  int get hashCode => type.hashCode ^ literal.hashCode;
}
