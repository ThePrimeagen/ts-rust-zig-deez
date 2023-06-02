enum TokenType {
  illegal,
  eof,

  equal,
  assign,
  notEqual,
  bang,
  plus,
  minus,
  slash,
  asterisk,
  lte,
  gte,
  lt,
  gt,
  comma,
  semicolon,

  lParen,
  rParen,
  lSquirly,
  rSquirly,

  ident,
  int,

  function,
  let,
  true_,
  false_,
  if_,
  else_,
  return_,
  ;

  const TokenType();
}

class Token {
  final TokenType type;
  final String literal;

  const Token(this.type, this.literal);

  const Token.illegal(this.literal) : type = TokenType.illegal;
  const Token.eof() : this(TokenType.eof, "");

  const Token.equal() : this(TokenType.equal, "==");
  const Token.assign() : this(TokenType.assign, "=");
  const Token.notEqual() : this(TokenType.notEqual, "!=");
  const Token.bang() : this(TokenType.bang, "!");
  const Token.plus() : this(TokenType.plus, "+");
  const Token.minus() : this(TokenType.minus, "-");
  const Token.slash() : this(TokenType.slash, "/");
  const Token.asterisk() : this(TokenType.asterisk, "*");
  const Token.lte() : this(TokenType.lte, "<=");
  const Token.gte() : this(TokenType.gte, ">=");
  const Token.lt() : this(TokenType.lt, "<");
  const Token.gt() : this(TokenType.gt, ">");
  const Token.comma() : this(TokenType.comma, ",");
  const Token.semicolon() : this(TokenType.semicolon, ";");

  const Token.lParen() : this(TokenType.lParen, "(");
  const Token.rParen() : this(TokenType.rParen, ")");
  const Token.lSquirly() : this(TokenType.lSquirly, "{");
  const Token.rSquirly() : this(TokenType.rSquirly, "}");

  const Token.ident(this.literal) : type = TokenType.ident;
  const Token.int(this.literal) : type = TokenType.int;
  const Token.fn() : this(TokenType.function, "fn");
  const Token.let() : this(TokenType.let, "let");
  const Token.true_() : this(TokenType.true_, "true");
  const Token.false_() : this(TokenType.false_, "false");
  const Token.if_() : this(TokenType.if_, "if");
  const Token.else_() : this(TokenType.else_, "else");
  const Token.return_() : this(TokenType.return_, "return");

  @override
  String toString() => "Token.${type.name}('$literal')";

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Token && type == other.type && literal == other.literal;

  @override
  int get hashCode => type.hashCode ^ literal.hashCode;
}
