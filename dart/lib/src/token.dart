enum TokenType {
  illegal,
  eof,
  ident,
  int,
  // TODO: (Fa C Shus) consider changing equal to assign,
  equal,
  plus,
  comma,
  semicolon,
  lParen,
  rParen,
  lSquirly,
  rSquirly,
  function,
  let,
  bang,
  asterisk,
  slash,
  dash,
  gt,
  lt,
  eq,
  neq,
  true_,
  false_,
  if_,
  else_,
  return_;

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

  const Token.fn()
      : type = TokenType.function,
        literal = "fn";

  const Token.let()
      : type = TokenType.let,
        literal = "let";

  const Token.bang()
      : type = TokenType.bang,
        literal = "!";

  const Token.slash()
      : type = TokenType.slash,
        literal = "";

  const Token.dash()
      : type = TokenType.dash,
        literal = "-";

  const Token.asterisk()
      : type = TokenType.asterisk,
        literal = "*";

  const Token.eq()
      : type = TokenType.eq,
        literal = "==";

  const Token.neq()
      : type = TokenType.neq,
        literal = "!=";

  const Token.gt()
      : type = TokenType.gt,
        literal = ">";

  const Token.lt()
      : type = TokenType.lt,
        literal = "<";

  const Token.true_()
      : type = TokenType.true_,
        literal = "true"; 

  const Token.false_()
      : type = TokenType.false_,
        literal = "false";  

  const Token.if_()
      : type = TokenType.if_,
        literal = "if";

  const Token.else_()
      : type = TokenType.else_,
        literal = "else";

  const Token.return_()
      : type = TokenType.return_,
        literal = "return";
        

  @override
  String toString() => "Token.${type.name}('$literal')";

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Token && type == other.type && literal == other.literal;

  @override
  int get hashCode => type.hashCode ^ literal.hashCode;
}
