internal enum Monkey.TokenType {
  ILLEGAL,
  EOF,

  // Identifiers and literals
  IDENT,
  INT,

  // Operators
  ASSIGN,
  PLUS,
  MINUS,
  BANG,
  ASTERISK,
  SLASH,
  LT,
  GT,
  EQ,
  NOT_EQ,

  // Delimiters
  COMMA,
  SEMICOLON,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,

  // Keywords
  FUNCTION,
  LET,
  TRUE,
  FALSE,
  IF,
  ELSE,
  RETURN,
}

internal struct Monkey.Token {
  TokenType type;
  string literal;

  public Token(TokenType type = TokenType.ILLEGAL, string literal = "") {
    this.type = type;
    this.literal = literal;
  }
}
