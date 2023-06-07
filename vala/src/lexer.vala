class Lexer {
  string input = "";
  int position = 0;
  int read_position = 0;
  char ch = '\0';

  public Lexer(string input) {
    this.input = input;
    read_char();
  }

  void read_char() {
    if (read_position >= input.length) {
      ch = '\0';
    } else {
      ch = input[read_position];
    }
    position = read_position;
    read_position += 1;
  }

  char peek_char() {
    if (read_position >= input.length) {
      return 0;
    } else {
      return input[read_position];
    }
  }

  static bool is_letter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
  }

  static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
  }

  static bool is_digit(char c) {
    return c >= '0' && c <= '9';
  }

  string read_identifier() {
    var pos = position;
    while (is_letter(ch)) {
      read_char();
    }
    return input[pos:position];
  }

  string read_number() {
    var pos = position;
    while (is_digit(ch)) {
      read_char();
    }
    return input[pos:position];
  }

  static HashTable<string, TokenType?> keywords;

  static construct {
    keywords = new HashTable<string, TokenType?>(str_hash, str_equal);
    keywords.insert("fn", TokenType.FUNCTION);
    keywords.insert("let", TokenType.LET);
    keywords.insert("true", TokenType.TRUE);
    keywords.insert("false", TokenType.FALSE);
    keywords.insert("if", TokenType.IF);
    keywords.insert("else", TokenType.ELSE);
    keywords.insert("return", TokenType.RETURN);
  }

  static TokenType lookup_ident(string ident) {
    var type = keywords.lookup(ident);
    return type != null ? type : TokenType.IDENT;
  }

  void skip_whitespace() {
    while (is_whitespace(ch)) {
      read_char();
    }
  }

  public Token next_token() {
    Token tok;

    skip_whitespace();

    switch (ch) {
      case '=':
        if (peek_char() == '=') {
          read_char();
          tok = Token(TokenType.EQ, "==");
        } else {
          tok = Token(TokenType.ASSIGN, "=");
        }
        break;
      case '+':
        tok = Token(TokenType.PLUS, "+");
        break;
      case '-':
        tok = Token(TokenType.MINUS, "-");
        break;
      case '!':
        if (peek_char() == '=') {
          read_char();
          tok = Token(TokenType.NOT_EQ, "!=");
        } else {
          tok = Token(TokenType.BANG, "!");
        }
        break;
      case '/':
        tok = Token(TokenType.SLASH, "/");
        break;
      case '*':
        tok = Token(TokenType.ASTERISK, "*");
        break;
      case '<':
        tok = Token(TokenType.LT, "<");
        break;
      case '>':
        tok = Token(TokenType.GT, ">");
        break;
      case ';':
        tok = Token(TokenType.SEMICOLON, ";");
        break;
      case ',':
        tok = Token(TokenType.COMMA, ",");
        break;
      case '(':
        tok = Token(TokenType.LPAREN, "(");
        break;
      case ')':
        tok = Token(TokenType.RPAREN, ")");
        break;
      case '{':
        tok = Token(TokenType.LBRACE, "{");
        break;
      case '}':
        tok = Token(TokenType.RBRACE, "}");
        break;
      case '\0':
        tok = Token(TokenType.EOF);
        break;
      default:
        tok = Token();
        if (is_letter(ch)) {
          tok.literal = read_identifier();
          tok.type = lookup_ident(tok.literal);
          return tok;
        } else if (is_digit(ch)) {
          tok.literal = read_number();
          tok.type = TokenType.INT;
          return tok;
        } else {
          warn_if_reached();
        }
        break;
    }

    read_char();
    return tok;
  }

}
