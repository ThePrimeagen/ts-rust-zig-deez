internal class Monkey.Lexer {
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

  static HashTable<string, Monkey.TokenType?> keywords;

  static construct {
    keywords = new HashTable<string, Monkey.TokenType?>(str_hash, str_equal);
    keywords.insert("fn", Monkey.TokenType.FUNCTION);
    keywords.insert("let", Monkey.TokenType.LET);
    keywords.insert("true", Monkey.TokenType.TRUE);
    keywords.insert("false", Monkey.TokenType.FALSE);
    keywords.insert("if", Monkey.TokenType.IF);
    keywords.insert("else", Monkey.TokenType.ELSE);
    keywords.insert("return", Monkey.TokenType.RETURN);
  }

  static Monkey.TokenType lookup_ident(string ident) {
    var type = keywords.lookup(ident);
    return type != null ? type : Monkey.TokenType.IDENT;
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
          tok = Token(Monkey.TokenType.EQ, "==");
        } else {
          tok = Token(Monkey.TokenType.ASSIGN, "=");
        }
        break;
      case '+':
        tok = Token(Monkey.TokenType.PLUS, "+");
        break;
      case '-':
        tok = Token(Monkey.TokenType.MINUS, "-");
        break;
      case '!':
        if (peek_char() == '=') {
          read_char();
          tok = Token(Monkey.TokenType.NOT_EQ, "!=");
        } else {
          tok = Token(Monkey.TokenType.BANG, "!");
        }
        break;
      case '/':
        tok = Token(Monkey.TokenType.SLASH, "/");
        break;
      case '*':
        tok = Token(Monkey.TokenType.ASTERISK, "*");
        break;
      case '<':
        tok = Token(Monkey.TokenType.LT, "<");
        break;
      case '>':
        tok = Token(Monkey.TokenType.GT, ">");
        break;
      case ';':
        tok = Token(Monkey.TokenType.SEMICOLON, ";");
        break;
      case ',':
        tok = Token(Monkey.TokenType.COMMA, ",");
        break;
      case '(':
        tok = Token(Monkey.TokenType.LPAREN, "(");
        break;
      case ')':
        tok = Token(Monkey.TokenType.RPAREN, ")");
        break;
      case '{':
        tok = Token(Monkey.TokenType.LBRACE, "{");
        break;
      case '}':
        tok = Token(Monkey.TokenType.RBRACE, "}");
        break;
      case '\0':
        tok = Token(Monkey.TokenType.EOF);
        break;
      default:
        tok = Token();
        if (is_letter(ch)) {
          tok.literal = read_identifier();
          tok.type = lookup_ident(tok.literal);
          return tok;
        } else if (is_digit(ch)) {
          tok.literal = read_number();
          tok.type = Monkey.TokenType.INT;
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
