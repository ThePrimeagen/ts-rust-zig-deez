enum TokenType {
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

struct Token {
  TokenType type;
  string literal;

  Token(TokenType type, string literal = "") {
    this.type = type;
    this.literal = literal;
  }
}

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
      case '=': tok = Token(TokenType.ASSIGN, @"$ch"); break;
      case ';': tok = Token(TokenType.SEMICOLON, @"$ch"); break;
      case '(': tok = Token(TokenType.LPAREN, @"$ch"); break;
      case ')': tok = Token(TokenType.RPAREN, @"$ch"); break;
      case ',': tok = Token(TokenType.COMMA, @"$ch"); break;
      case '+': tok = Token(TokenType.PLUS, @"$ch"); break;
      case '{': tok = Token(TokenType.LBRACE, @"$ch"); break;
      case '}': tok = Token(TokenType.RBRACE, @"$ch"); break;
      case '\0': tok = Token(TokenType.EOF); break;
      default:
        tok = Token(TokenType.ILLEGAL);
        if (is_letter(ch)) {
          tok.literal = read_identifier();
          tok.type = lookup_ident(tok.literal);
          return tok;
        } else if (is_digit(ch)) {
          tok.literal = read_number();
          tok.type = TokenType.INT;
          return tok;
        } else {
          tok = Token(TokenType.ILLEGAL);
          warn_if_reached();
        }
        break;
    }

    read_char();
    return tok;
  }

}

struct Test {
  TokenType expected_type;
  string expected_literal;
}

static int main(string[] args) {
  const string input = """let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
""";

  const Test[] tests = {
    { TokenType.LET, "let" },
    { TokenType.IDENT, "five" },
    { TokenType.ASSIGN, "=" },
    { TokenType.INT, "5" },
    { TokenType.SEMICOLON, ";" },
    { TokenType.LET, "let" },
    { TokenType.IDENT, "ten" },
    { TokenType.ASSIGN, "=" },
    { TokenType.INT, "10" },
    { TokenType.SEMICOLON, ";" },
    { TokenType.LET, "let" },
    { TokenType.IDENT, "add" },
    { TokenType.ASSIGN, "=" },
    { TokenType.FUNCTION, "fn" },
    { TokenType.LPAREN, "(" },
    { TokenType.IDENT, "x" },
    { TokenType.COMMA, "," },
    { TokenType.IDENT, "y" },
    { TokenType.RPAREN, ")" },
    { TokenType.LBRACE, "{" },
    { TokenType.IDENT, "x" },
    { TokenType.PLUS, "+" },
    { TokenType.IDENT, "y" },
    { TokenType.SEMICOLON, ";" },
    { TokenType.RBRACE, "}" },
    { TokenType.SEMICOLON, ";" },
    { TokenType.LET, "let" },
    { TokenType.IDENT, "result" },
    { TokenType.ASSIGN, "=" },
    { TokenType.IDENT, "add" },
    { TokenType.LPAREN, "(" },
    { TokenType.IDENT, "five" },
    { TokenType.COMMA, "," },
    { TokenType.IDENT, "ten" },
    { TokenType.RPAREN, ")" },
    { TokenType.SEMICOLON, ";" },
    { TokenType.EOF, "" },
  };

  var l = new Lexer(input);

  for (var i = 0; i < tests.length; i++) {
    var tok = l.next_token();

    if (tok.type != tests[i].expected_type) {
      printerr(@"tests[$i] - tokentype wrong. expected=$(tests[i].expected_type), got=$(tok.type)\n");
      return 1;
    }

    if (tok.literal != tests[i].expected_literal) {
      printerr(@"tests[$i] - literal wrong. expected=$(tests[i].expected_literal), got=$(tok.literal)\n");
      return 1;
    }
  }

  return 0;
}
