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

  Token(TokenType type = TokenType.ILLEGAL, string literal = "") {
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

struct Test {
  TokenType expected_type;
  string expected_literal;
}

static int main(string[] args) {
  const string input = """
let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
  return true;
} else {
  return false;
}
10 == 10;
10 != 9;
""";

  const Test[] tests = {
		{TokenType.LET, "let"},
		{TokenType.IDENT, "five"},
		{TokenType.ASSIGN, "="},
		{TokenType.INT, "5"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.LET, "let"},
		{TokenType.IDENT, "ten"},
		{TokenType.ASSIGN, "="},
		{TokenType.INT, "10"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.LET, "let"},
		{TokenType.IDENT, "add"},
		{TokenType.ASSIGN, "="},
		{TokenType.FUNCTION, "fn"},
		{TokenType.LPAREN, "("},
		{TokenType.IDENT, "x"},
		{TokenType.COMMA, ","},
		{TokenType.IDENT, "y"},
		{TokenType.RPAREN, ")"},
		{TokenType.LBRACE, "{"},
		{TokenType.IDENT, "x"},
		{TokenType.PLUS, "+"},
		{TokenType.IDENT, "y"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.RBRACE, "}"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.LET, "let"},
		{TokenType.IDENT, "result"},
		{TokenType.ASSIGN, "="},
		{TokenType.IDENT, "add"},
		{TokenType.LPAREN, "("},
		{TokenType.IDENT, "five"},
		{TokenType.COMMA, ","},
		{TokenType.IDENT, "ten"},
		{TokenType.RPAREN, ")"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.BANG, "!"},
		{TokenType.MINUS, "-"},
		{TokenType.SLASH, "/"},
		{TokenType.ASTERISK, "*"},
		{TokenType.INT, "5"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.INT, "5"},
		{TokenType.LT, "<"},
		{TokenType.INT, "10"},
		{TokenType.GT, ">"},
		{TokenType.INT, "5"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.IF, "if"},
		{TokenType.LPAREN, "("},
		{TokenType.INT, "5"},
		{TokenType.LT, "<"},
		{TokenType.INT, "10"},
		{TokenType.RPAREN, ")"},
		{TokenType.LBRACE, "{"},
		{TokenType.RETURN, "return"},
		{TokenType.TRUE, "true"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.RBRACE, "}"},
		{TokenType.ELSE, "else"},
		{TokenType.LBRACE, "{"},
		{TokenType.RETURN, "return"},
		{TokenType.FALSE, "false"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.RBRACE, "}"},
		{TokenType.INT, "10"},
		{TokenType.EQ, "=="},
		{TokenType.INT, "10"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.INT, "10"},
		{TokenType.NOT_EQ, "!="},
		{TokenType.INT, "9"},
		{TokenType.SEMICOLON, ";"},
		{TokenType.EOF, ""},
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
