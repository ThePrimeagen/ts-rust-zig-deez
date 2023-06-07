enum TokenType {
  ILLEGAL,
  EOF,

  IDENT,
  INT,

  ASSIGN,
  PLUS,

  COMMA,
  SEMICOLON,

  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,

  FUNCTION,
  LET,
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

  public Token next_token() {
    Token tok;

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
        warn_if_reached();
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
  const string input = "=+(){},;";

  Test[] tests = {
    { TokenType.ASSIGN, "=" },
    { TokenType.PLUS, "+" },
    { TokenType.LPAREN, "(" },
    { TokenType.RPAREN, ")" },
    { TokenType.LBRACE, "{" },
    { TokenType.RBRACE, "}" },
    { TokenType.COMMA, "," },
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
