struct Test {
  Monkey.TokenType expected_type;
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
		{Monkey.TokenType.LET, "let"},
		{Monkey.TokenType.IDENT, "five"},
		{Monkey.TokenType.ASSIGN, "="},
		{Monkey.TokenType.INT, "5"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.LET, "let"},
		{Monkey.TokenType.IDENT, "ten"},
		{Monkey.TokenType.ASSIGN, "="},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.LET, "let"},
		{Monkey.TokenType.IDENT, "add"},
		{Monkey.TokenType.ASSIGN, "="},
		{Monkey.TokenType.FUNCTION, "fn"},
		{Monkey.TokenType.LPAREN, "("},
		{Monkey.TokenType.IDENT, "x"},
		{Monkey.TokenType.COMMA, ","},
		{Monkey.TokenType.IDENT, "y"},
		{Monkey.TokenType.RPAREN, ")"},
		{Monkey.TokenType.LBRACE, "{"},
		{Monkey.TokenType.IDENT, "x"},
		{Monkey.TokenType.PLUS, "+"},
		{Monkey.TokenType.IDENT, "y"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.RBRACE, "}"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.LET, "let"},
		{Monkey.TokenType.IDENT, "result"},
		{Monkey.TokenType.ASSIGN, "="},
		{Monkey.TokenType.IDENT, "add"},
		{Monkey.TokenType.LPAREN, "("},
		{Monkey.TokenType.IDENT, "five"},
		{Monkey.TokenType.COMMA, ","},
		{Monkey.TokenType.IDENT, "ten"},
		{Monkey.TokenType.RPAREN, ")"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.BANG, "!"},
		{Monkey.TokenType.MINUS, "-"},
		{Monkey.TokenType.SLASH, "/"},
		{Monkey.TokenType.ASTERISK, "*"},
		{Monkey.TokenType.INT, "5"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.INT, "5"},
		{Monkey.TokenType.LT, "<"},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.GT, ">"},
		{Monkey.TokenType.INT, "5"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.IF, "if"},
		{Monkey.TokenType.LPAREN, "("},
		{Monkey.TokenType.INT, "5"},
		{Monkey.TokenType.LT, "<"},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.RPAREN, ")"},
		{Monkey.TokenType.LBRACE, "{"},
		{Monkey.TokenType.RETURN, "return"},
		{Monkey.TokenType.TRUE, "true"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.RBRACE, "}"},
		{Monkey.TokenType.ELSE, "else"},
		{Monkey.TokenType.LBRACE, "{"},
		{Monkey.TokenType.RETURN, "return"},
		{Monkey.TokenType.FALSE, "false"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.RBRACE, "}"},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.EQ, "=="},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.INT, "10"},
		{Monkey.TokenType.NOT_EQ, "!="},
		{Monkey.TokenType.INT, "9"},
		{Monkey.TokenType.SEMICOLON, ";"},
		{Monkey.TokenType.EOF, ""},
  };

  var l = new Monkey.Lexer(input);

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
