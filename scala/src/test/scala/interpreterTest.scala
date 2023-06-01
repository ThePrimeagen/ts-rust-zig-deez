//> using test.dep org.scalameta::munit::0.7.29

class LexerTest extends munit.FunSuite {

  test("Test lexer") {
    import token.Token.*
    import token.Token
    import lexer.*
    val input = """
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
    """.stripMargin

    val tests: List[Token] = List(
      Let,
      Ident("five"),
      Assign,
      Int("5"),
      SemiColon,
      Let,
      Ident("ten"),
      Assign,
      Int("10"),
      SemiColon,
      Let,
      Ident("add"),
      Assign,
      Function,
      LParen,
      Ident("x"),
      Comma,
      Ident("y"),
      RParen,
      LBrace,
      Ident("x"),
      Plus,
      Ident("y"),
      SemiColon,
      RBrace,
      SemiColon,
      Let,
      Ident("result"),
      Assign,
      Ident("add"),
      LParen,
      Ident("five"),
      Comma,
      Ident("ten"),
      RParen,
      SemiColon,
      Bang,
      Minus,
      Slash,
      Asterix,
      Int("5"),
      SemiColon,
      Int("5"),
      LT,
      Int("10"),
      GT,
      Int("5"),
      SemiColon,
      If,
      LParen,
      Int("5"),
      LT,
      Int("10"),
      RParen,
      LBrace,
      Return,
      Ident("true"),
      SemiColon,
      RBrace,
      Else,
      LBrace,
      Return,
      Ident("false"),
      SemiColon,
      RBrace,
      Int("10"),
      Eq,
      Int("10"),
      SemiColon,
      Int("10"),
      NotEq,
      Int("9"),
      SemiColon,
      EOF
    )

    // The repl has the nice immutable implementation of iterating the lexer, I'm too lazy here
    var l = Lexer(input)
    tests.zipWithIndex.foreach { case (expectedLiteral, i) =>
      val lexerAndToken = l.nextToken
      l = lexerAndToken._1
      val tk = lexerAndToken._2

      assertEquals(
        tk.literal,
        expectedLiteral.literal,
        s"tests[$i] - literal wrong. Literal ${tk.literal}. Expected $expectedLiteral"
      )
    }
  }
}
