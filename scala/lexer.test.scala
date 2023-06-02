//> using test.dep org.scalatest::scalatest::3.2.16

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must
import org.scalatest.prop.TableDrivenPropertyChecks._

class LexerSpec extends AnyWordSpec with must.Matchers {

  "parseTokens()" must {

    "throw on unexpected input that is not currently handled/implemented" in {
      assertThrows[AssertionError](parseTokens("\uD83E\uDD5C\uD83D\uDE33"))
    }

    "be able to parse a single char token" in {
      val cases = Table(
        ("Input", "Expected result"),
        ("=", Token.Equal),
        ("+", Token.Plus),
        (",", Token.Comma),
        (";", Token.Semicolon),
        ("{", Token.LSquirly),
        ("}", Token.RSquirly),
        ("(", Token.LParen),
        (")", Token.RParen),
        (" ", Token.Whitespace),
        ("\t", Token.Whitespace)
      )

      forAll(cases) { (input: String, expected: Token) =>
        parseTokens(input) must be(Seq(expected))
      }
    }

    "be able to parse a sequence of char tokens" in {
      parseTokens("=+(){},") must be(
        Seq(
          Token.Equal,
          Token.Plus,
          Token.LParen,
          Token.RParen,
          Token.LSquirly,
          Token.RSquirly,
          Token.Comma
        )
      )
    }

    "be able to parse a keyword token" in {
      parseTokens("let") must be(Seq(Token.Let))
      parseTokens("fn") must be(Seq(Token.Function))
    }

    "be able to parse a string literal" in {
      parseTokens("\"hello\"") must be(Seq(Token.Str("hello")))
    }

    "be able to parse an integer literal" in {
      parseTokens("123") must be(Seq(Token.Integer(123)))
    }

    "produce Token.Invalid for a number that exceeds the range of an int" in {
      val input = Integer.MAX_VALUE.toString + "0"
      parseTokens(input) must be(Seq(Token.Invalid))
    }

    "be able to parse an identifier starting with an underscore" in {
      parseTokens("_hello") must be(Seq(Token.Identifier("_hello")))
    }

    "be able to parse an identifier with a number" in {
      parseTokens("hello123") must be(Seq(Token.Identifier("hello123")))
    }

    "be able to parse a simple assignment" in {
      parseTokens("let x = 5;").filterNot(_ == Token.Whitespace) must be(
        Seq(
          Token.Let,
          Token.Identifier("x"),
          Token.Equal,
          Token.Integer(5),
          Token.Semicolon
        )
      )
    }

    "be able to parse complex input" in {
      val input: String =
        """
          |let five = 5;
          |let ten = 10;
          |
          |let add = fn(x, y) {
          |   x + y;
          |};
          |
          |let result = add(five, ten);
          |""".stripMargin

      // Don't care about whitespace for assertions, discard it after parsing
      val res = parseTokens(input).filterNot(_ == Token.Whitespace)

      res must be(
        Seq(
          // let five = 5;
          Token.Let,
          Token.Identifier("five"),
          Token.Equal,
          Token.Integer(5),
          Token.Semicolon,

          // let ten = 10;
          Token.Let,
          Token.Identifier("ten"),
          Token.Equal,
          Token.Integer(10),
          Token.Semicolon,

          // let add = fn(x, y) {
          //   x + y;
          // };
          Token.Let,
          Token.Identifier("add"),
          Token.Equal,
          Token.Function,
          Token.LParen,
          Token.Identifier("x"),
          Token.Comma,
          Token.Identifier("y"),
          Token.RParen,
          Token.LSquirly,
          Token.Identifier("x"),
          Token.Plus,
          Token.Identifier("y"),
          Token.Semicolon,
          Token.RSquirly,
          Token.Semicolon,

          // let result = add(five, ten);
          Token.Let,
          Token.Identifier("result"),
          Token.Equal,
          Token.Identifier("add"),
          Token.LParen,
          Token.Identifier("five"),
          Token.Comma,
          Token.Identifier("ten"),
          Token.RParen,
          Token.Semicolon
        )
      )
    }
  }
}
