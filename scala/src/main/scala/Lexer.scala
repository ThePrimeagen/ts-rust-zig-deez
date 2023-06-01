package lexer

import token.*

case class Lexer(
    input: String,
    position: Int = 0,
    readPosition: Int = 0,
    ch: Option[Char] = None
)

object Lexer:
  import Token.*

  def apply(input: String): Lexer = new Lexer(input).readChar

  extension (l: Lexer)
    def readChar: Lexer =
      l.copy(
        ch =
          if l.readPosition >= l.input.size then None
          else Some(l.input.charAt(l.readPosition)),
        position = l.readPosition,
        readPosition = l.readPosition + 1
      )

    def readIdentifier(str: String, check: Char => Boolean): (Lexer, String) =
      if !check(l.ch.getOrElse('_')) then (l, str)
      else l.readChar.readIdentifier(str ++ l.ch.getOrElse("").toString, check)

    def nextToken: (Lexer, Token) =
      l.ch match
        case Some(char) => matchNext(char, l)
        case None       => (l.readChar, EOF)

  end extension

  private val keywords =
    Map(
      "fn" -> Function,
      "let" -> Let,
      "true" -> True,
      "false" -> False,
      "if" -> If,
      "else" -> Else,
      "return" -> Return
    )

  private def lookupIdent(ident: String): Token =
    keywords.get(ident).getOrElse(Ident(ident))

  private def matchNext(char: Char, l: Lexer): (Lexer, Token) =
    char match
      case ' ' | '\t' | '\n' | '\r' => l.readChar.nextToken
      case '=' =>
        val next = l.readChar
        next.ch match
          case Some(value) if value == '=' => (next.readChar, Eq)
          case _                           => (l.readChar, Assign)
      case ';' => (l.readChar, SemiColon)
      case '(' => (l.readChar, LParen)
      case ')' => (l.readChar, RParen)
      case ',' => (l.readChar, Comma)
      case '{' => (l.readChar, LBrace)
      case '}' => (l.readChar, RBrace)
      case '-' => (l.readChar, Minus)
      case '!' =>
        val next = l.readChar
        next.ch match
          case Some(value) if value == '=' => (next.readChar, NotEq)
          case _                           => (l.readChar, Bang)
      case '*' => (l.readChar, Asterix)
      case '/' => (l.readChar, Slash)
      case '<' => (l.readChar, LT)
      case '>' => (l.readChar, GT)
      case '+' => (l.readChar, Plus)
      case _ if char.isDigit =>
        val digit = l.readIdentifier("", (ch: Char) => ch.isDigit)
        (digit._1, Int(digit._2))
      case _ if char.isLetter =>
        val letter = l.readIdentifier("", (ch: Char) => ch.isLetter)
        (letter._1, lookupIdent(letter._2))
      case _ => (l.readChar, Illegal(char.toString))

end Lexer
