enum Token:
  case Eof                      extends Token
  case Equal                    extends Token
  case Plus                     extends Token
  case Comma                    extends Token
  case Semicolon                extends Token
  case LParen                   extends Token
  case RParen                   extends Token
  case LSquirly                 extends Token
  case RSquirly                 extends Token
  case Let                      extends Token
  case Invalid                  extends Token
  case Function                 extends Token
  case Whitespace               extends Token
  case Identifier(name: String) extends Token
  case Str(value: String)       extends Token
  case Integer(value: Int)      extends Token

// Identifiers must start with a letter or underscore, and can contain further letters, numbers and underscores
private val InitialIdentifierChars = (('A' to 'Z') ++ ('a' to 'z') ++ Seq('_')).toSet
private val IdentifierChars        = InitialIdentifierChars ++ ('0' to '9').toSet

def parseTokens(input: String): Seq[Token] = Seq.unfold(input.toList) {
  case Nil                         => None
  case c :: tail if c.isWhitespace => Some((Token.Whitespace, tail))
  case '\u0000' :: tail            => Some((Token.Eof, tail))
  case '=' :: tail                 => Some((Token.Equal, tail))
  case '+' :: tail                 => Some((Token.Plus, tail))
  case ',' :: tail                 => Some((Token.Comma, tail))
  case ';' :: tail                 => Some((Token.Semicolon, tail))
  case '{' :: tail                 => Some((Token.LSquirly, tail))
  case '}' :: tail                 => Some((Token.RSquirly, tail))
  case '(' :: tail                 => Some((Token.LParen, tail))
  case ')' :: tail                 => Some((Token.RParen, tail))
  case 'l' :: 'e' :: 't' :: tail   => Some((Token.Let, tail))
  case 'f' :: 'n' :: tail          => Some((Token.Function, tail))
  case all @ c :: _ if InitialIdentifierChars.contains(c) =>
    val (identifierChars, tail) = all.span(IdentifierChars.contains)
    Some((Token.Identifier(identifierChars.mkString), tail))
  case '"' :: tailWithLiteralValue =>
    val (literalChars, tailWithClosingQuote) = tailWithLiteralValue.span(_ != '"')
    Some((Token.Str(literalChars.mkString), tailWithClosingQuote.tail))
  case all @ c :: _ if c.isDigit =>
    val (valueChars, tail) = all.span(_.isDigit)
    val token              = valueChars.mkString.toIntOption.fold(Token.Invalid)(Token.Integer(_))
    Some((token, tail))
  case unexpected =>
    // This should never happen. 🙃 Throw with enabled assertions (usually in dev). Alternatively try
    // to recover by returning Token.Invalid for the first char and continuing with the remaining input.
    assert(false, s"Dear code person, you forgot to handle deez: $unexpected")
    Some((Token.Invalid, unexpected.tail))
}
