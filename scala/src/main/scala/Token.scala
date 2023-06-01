package token

enum Token(val literal: String):
  case Illegal(override val literal: String)
      extends Token(s"ILLEGAL: $literal")
  case EOF extends Token("0") // The value could be whatever here.

  // Identifiers + literals
  case Ident(override val literal: String) extends Token(literal)
  case Int(override val literal: String) extends Token(literal)

  // Operators
  case Assign extends Token("=")
  case Plus extends Token("+")
  case Minus extends Token("-")
  case Bang extends Token("!")
  case Asterix extends Token("*")
  case Slash extends Token("/")
  case LT extends Token("<")
  case GT extends Token(">")
  case Eq extends Token("==")
  case NotEq extends Token("!=")

  // Delimiters
  case Comma extends Token(",")
  case SemiColon extends Token(";")
  case LParen extends Token("(")
  case RParen extends Token(")")
  case LBrace extends Token("{")
  case RBrace extends Token("}")

  // Keywords
  case Function extends Token("fn")
  case Let extends Token("let")
  case True extends Token("true")
  case False extends Token("false")
  case If extends Token("if")
  case Else extends Token("else")
  case Return extends Token("return")
end Token
