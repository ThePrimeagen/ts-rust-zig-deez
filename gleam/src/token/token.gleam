pub type Token {
  Illegal(String)
  Eof

  // Items 
  Ident(String)
  Int(String)
  String(String)

  // Operators 
  Assign
  Plus
  Minus
  Bang
  Asterisk
  Slash
  LessThan
  GreaterThan
  Equal
  NotEqual

  // Delimiters 
  Comma
  Semicolon
  Colon
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftBracket
  RightBracket

  // Keywords
  Macro
  Function
  Let
  True
  False
  If
  Else
  Return
}

pub fn lookup_ident(str) {
  case str {
    "fn" -> Function
    "let" -> Let
    "true" -> True
    "false" -> False
    "if" -> If
    "else" -> Else
    "return" -> Return
    "macro" -> Macro
    _ -> Ident(str)
  }
}
