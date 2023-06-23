pub type Token {
  Illegal
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
