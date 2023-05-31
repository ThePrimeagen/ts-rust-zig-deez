type
  TokenKind* = enum
    # identifiers + literals
    tkIdent,
    tkInt,
    # atoms
    tkIllegal,
    tkEof,
    tkEqual,
    tkPlus,
    tkComma,
    tkSemicolon,
    tkLparen,
    tkRparen,
    tkLSquirly,
    tkRSquirly,
    tkFunction,
    tkLet

  Token* = object
    case kind*: TokenKind
    of tkIdent, tkInt:
      value*: string
    else:
      discard

type
  Lexer* = ref object
    position*: int
    read_position*: int
    ch*: char
    input*: seq[char]

proc newLexer(input: string): Lexer =
  result = Lexer(
    position: 0,
    read_position: 0,
    ch: '\0',
    input: toSeq(input)
  )
  result.read_char()

proc read_char*(self: var Lexer) =
  if self.read_position >= self.input.len:
    self.ch = '\0'
  else:
    self.ch = self.input[self.read_position]
  self.position = self.read_position
  self.read_position += 1

proc skip_whitespace*(self: var Lexer) =
  while self.ch.isSpace:
    self.read_char()

proc read_ident*(self: var Lexer): string =
  let pos = self.position
  while self.ch.isAlphaAscii or self.ch == '_':
    self.read_char()
  return self.input[pos ..< self.position].join()

proc read_int*(self: var Lexer): string =
  let pos = self.position
  while self.ch.isDigit:
    self.read_char()
  return self.input[pos ..< self.position].join()

proc next_token*(self: var Lexer): Token =
  self.skip_whitespace()

  let tok = case self.ch:
    '{': Token(kind: tkLSquirly)
    '}': Token(kind: tkRSquirly)
    '(': Token(kind: tkLparen)
    ')': Token(kind: tkRparen)
    ',': Token(kind: tkComma)
    ';': Token(kind: tkSemicolon)
    '+': Token(kind: tkPlus)
    '=': Token(kind: tkEqual)
    'a'..'z', 'A'..'Z', '_':
      let ident = self.read_ident()
      if ident == "fn":
        Token(kind: tkFunction)
      elif ident == "let":
        Token(kind: tkLet)
      else:
        Token(kind: tkIdent, value: ident)
    '0'..'9':
      Token(kind: tkInt, value: self.read_int())
    '\0':
      Token(kind: tkEof)
    else:
      raise newException(ValueError, "Unexpected character")

  self.read_char()
  return tok
