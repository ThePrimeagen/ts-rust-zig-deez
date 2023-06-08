{.experimental: "codeReordering".}
import std/[strutils, sequtils]

type 
  TokenKind* = enum
    tkIllegal
    tkEof
    tkLet
    tkIdent
    tkFunction
    tkInt
    tkAssign
    tkPlus
    tkComma
    tkSemicolon
    tkLParen
    tkRParen
    tkLBrace
    tkRBrace
  Token* = object
    kind*: TokenKind
    value*: string
  Lexer* = object
    input: string
    position: int
    readPosition: int
    ch: char

proc newLexer*(input: string): Lexer =
  result.input = input
  result.readChar()

proc readChar(l: var Lexer) =
  if l.readPosition >= l.input.len:
    l.ch = '\0'
  else:
    l.ch = l.input[l.readPosition]
  l.position = l.readPosition
  l.readPosition += 1

proc peekChar(l: Lexer): char =
    if l.readposition >= l.input.len:
        return '\0'
    else:
        return l.input[l.readposition]

proc skipWhitespace(l: var Lexer) =
  while l.ch.isSpaceAscii:
    l.readChar()

proc isLetter(c: char): bool =
  c.isAlphaAscii or c == '_'

proc isNumber(c: char): bool =
  c.isDigit

proc readIdent(l: var Lexer): string =
  let start_pos = l.position
  while l.ch.isLetter:
    l.readChar()
  l.input[start_pos ..< l.position]

proc readNumber(l: var Lexer): string =
  let start_pos = l.position
  while l.ch.isNumber:
    l.readChar()
  l.input[start_pos ..< l.position]

proc lookupIdent*(s: string): TokenKind =
  # We'll come back f or the others
  case s:
    of "fn":
      result = tkFunction
    of "let":
      result = tkLet
    else:
      result = tkIdent

# proc getSymbol*(s: string): TokenKind =
#   # Not a fan because it only checks a char not more
#   # Also it does a weird split in our main pathing logic
#   case s:
#     of "=": result = tkAssign
#     of "+": result = tkPlus
#     of ",": result = tkComma
#     of ";": result = tkSemicolon
#     of "(": result = tkLParen
#     of ")": result = tkRParen
#     of "{": result = tkLBrace
#     of "}": result = tkRBrace
#     else: result = tkIllegal

proc nextToken*(l: var Lexer): Token =
  l.skipWhitespace()

  case l.ch:
    of '=':
      result = Token(kind: tkAssign)
    of ';':
      result = Token(kind: tksemicolon)
    of '(':
      result = Token(kind: tkLparen)
    of ')':
      result = Token(kind: tkRparen)
    of ',':
      result = Token(kind: tkComma)
    of '+':
      result = Token(kind: tkPlus)
    of '{':
      result = Token(kind: tkLbrace)
    of '}':
      result = Token(kind: tkRbrace)
    of '\0':
      result = Token(kind: tkEof)
    else:
      if l.ch.isLetter:
        result.value = l.readIdent()
        result.kind = lookupIdent(result.value)
        return result
      elif l.ch.isDigit:
        result.value = l.readNumber()
        result.kind = tkInt
        return result
      else:
        result = Token(kind: tkIllegal, value: $l.ch)

  l.readChar()

iterator tokens*(l: var Lexer): Token =
  var next = l.nextToken()
  while next.kind notin {tkEof, tkIllegal}:
    yield next
    next = l.nextToken()
  yield next


# proc getType*(s: string): Token =
#   # Kinda confused as to how this works. It kinda feels like it was chopped out of a better spot
#   var tokentype = getIdent(s)
#   result = Token(kind: tokentype, value: $tokentype)
#   if tokentype == tkIdent:
#     return Token(kind: tkIdent, value: s)
#   elif tokentype == tkInt:
#     return Token(kind: tkInt, value: s)
#   else:
#     let tkn = getIdent(s)
#     return Token(kind: tkn, value: $tkn)

# iterator lex*(s: string): Token =
#   # We have now shifted to a proc to request the next token. We can still iterate all tokens through tokens(). This proc has the core desicion tree all together in it.
#   var lexeme = ""
#   for l in s:
#     case l:
#       of Letters:
#         lexeme &= l
#       of Digits:
#         lexeme &= l
#       of Whitespace:
#         if lexeme.len == 0:
#           continue
#         yield getType(lexeme)
#         lexeme = ""
#       else:
#         if lexeme.len > 0:
#           yield getType(lexeme)
#         let sym = getSymbol($l)
#         yield Token(kind: sym, value: $sym)
#         lexeme = ""

