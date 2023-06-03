import std/[strutils, sequtils]

type 
  TokenKind* = enum
    tkIllegal
    tkLet
    tkIdent
    tkFunction
    tkInt
    tkEqual
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
  

proc getIdent*(s: string): TokenKind =
  case s:
    of "fn":
      result = tkFunction
    of "let":
      result = tkLet
    else:
      result = tkIdent
  for c in s:
    if not c.isDigit:
      return
  result = tkInt

proc getSymbol*(s: string): TokenKind =
  case s:
    of "=": result = tkEqual
    of "+": result = tkPlus
    of ",": result = tkComma
    of ";": result = tkSemicolon
    of "(": result = tkLParen
    of ")": result = tkRParen
    of "{": result = tkLBrace
    of "}": result = tkRBrace
    else: result = tkIllegal

proc getType*(s: string): Token =
  var tokentype = getIdent(s)
  result = Token(kind: tokentype, value: $tokentype)
  if tokentype == tkIdent:
    return Token(kind: tkIdent, value: s)
  elif tokentype == tkInt:
    return Token(kind: tkInt, value: s)
  else:
    let tkn = getIdent(s)
    return Token(kind: tkn, value: $tkn)

iterator lex*(s: string): Token =
  var lexeme = ""
  for l in s:
    case l:
      of Letters:
        lexeme &= l
      of Digits:
        lexeme &= l
      of Whitespace:
        if lexeme.len == 0:
          continue
        yield getType(lexeme)
        lexeme = ""
      else:
        if lexeme.len > 0:
          yield getType(lexeme)
        let sym = getSymbol($l)
        yield Token(kind: sym, value: $sym)
        lexeme = ""

