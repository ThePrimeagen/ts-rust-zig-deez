import std/[strutils, sequtils]

type 
  TokenKind* = enum
    ident = "Ident"
    t_int = "Int"
    illegal = "Illegal"
    equal = "Equals"
    plus = "Plus"
    comma = "Comma"
    semicolon = "Semicolon"
    leftparen = "LeftParen"
    rightparen = "RightParen"
    leftsquirrel = "LeftSquirrel"
    rightsquirrel = "RightSquirrel"
    function = "Function"
    t_let = "Let"
  Token* = object
    kind*: TokenKind
    value*: string

proc getIdent*(s: string): TokenKind =
  case s:
    of "fn":
      result = function
    of "let":
      result = t_let
    else:
      result = ident
  for c in s:
    if not c.isDigit:
      return
  result = t_int

proc getSymbol*(s: string): TokenKind =
  case s:
    of "=": result = equal
    of "+": result = plus
    of ",": result = comma
    of ";": result = semicolon
    of "(": result = leftparen
    of ")": result = rightparen
    of "{": result = leftsquirrel
    of "}": result = rightsquirrel
    else: result = illegal

proc getType*(s: string): Token =
  var tokentype = getIdent(s)
  result = Token(kind: tokentype, value: $tokentype)
  if tokentype == ident:
    return Token(kind: ident, value: s)
  elif tokentype == t_int:
    return Token(kind: t_int, value: s)
  else:
    let tkn = getIdent(s)
    return Token(kind: tkn, value: $tkn)

iterator lex*(s: string): Token =
  var lexme = ""
  for l in s:
    case l:
      of Letters:
        lexme &= l
      of Digits:
        lexme &= l
      of Whitespace:
        if lexme.len == 0:
          continue
        yield getType(lexme)
        lexme = ""
      else:
        if lexme.len > 0:
          yield getType(lexme)
        let sym = getSymbol($l)
        yield Token(kind: sym, value: $sym)
        lexme = ""

