import std/[strutils, sequtils]

type Token* = enum
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

proc getIdent*(s: string): Token =
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

proc getSymbol*(s: string): Token =
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

proc getType*(s: string): (Token, string) =
  var tokentype = getIdent(s)
  result = (tokentype, $tokentype)
  if tokentype == ident:
    return (ident, "Ident " & s)
  elif tokentype == t_int:
    return (t_int, "Int " & s)
  else:
    let tkn = getIdent(s)
    return (tkn, $tkn)

iterator lex*(s: string): (Token, string) =
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
        yield (sym, $sym)
        lexeme = ""

