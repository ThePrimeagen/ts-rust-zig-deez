import tables
import strformat

type 
    TokenType* = enum
        ttIllegal
        ttEof

        ttInt
        ttIdent

        ttAssign
        ttPlus
        ttMinus
        ttBang
        ttAsterisk,
        ttSlash,

        ttLt,
        ttGt,
        ttEql,
        ttNeql,

        ttComma
        ttSemicolon
        ttLparen
        ttRparen
        ttLbrace
        ttRbrace

        ttFn
        ttLet
        ttTrue
        ttFalse
        ttIf
        ttElse
        ttReturn


    Token* = object
        case ty*: TokenType
        of ttInt, ttIdent: lit*: string
        else: discard

proc `!=`*(t1, t2: Token): bool = 
    if t1.ty != t2.ty: 
        return true
    case t1.ty:
    of ttIdent, ttInt:
        result = t1.lit != t2.lit
    else: result = false

const keywords = ([
    ("let", ttLet), 
    ("fn", ttFn), 
    ("true", ttTrue), 
    ("false", ttFalse), 
    ("if", ttIf), 
    ("else", ttElse), 
    ("return", ttReturn)
]).toTable

proc lookupIdent*(ident: string): TokenType {.inline.} = 
    if ident in keywords:
        return keywords[ident]
    result = ttIdent


