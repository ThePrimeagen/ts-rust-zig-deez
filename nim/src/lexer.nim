import sequtils, strutils, parseutils

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

#forward declaration 
proc read_char*(self: var Lexer)

proc newLexer*(input: string): Lexer =
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
    while self.ch in WhiteSpace:
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
    of '{': Token(kind: tkLSquirly)
    of '}': Token(kind: tkRSquirly)
    of '(': Token(kind: tkLparen)
    of ')': Token(kind: tkRparen)
    of ',': Token(kind: tkComma)
    of ';': Token(kind: tkSemicolon)
    of '+': Token(kind: tkPlus)
    of '=': Token(kind: tkEqual)
    of 'a'..'z', 'A'..'Z', '_':
        let ident = self.read_ident()
        case ident:
        of "fn":
            Token(kind: tkFunction)
        of "let":
            Token(kind: tkLet)
        else:
            Token(kind: tkIdent)
    of '0'..'9':
        Token(kind: tkInt, value: self.read_int())
    of '\0':
        Token(kind: tkEof)
    else:
        raise newException(ValueError, "Unexpected character")

    self.read_char()
    return tok