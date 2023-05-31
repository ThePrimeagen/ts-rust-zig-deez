import sequtils, strutils

# lantos waz here
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
        tkLParen,
        tkRParen,
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


proc `==`*(lhs, rhs: Token): bool = 
    result = false
    if lhs.kind == rhs.kind:
        result = true
    if lhs.kind in { tkIdent, tkInt } and rhs.kind in {tkIdent, tkInt}:
        if lhs.value == rhs.value:
            result = true

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
    self.read_position.inc()

proc skip_whitespace(self: var Lexer) =
    while self.ch in WhiteSpace:
        self.read_char()

proc read_ident*(self: var Lexer): string =
    let start_pos = self.position
    while self.ch.isAlphaAscii or self.ch == '_':
        self.read_char()
    return self.input[start_pos ..< self.position].join()

proc read_int*(self: var Lexer): string =
    let start_pos = self.position
    while self.ch.isDigit:
        self.read_char()
    return self.input[start_pos ..< self.position].join()

proc next_token*(self: var Lexer): Token =
    self.skip_whitespace()

    let token = case self.ch:
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
            Token(kind: tkIdent, value: ident)
    of '0'..'9':
        Token(kind: tkInt, value: self.read_int())
    of '\0':
        Token(kind: tkEof)
    else:
        raise newException(ValueError, "Unexpected character \"" & self.ch & "\" " & $self.ch.ord )
    
    self.read_char()
    return token

iterator tokens*(lexer: var Lexer): Token =
    while true:
        var token = lexer.next_token()
        yield token
        if token.kind in { tkEof, tkIllegal }:
            break
