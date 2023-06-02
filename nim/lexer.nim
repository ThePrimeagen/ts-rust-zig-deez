import token
import strformat
import strutils

proc isLetter(c: char): bool {.inline, noSideEffect.} = 
    c.isAlphaAscii() or c == '_'

type 
    Lexer = object
        input: string
        position: int
        readPosition: int
        ch: char

proc readChar(l: var Lexer) = 
    if l.readPosition >= l.input.len():
        l.ch = '\0'
    else:
        l.ch = l.input[l.readPosition]
    l.position = l.readPosition
    l.readPosition += 1

proc newLexer*(input: string): Lexer = 
    result = Lexer(input: input, position: 0, readPosition: 0, ch: '\0')
    result.readChar()

proc readIdent(l: var Lexer): string {.inline.} =
    let position = l.position
    while l.ch.isLetter():
        l.readChar()
    result = l.input[position..<l.position]

proc readNumber(l: var Lexer): string {.inline.} = 
    let position = l.position
    while l.ch.isDigit():
        l.readChar()
    result = l.input[position..<l.position]

proc skipWhitespace(l: var Lexer) {.inline.} = 
    while [' ', '\t', '\r', '\n'].contains(l.ch):
        l.readChar()

proc peekChar(l: var Lexer): char = 
    result = if l.readPosition >= l.input.len():
        '\0'
    else:
        l.input[l.readPosition]

proc nextToken*(l: var Lexer): Token =
    l.skipWhitespace()
    case l.ch:
    of '=': result = if l.peekChar() == '=':
        l.readChar()
        Token(ty: ttEql)
    else:
        Token(ty: ttAssign)
    of ';': result = Token(ty: ttSemicolon)
    of '(': result = Token(ty: ttLparen)
    of ')': result = Token(ty: ttRparen)
    of ',': result = Token(ty: ttComma)
    of '+': result = Token(ty: ttPlus)
    of '-': result = Token(ty: ttMinus)
    of '*': result = Token(ty: ttAsterisk)
    of '/': result = Token(ty: ttSlash)
    of '!': result = if l.peekChar() == '=':
        l.readChar()
        Token(ty: ttNeql)
    else:
        Token(ty: ttBang)
    of '<': result = Token(ty: ttLt)
    of '>': result = Token(ty: ttGt)
    of '{': result = Token(ty: ttLbrace)
    of '}': result = Token(ty: ttRbrace)
    of '\0': result = Token(ty: ttEof)
    else: 
        if l.ch.isLetter():
            let lit = l.readIdent()
            let ty = lookupIdent(lit)
            case ty:
                of ttIdent: return Token(ty: ttIdent, lit: lit)
                else: return Token(ty: ty)
        elif l.ch.isDigit():
            let lit = l.readNumber()
            return Token(ty: ttInt, lit: lit)
        else:
            result = Token(ty: ttIllegal)

    l.readChar()




##############################################################################
### TESTS ####################################################################
##############################################################################
when isMainModule:
    proc testNextToken() = 
        let input = "=+(){},;"

        let expected = [
            Token(ty: ttAssign), 
            Token(ty: ttPlus), 
            Token(ty: ttLparen), 
            Token(ty: ttRparen),
            Token(ty: ttLbrace),
            Token(ty: ttRbrace),
            Token(ty: ttComma),
            Token(ty: ttSemicolon),
        ]

        var l = newLexer(input)

        for i, expectedTok in expected.pairs():
            let tok = l.nextToken()
            if tok != expectedTok:
                raiseAssert(&"TestNextToken[{i}] - wrong ty. expected={$expectedTok}, got={$tok}")
        
        echo("TestNextToken: Passed")

    proc testNextToken2() = 
        let input = """let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y
};

let result = add(five, ten);"""

        let expected = [
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "five"),
            Token(ty: ttAssign),
            Token(ty: ttInt, lit: "5"),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "ten"),
            Token(ty: ttAssign),
            Token(ty: ttInt, lit: "10"),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "add"),
            Token(ty: ttAssign),
            Token(ty: ttFn),
            Token(ty: ttLparen),
            Token(ty: ttIdent, lit: "x"),
            Token(ty: ttComma),
            Token(ty: ttIdent, lit: "y"),
            Token(ty: ttRparen),
            Token(ty: ttLbrace),
            Token(ty: ttIdent, lit: "x"),
            Token(ty: ttPlus),
            Token(ty: ttIdent, lit: "y"),
            Token(ty: ttRbrace),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "result"),
            Token(ty: ttAssign),
            Token(ty: ttIdent, lit: "add"),
            Token(ty: ttLparen),
            Token(ty: ttIdent, lit:"five"),
            Token(ty: ttComma),
            Token(ty: ttIdent, lit: "ten"),
            Token(ty: ttRparen),
            Token(ty: ttSemicolon),
        ]

        var l = newLexer(input)

        for i, expectedTok in expected.pairs():
            let tok = l.nextToken()
            if tok != expectedTok:
                raiseAssert(&"TestNextToken2[{i}] - wrong ty. expected={$expectedTok}, got={$tok}")
        
        echo "testNextToken2: Passed"

    proc testNextToken3() = 
        let input = """let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y
};

let result = add(five, ten);

!-*5;
5<10>5;
if else return true false==!="""

        let expected = [
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "five"),
            Token(ty: ttAssign),
            Token(ty: ttInt, lit: "5"),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "ten"),
            Token(ty: ttAssign),
            Token(ty: ttInt, lit: "10"),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "add"),
            Token(ty: ttAssign),
            Token(ty: ttFn),
            Token(ty: ttLparen),
            Token(ty: ttIdent, lit: "x"),
            Token(ty: ttComma),
            Token(ty: ttIdent, lit: "y"),
            Token(ty: ttRparen),
            Token(ty: ttLbrace),
            Token(ty: ttIdent, lit: "x"),
            Token(ty: ttPlus),
            Token(ty: ttIdent, lit: "y"),
            Token(ty: ttRbrace),
            Token(ty: ttSemicolon),
            Token(ty: ttLet),
            Token(ty: ttIdent, lit: "result"),
            Token(ty: ttAssign),
            Token(ty: ttIdent, lit: "add"),
            Token(ty: ttLparen),
            Token(ty: ttIdent, lit:"five"),
            Token(ty: ttComma),
            Token(ty: ttIdent, lit: "ten"),
            Token(ty: ttRparen),
            Token(ty: ttSemicolon),
            Token(ty: ttBang),
            Token(ty: ttMinus),
            Token(ty: ttAsterisk),
            Token(ty: ttInt, lit: "5"),
            Token(ty: ttSemicolon),
            Token(ty: ttInt, lit: "5"),
            Token(ty: ttLt),
            Token(ty: ttInt, lit: "10"),
            Token(ty: ttGt),
            Token(ty: ttInt, lit: "5"),
            Token(ty: ttSemicolon),
            Token(ty: ttIf),
            Token(ty: ttElse),
            Token(ty: ttReturn),
            Token(ty: ttTrue),
            Token(ty: ttFalse),
            Token(ty: ttEql),
            Token(ty: ttNeql)
        ]

        var l = newLexer(input)

        for i, expectedTok in expected.pairs():
            let tok = l.nextToken()
            if tok != expectedTok:
                raiseAssert(&"TestNextToken2[{i}] - wrong ty. expected={$expectedTok}, got={$tok}")
        
        echo("TestNextToken3: Passed")

    testNextToken()
    testNextToken2()
    testNextToken3()