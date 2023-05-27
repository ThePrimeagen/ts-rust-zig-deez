interface Lexer
    exposes [lex, debugPrint, debugPrintToken, Token, Kind, getInt, getIdent, LexedData]
    imports []

LexedData : {
    bytes : List U8,
    tokens : List Token,
}

Kind : [
    Illegal,
    Eof,
    Ident,
    Int,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Eq,
    NotEq,
]

Token : {
    kind : Kind,
    index : U32,
}

lex : List U8 -> LexedData
lex = \bytes ->
    helper = \state ->
        # TODO: Investigate the perf of this compared to a state machine based parser.
        # This is a branching prefix tree.
        when state.remaining is
            ['r', 'e', 't', 'u', 'r', 'n', ..] ->
                consumeToken state Return 6 |> helper

            ['f', 'a', 'l', 's', 'e', ..] ->
                consumeToken state False 5 |> helper

            ['t', 'r', 'u', 'e', ..] ->
                consumeToken state True 4 |> helper

            ['e', 'l', 's', 'e', ..] ->
                consumeToken state Else 4 |> helper

            ['l', 'e', 't', ..] ->
                consumeToken state Let 3 |> helper

            ['f', 'n', ..] ->
                consumeToken state Function 2 |> helper

            ['i', 'f', ..] ->
                consumeToken state If 2 |> helper

            ['=', '=', ..] ->
                consumeToken state Eq 2 |> helper

            ['!', '=', ..] ->
                consumeToken state NotEq 2 |> helper

            ['(', ..] ->
                consumeToken state LParen 1 |> helper

            [')', ..] ->
                consumeToken state RParen 1 |> helper

            ['{', ..] ->
                consumeToken state LBrace 1 |> helper

            ['}', ..] ->
                consumeToken state RBrace 1 |> helper

            ['=', ..] ->
                consumeToken state Assign 1 |> helper

            ['+', ..] ->
                consumeToken state Plus 1 |> helper

            ['-', ..] ->
                consumeToken state Minus 1 |> helper

            ['!', ..] ->
                consumeToken state Bang 1 |> helper

            ['*', ..] ->
                consumeToken state Asterisk 1 |> helper

            ['/', ..] ->
                consumeToken state Slash 1 |> helper

            ['<', ..] ->
                consumeToken state Lt 1 |> helper

            ['>', ..] ->
                consumeToken state Gt 1 |> helper

            [',', ..] ->
                consumeToken state Comma 1 |> helper

            [';', ..] ->
                consumeToken state Semicolon 1 |> helper

            ['\r', ..] | ['\n', ..] | ['\t', ..] | [' ', ..] ->
                nextRemaining = List.drop state.remaining 1
                nextIndex = state.index + 1
                { tokens: state.tokens, remaining: nextRemaining, index: nextIndex } |> helper

            [ch, ..] if isLetter ch ->
                consumeToken state Ident (identLength state.remaining 1) |> helper

            [ch, ..] if isDigit ch ->
                consumeToken state Int (intLength state.remaining 1) |> helper

            [_, ..] ->
                consumeToken state Illegal 1 |> helper

            [] ->
                List.append state.tokens { kind: Eof, index: state.index }

    tokens = helper { tokens: List.withCapacity 1024, remaining: bytes, index: 0 }
    { bytes, tokens }

consumeToken = \{ tokens, remaining, index }, kind, size ->
    nextTokens = List.append tokens { kind, index }
    nextRemaining = List.drop remaining (Num.toNat size)
    nextIndex = index + size
    { tokens: nextTokens, remaining: nextRemaining, index: nextIndex }

identLength = \remaining, size ->
    when List.get remaining (Num.toNat size) is
        Ok ch if isLetter ch ->
            identLength remaining (size + 1)

        _ ->
            size

intLength = \remaining, size ->
    when List.get remaining (Num.toNat size) is
        Ok ch if isDigit ch ->
            intLength remaining (size + 1)

        _ ->
            size

isLetter = \ch ->
    (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_'

isDigit = \ch ->
    ch >= '0' && ch <= '9'

getIdent = \bytes, index ->
    len = identLength (List.drop bytes (Num.toNat index)) 1
    List.sublist bytes { start: Num.toNat index, len }

getInt = \bytes, index ->
    len = intLength (List.drop bytes (Num.toNat index)) 1
    List.sublist bytes { start: Num.toNat index, len }

debugPrint : List U8, LexedData -> List U8
debugPrint = \buf, { bytes, tokens } ->
    List.walk tokens buf \b, token ->
        debugPrintToken b bytes token
        |> List.append '\n'

debugPrintToken : List U8, List U8, Token -> List U8
debugPrintToken = \buf, bytes, token ->
    buf
    |> List.concat (Str.toUtf8 "{ kind: ")
    |> debugPrintKind token.kind
    |> \b ->
        when token.kind is
            Ident ->
                b
                |> List.concat (Str.toUtf8 ", value: ")
                |> List.concat (getIdent bytes token.index)

            Int ->
                b
                |> List.concat (Str.toUtf8 ", value: ")
                |> List.concat (getInt bytes token.index)

            _ ->
                b
    |> List.concat (Str.toUtf8 " }")

debugPrintKind : List U8, Kind -> List U8
debugPrintKind = \buf, kind ->
    out =
        when kind is
            Illegal -> "Illegal"
            Eof -> "Eof"
            Ident -> "Ident"
            Int -> "Int"
            Assign -> "Assign"
            Plus -> "Plus"
            Minus -> "Minus"
            Bang -> "Bang"
            Asterisk -> "Asterisk"
            Slash -> "Slash"
            Lt -> "Lt"
            Gt -> "Gt"
            Comma -> "Comma"
            Semicolon -> "Semicolon"
            LParen -> "LParen"
            RParen -> "RParen"
            LBrace -> "LBrace"
            RBrace -> "RBrace"
            Function -> "Function"
            Let -> "Let"
            True -> "True"
            False -> "False"
            If -> "If"
            Else -> "Else"
            Return -> "Return"
            Eq -> "Eq"
            NotEq -> "NotEq"

    List.concat buf (Str.toUtf8 out)

expect
    lexed = lex (Str.toUtf8 "") |> .tokens |> List.map .kind
    expected = [Eof]

    lexed == expected

expect
    lexed = lex (Str.toUtf8 "=+(){},;") |> .tokens |> List.map .kind
    expected = [
        Assign,
        Plus,
        LParen,
        RParen,
        LBrace,
        RBrace,
        Comma,
        Semicolon,
        Eof,
    ]

    lexed == expected

expect
    # This is a horrid and overly gigantic test.
    # Just followed what is in the book.
    # Probably should write a test like this with by reprinting to source.
    # That would be a lot less painful to update.
    bytes =
        Str.toUtf8
            """
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            """

    lexed = lex bytes |> .tokens
    kinds = List.map lexed .kind
    expectedKinds = [
        Let,
        Ident,
        Assign,
        Int,
        Semicolon,
        Let,
        Ident,
        Assign,
        Int,
        Semicolon,
        Let,
        Ident,
        Assign,
        Function,
        LParen,
        Ident,
        Comma,
        Ident,
        RParen,
        LBrace,
        Ident,
        Plus,
        Ident,
        Semicolon,
        RBrace,
        Semicolon,
        Let,
        Ident,
        Assign,
        Ident,
        LParen,
        Ident,
        Comma,
        Ident,
        RParen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int,
        Semicolon,
        Int,
        Lt,
        Int,
        Gt,
        Int,
        Semicolon,
        If,
        LParen,
        Int,
        Lt,
        Int,
        RParen,
        LBrace,
        Return,
        True,
        Semicolon,
        RBrace,
        Else,
        LBrace,
        Return,
        False,
        Semicolon,
        RBrace,
        Int,
        Eq,
        Int,
        Semicolon,
        Int,
        NotEq,
        Int,
        Semicolon,
        Eof,
    ]

    idents =
        lexed
        |> List.keepIf \{ kind } -> kind == Ident
        |> List.map \{ index } -> getIdent bytes index

    expectedIdents = [
        Str.toUtf8 "five",
        Str.toUtf8 "ten",
        Str.toUtf8 "add",
        Str.toUtf8 "x",
        Str.toUtf8 "y",
        Str.toUtf8 "x",
        Str.toUtf8 "y",
        Str.toUtf8 "result",
        Str.toUtf8 "add",
        Str.toUtf8 "five",
        Str.toUtf8 "ten",
    ]

    ints =
        lexed
        |> List.keepIf \{ kind } -> kind == Int
        |> List.map \{ index } -> getInt bytes index

    expectedInts = [
        Str.toUtf8 "5",
        Str.toUtf8 "10",
        Str.toUtf8 "5",
        Str.toUtf8 "5",
        Str.toUtf8 "10",
        Str.toUtf8 "5",
        Str.toUtf8 "5",
        Str.toUtf8 "10",
        Str.toUtf8 "10",
        Str.toUtf8 "10",
        Str.toUtf8 "10",
        Str.toUtf8 "9",
    ]

    kinds == expectedKinds && idents == expectedIdents && ints == expectedInts
