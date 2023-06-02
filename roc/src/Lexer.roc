interface Lexer
    exposes [lex, debugPrint, debugPrintToken, Token]
    imports []

Token : [
    Illegal,
    Eof,
    Ident Str,
    Int I64,
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

lex : List U8 -> List Token
lex = \bytes ->
    helper = \state ->
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
                { tokens: state.tokens, remaining: nextRemaining } |> helper

            [ch, ..] if isLetter ch ->
                len = identLength state.remaining 1
                ident = getIdent state.remaining len
                consumeToken state ident len |> helper

            [ch, ..] if isDigit ch ->
                len = intLength state.remaining 1
                int = getInt state.remaining len
                consumeToken state int len |> helper

            [_, ..] ->
                consumeToken state Illegal 1 |> helper

            [] ->
                List.append state.tokens Eof

    helper { tokens: List.withCapacity 1024, remaining: bytes }

consumeToken = \{ tokens, remaining }, tok, size ->
    nextTokens = List.append tokens tok
    nextRemaining = List.drop remaining (Num.toNat size)
    { tokens: nextTokens, remaining: nextRemaining }

isLetter = \ch ->
    (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_'

isDigit = \ch ->
    ch >= '0' && ch <= '9'

getIdent = \bytes, len ->
    List.takeFirst bytes len
    |> Str.fromUtf8
    |> okOrUnreachable
    |> Str.releaseExcessCapacity
    |> Ident

getInt = \bytes, len ->
    List.takeFirst bytes len
    |> Str.fromUtf8
    |> Result.try Str.toI64
    |> okOrUnreachable
    |> Int

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

okOrUnreachable = \res ->
    when res is
        Ok v -> v
        Err _ -> crash "unreachable"

debugPrint : List Token -> Str
debugPrint = \tokens ->
    List.walk tokens "" \buf, token ->
        tokenStr = debugPrintToken token
        Str.concat buf "\(tokenStr)\n"

debugPrintToken : Token -> Str
debugPrintToken = \token ->
    when token is
        Illegal -> "Illegal"
        Eof -> "Eof"
        Ident ident -> "Ident \(ident)"
        Int int ->
            intStr = Num.toStr int
            "Int \(intStr)"

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

expect
    lexed = lex (Str.toUtf8 "")
    expected = [Eof]

    lexed == expected

expect
    lexed = lex (Str.toUtf8 "=+(){},;")
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

    tokens = lex bytes
    expected = [
        Let,
        Ident "five",
        Assign,
        Int 5,
        Semicolon,
        Let,
        Ident "ten",
        Assign,
        Int 10,
        Semicolon,
        Let,
        Ident "add",
        Assign,
        Function,
        LParen,
        Ident "x",
        Comma,
        Ident "y",
        RParen,
        LBrace,
        Ident "x",
        Plus,
        Ident "y",
        Semicolon,
        RBrace,
        Semicolon,
        Let,
        Ident "result",
        Assign,
        Ident "add",
        LParen,
        Ident "five",
        Comma,
        Ident "ten",
        RParen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int 5,
        Semicolon,
        Int 5,
        Lt,
        Int 10,
        Gt,
        Int 5,
        Semicolon,
        If,
        LParen,
        Int 5,
        Lt,
        Int 10,
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
        Int 10,
        Eq,
        Int 10,
        Semicolon,
        Int 10,
        NotEq,
        Int 9,
        Semicolon,
        Eof,
    ]

    tokens == expected
