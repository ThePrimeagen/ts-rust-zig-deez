interface Parser
    exposes [parse, debugPrint, Node]
    imports [Lexer]

Program : List Node

Node : [
    Fn { params : List Str, body : Node },
    If { cond : Node, consequence : Node },
    IfElse { cond : Node, consequence : Node, alternative : Node },
    Block (List Node),
    Call { fn : Node, args : List Node },
    Let { ident : Str, expr : Node },
    Return Node,
    Ident Str,
    Int I64,
    True,
    False,
    Not Node,
    Negate Node,
    Plus Node Node,
    Minus Node Node,
    Eq Node Node,
    NotEq Node Node,
    Lt Node Node,
    Gt Node Node,
    Product Node Node,
    Div Node Node,
]

Precedence : U8

precLowest = 1
precEquals = 2
precLessGreater = 3
precSum = 4
precProduct = 5
precPrefix = 6
precCall = 7

Errors : List Str

Parser : {
    remainingTokens : List Lexer.Token,
    errors : Errors,
}

advanceTokens : Parser, Nat -> Parser
advanceTokens = \{ remainingTokens, errors }, n ->
    { remainingTokens: List.drop remainingTokens n, errors }

addError : Parser, Str -> Parser
addError = \{ remainingTokens, errors }, error ->
    { remainingTokens, errors: List.append errors error }

parse : List Lexer.Token -> Result Program Errors
parse = \tokens ->
    parser = {
        remainingTokens: tokens,
        errors: [],
    }
    program = List.withCapacity 128

    parseProgram parser program

parseProgram : Parser, Program -> Result Program Errors
parseProgram = \p0, program ->
    when List.first p0.remainingTokens is
        Ok Eof | Err _ ->
            if List.isEmpty p0.errors then
                Ok program
            else
                Err p0.errors

        Ok token ->
            when parseStatement p0 token is
                Ok (p1, statement) ->
                    parseProgram p1 (List.append program statement)

                Err p1 ->
                    # Even on error, continue parsing
                    parseProgram p1 program

parseStatement : Parser, Lexer.Token -> Result (Parser, Node) Parser
parseStatement = \p0, token ->
    when token is
        Let ->
            parseLetStatement (advanceTokens p0 1)

        Return ->
            parseReturnStatement (advanceTokens p0 1)

        _ ->
            parseExpressionStatement p0

parseLetStatement : Parser -> Result (Parser, Node) Parser
parseLetStatement = \p0 ->
    when p0.remainingTokens is
        [Ident ident, Assign, ..] ->
            (p1, expr) <- parseExpression (advanceTokens p0 2) precLowest |> Result.map
            (consumeOptionalSemicolon p1, Let { ident, expr })

        [Ident _, token, ..] ->
            p0
            |> advanceTokens 1
            |> tokenMismatch "Assign" token
            |> Err

        [token, ..] ->
            p0
            |> tokenMismatch "Ident" token
            |> Err

        [] ->
            Err (unexpectedEof p0)

parseReturnStatement : Parser -> Result (Parser, Node) Parser
parseReturnStatement = \p0 ->
    (p1, expr) <- parseExpression p0 precLowest |> Result.map
    (consumeOptionalSemicolon p1, Return expr)

tokenMismatch : Parser, Str, Lexer.Token -> Parser
tokenMismatch = \p0, wanted, got ->
    debugStr = Lexer.debugPrintToken got
    addError p0 "Expected next token to be \(wanted), instead got: \(debugStr)"

unexpectedEof : Parser -> Parser
unexpectedEof = \p0 ->
    addError p0 "Unexpectly ran out of tokens and hit Eof"

parseExpressionStatement : Parser -> Result (Parser, Node) Parser
parseExpressionStatement = \p0 ->
    (p1, expr) <- parseExpression p0 precLowest |> Result.map
    (consumeOptionalSemicolon p1, expr)

parseExpression : Parser, Precedence -> Result (Parser, Node) Parser
parseExpression = \p0, basePrec ->
    (p1, lhs) <- parsePrefix p0 |> Result.try
    parseInfix p1 lhs basePrec

parsePrefix : Parser -> Result (Parser, Node) Parser
parsePrefix = \p0 ->
    when List.first p0.remainingTokens is
        Ok (Ident ident) ->
            Ok (advanceTokens p0 1, Ident ident)

        Ok (Int int) ->
            Ok (advanceTokens p0 1, Int int)

        Ok Bang ->
            (p1, expr) <- parseExpression (advanceTokens p0 1) precPrefix |> Result.map
            (p1, Not expr)

        Ok Minus ->
            (p1, expr) <- parseExpression (advanceTokens p0 1) precPrefix |> Result.map
            (p1, Negate expr)

        Ok LParen ->
            (p1, expr) <- parseExpression (advanceTokens p0 1) precLowest |> Result.try
            when List.first p1.remainingTokens is
                Ok RParen ->
                    Ok (advanceTokens p1 1, expr)

                Ok token ->
                    p1
                    |> advanceTokens 1
                    |> tokenMismatch "RParen" token
                    |> Err

                Err _ ->
                    Err (unexpectedEof p1)

        Ok True ->
            Ok (advanceTokens p0 1, True)

        Ok False ->
            Ok (advanceTokens p0 1, False)

        Ok If ->
            parseIfExpression (advanceTokens p0 1)

        Ok Function ->
            parseFnExpression (advanceTokens p0 1)

        Ok token ->
            debugStr = Lexer.debugPrintToken token

            p0
            |> advanceTokens 1
            |> addError "no prefix parse function for \(debugStr) found"
            |> Err

        Err _ ->
            Err (unexpectedEof p0)

parseInfix : Parser, Node, Precedence -> Result (Parser, Node) Parser
parseInfix = \p0, lhs, basePrec ->
    nextPrec = peekPrecedence p0
    if basePrec >= nextPrec then
        Ok (p0, lhs)
    else
        when List.first p0.remainingTokens is
            Ok LParen ->
                (p1, args) <- parseCallArgs (advanceTokens p0 1) |> Result.try
                parseInfix p1 (Call { fn: lhs, args }) basePrec

            Ok token ->
                # parse standard infix
                binOpRes =
                    when token is
                        Eq -> Ok Eq
                        NotEq -> Ok NotEq
                        Lt -> Ok Lt
                        Gt -> Ok Gt
                        Plus -> Ok Plus
                        Minus -> Ok Minus
                        Asterisk -> Ok Product
                        Slash -> Ok Div
                        _ -> Err NotInfix

                when binOpRes is
                    Ok binOp ->
                        (p1, rhs) <- parseExpression (advanceTokens p0 1) nextPrec |> Result.try
                        parseInfix p1 (binOp lhs rhs) basePrec

                    Err NotInfix ->
                        Ok (p0, lhs)

            Err _ ->
                Err (unexpectedEof p0)

peekPrecedence : Parser -> Precedence
peekPrecedence = \p0 ->
    when List.first p0.remainingTokens is
        Ok Eq -> precEquals
        Ok NotEq -> precEquals
        Ok Gt -> precLessGreater
        Ok Lt -> precLessGreater
        Ok Plus -> precSum
        Ok Minus -> precSum
        Ok Asterisk -> precProduct
        Ok Slash -> precProduct
        Ok LParen -> precCall
        _ -> precLowest

parseCallArgs : Parser -> Result (Parser, List Node) Parser
parseCallArgs = \p0 ->
    when List.first p0.remainingTokens is
        Ok RParen ->
            Ok (p0, [])

        Ok _ ->
            (p1, arg) <- parseExpression p0 precLowest |> Result.try
            parseCallArgsHelper p1 [arg]

        Err _ ->
            Err (unexpectedEof p0)

parseCallArgsHelper : Parser, List Node -> Result (Parser, List Node) Parser
parseCallArgsHelper = \p0, args ->
    when List.first p0.remainingTokens is
        Ok RParen ->
            Ok (advanceTokens p0 1, args)

        Ok Comma ->
            (p1, arg) <- parseExpression (advanceTokens p0 1) precLowest |> Result.try
            parseCallArgsHelper p1 (List.append args arg)

        Ok token ->
            p0
            |> tokenMismatch "RParen or Comma" token
            |> Err

        Err _ ->
            Err (unexpectedEof p0)

parseFnExpression : Parser -> Result (Parser, Node) Parser
parseFnExpression = \p0 ->
    when List.first p0.remainingTokens is
        Ok LParen ->
            (p1, params) <- parseFnParams (advanceTokens p0 1) [] |> Result.try
            when List.first p1.remainingTokens is
                Ok LBrace ->
                    (p2, body) <- parseBlock (advanceTokens p1 1) [] |> Result.map
                    (p2, Fn { params, body })

                Ok token ->
                    p0
                    |> tokenMismatch "LBrace" token
                    |> Err

                Err _ ->
                    Err (unexpectedEof p1)

        Ok token ->
            p0
            |> tokenMismatch "LParen" token
            |> Err

        Err _ ->
            Err (unexpectedEof p0)

parseFnParams : Parser, List Str -> Result (Parser, List Str) Parser
parseFnParams = \p0, params ->
    when p0.remainingTokens is
        [Ident param, Comma, ..] ->
            parseFnParams (advanceTokens p0 2) (List.append params param)

        [Ident param, RParen, ..] ->
            Ok (advanceTokens p0 2, List.append params param)

        [RParen, ..] ->
            Ok (advanceTokens p0 1, params)

        [token, ..] ->
            p0
            |> tokenMismatch "Ident" token
            |> Err

        [] ->
            Err (unexpectedEof p0)

parseIfExpression : Parser -> Result (Parser, Node) Parser
parseIfExpression = \p0 ->
    when List.first p0.remainingTokens is
        Ok LParen ->
            (p1, cond) <- parseExpression (advanceTokens p0 1) precLowest |> Result.try
            when p1.remainingTokens is
                [RParen, LBrace, ..] ->
                    (p2, consequence) <- parseBlock (advanceTokens p1 2) [] |> Result.try
                    when p2.remainingTokens is
                        [Else, LBrace, ..] ->
                            (p3, alternative) <- parseBlock (advanceTokens p2 2) [] |> Result.map
                            (p3, IfElse { cond, consequence, alternative })

                        [_, ..] ->
                            Ok (p2, If { cond, consequence })

                        [] ->
                            Err (unexpectedEof p2)

                [RParen, token, ..] ->
                    p1
                    |> advanceTokens 1
                    |> tokenMismatch "LBrace" token
                    |> Err

                [token, ..] ->
                    p1
                    |> tokenMismatch "RParen" token
                    |> Err

                [] ->
                    Err (unexpectedEof p1)

        Ok token ->
            p0
            |> tokenMismatch "LParen" token
            |> Err

        Err _ ->
            Err (unexpectedEof p0)

parseBlock : Parser, List Node -> Result (Parser, Node) Parser
parseBlock = \p0, statements ->
    when List.first p0.remainingTokens is
        Ok RBrace ->
            Ok (advanceTokens p0 1, Block statements)

        Ok token ->
            when parseStatement p0 token is
                Ok (p1, statement) ->
                    parseBlock p1 (List.append statements statement)

                Err p1 ->
                    # Even on error, continue parsing
                    parseBlock p1 statements

        Err _ ->
            Err (unexpectedEof p0)

consumeOptionalSemicolon : Parser -> Parser
consumeOptionalSemicolon = \p0 ->
    when List.first p0.remainingTokens is
        Ok Semicolon ->
            advanceTokens p0 1

        _ ->
            p0

debugPrint : Program -> Str
debugPrint = \program ->
    debugPrintIndented program ""

debugPrintIndented : Program, Str -> Str
debugPrintIndented = \program, spaces ->
    List.walk program "" \buf, node ->
        Str.concat buf (debugPrintNodeStatement node spaces)

debugPrintNodeStatement : Node, Str -> Str
debugPrintNodeStatement = \node, spaces ->
    debugStr = debugPrintNode node spaces
    "\(spaces)\(debugStr);\n"

debugPrintNode : Node, Str -> Str
debugPrintNode = \node, spaces ->
    when node is
        Call { fn, args } ->
            fnStr = debugPrintNode fn spaces
            argsStr =
                args
                |> List.map \arg -> debugPrintNode arg spaces
                |> Str.joinWith ", "
            "\(fnStr)(\(argsStr))"

        Fn { params, body } ->
            paramsStr = Str.joinWith params ", "
            bodyStr = debugPrintNode body spaces
            "fn(\(paramsStr)) \(bodyStr)"

        If { cond, consequence } ->
            condStr = debugPrintNode cond spaces
            consStr = debugPrintNode consequence spaces
            "if \(condStr) \(consStr)"

        IfElse { cond, consequence, alternative } ->
            condStr = debugPrintNode cond spaces
            consStr = debugPrintNode consequence spaces
            altStr = debugPrintNode alternative spaces
            "if \(condStr) \(consStr) else \(altStr)"

        Block statements ->
            blockStr = debugPrintIndented statements (Str.concat spaces "    ")
            "{\n\(blockStr)\(spaces)}"

        Let { ident, expr } ->
            exprStr = debugPrintNode expr spaces
            "let \(ident) = \(exprStr)"

        Return expr ->
            exprStr = debugPrintNode expr spaces
            "return \(exprStr)"

        Ident ident ->
            ident

        True ->
            "true"

        False ->
            "false"

        Int int ->
            Num.toStr int

        Not expr ->
            exprStr = debugPrintNode expr spaces
            "(!\(exprStr))"

        Negate expr ->
            exprStr = debugPrintNode expr spaces
            "(-\(exprStr))"

        Eq lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) == \(rhsStr))"

        NotEq lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) != \(rhsStr))"

        Lt lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) < \(rhsStr))"

        Gt lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) > \(rhsStr))"

        Plus lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) + \(rhsStr))"

        Minus lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) - \(rhsStr))"

        Product lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) * \(rhsStr))"

        Div lhs rhs ->
            lhsStr = debugPrintNode lhs spaces
            rhsStr = debugPrintNode rhs spaces
            "(\(lhsStr) / \(rhsStr))"

expect
    input = Str.toUtf8
        """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """
    parseRes =
        Lexer.lex input
        |> parse

    identNames =
        parsed <- Result.map parseRes
        parsed
        |> List.map \letNode ->
            when letNode is
                Let { ident } -> ident
                _ -> crash "all statements in program should be let statements"

    expected = Ok ["x", "y", "foobar"]

    identNames == expected

expect
    input = Str.toUtf8
        """
        let x 5;
        let = 10;
        let 838383;
        """
    parseRes =
        Lexer.lex input
        |> parse

    expected = Err [
        "Expected next token to be Assign, instead got: Int 5",
        "Expected next token to be Ident, instead got: Assign",
        "no prefix parse function for Assign found",
        "Expected next token to be Ident, instead got: Int 838383",
    ]
    parseRes == expected

expect
    input = Str.toUtf8
        """
        return 5;
        return 10;
        return 993322;
        """
    parseRes =
        Lexer.lex input
        |> parse

    expected = Ok [
        Return (Int 5),
        Return (Int 10),
        Return (Int 993322),
    ]
    parseRes == expected

formatedOutput = \input ->
    input
    |> Str.toUtf8
    |> Lexer.lex
    |> parse
    |> Result.map \parsed -> debugPrint parsed

expect
    input =
        """
        let x = 5;
        let y = x;
        return 838383;

        """
    out = formatedOutput input

    expected = Ok input
    out == expected

expect
    input =
        """
        !5;
        -15;

        """
    out = formatedOutput input

    expected = Ok
        """
        (!5);
        (-15);

        """
    out == expected

expect
    input =
        """
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;

        """
    out = formatedOutput input

    expected = Ok
        """
        (5 + 5);
        (5 - 5);
        (5 * 5);
        (5 / 5);
        (5 > 5);
        (5 < 5);
        (5 == 5);
        (5 != 5);

        """
    out == expected

expect
    input =
        """
        -a * b
        !-a
        a + b + c
        a + b - c
        a * b * c
        a * b / c
        a + b / c
        a + b * c + d / e - f
        3 + 4; -5 * 5
        5 > 4 == 3 < 4
        5 < 4 != 3 > 4
        3 + 4 * 5 == 3 * 1 + 4 * 5
        """
    out = formatedOutput input

    expected = Ok
        """
        ((-a) * b);
        (!(-a));
        ((a + b) + c);
        ((a + b) - c);
        ((a * b) * c);
        ((a * b) / c);
        (a + (b / c));
        (((a + (b * c)) + (d / e)) - f);
        (3 + 4);
        ((-5) * 5);
        ((5 > 4) == (3 < 4));
        ((5 < 4) != (3 > 4));
        ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));

        """
    out == expected

expect
    input =
        """
        true;
        false;
        3 > 5 == false
        3 < 5 == true
        """
    out = formatedOutput input

    expected = Ok
        """
        true;
        false;
        ((3 > 5) == false);
        ((3 < 5) == true);

        """
    out == expected

expect
    input =
        """
        1 + (2 + 3) + 4;
        (5 + 5) * 2
        2 / (5 + 5);
        -(5 + 5)
        !(true == true)
        """
    out = formatedOutput input

    expected = Ok
        """
        ((1 + (2 + 3)) + 4);
        ((5 + 5) * 2);
        (2 / (5 + 5));
        (-(5 + 5));
        (!(true == true));

        """
    out == expected

expect
    input =
        "if (x < y) { x }"
    out = formatedOutput input

    expected = Ok
        """
        if (x < y) {
            x;
        };

        """
    out == expected

expect
    input =
        "if (x < y) { x } else { y }"
    out = formatedOutput input

    expected = Ok
        """
        if (x < y) {
            x;
        } else {
            y;
        };

        """
    out == expected

expect
    input =
        """
        fn() { }
        fn(x) { x }
        fn(x, y, z) { x + y + z }
        """
    out = formatedOutput input

    expected = Ok
        """
        fn() {
        };
        fn(x) {
            x;
        };
        fn(x, y, z) {
            ((x + y) + z);
        };

        """
    out == expected

expect
    input =
        """
        a + add(b * c) + d
        add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))
        add(a + b + c * d / f + g)
        """
    out = formatedOutput input

    expected = Ok
        """
        ((a + add((b * c))) + d);
        add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));
        add((((a + b) + ((c * d) / f)) + g));

        """
    out == expected

