interface Eval
    exposes [eval, evalWithEnvs, newEnv, printValue]
    imports [Lexer, Parser.{ Program, Node }]

Value : [
    Int I64,
    True,
    False,
    Null,

    # This is a bit strange and I am still looking for a better way to do it.
    # Since Roc is fully immutable and we need to reference a mutable environment,
    # we need to use an array index.
    # There is probably a better way to do this.
    Fn { params : List Str, body : Node, envIndex : Nat },

    # Values that need to be propogated up and returned.
    RetInt I64,
    RetTrue,
    RetFalse,
    RetNull,

    # Errors are just strings.
    Err Str,
]

valueToType : Value -> Str
valueToType = \val ->
    when val is
        Int _ -> "INTEGER"
        True | False -> "BOOLEAN"
        Null -> "NULL"
        Fn _ -> "FUNCTION"
        _ -> crash "value has no type"

boolToValue : Bool -> Value
boolToValue = \bool ->
    if bool then
        True
    else
        False

printValue : Value -> Str
printValue = \value ->
    when value is
        Int int -> Num.toStr int
        True -> "true"
        False -> "false"
        Null -> "null"
        Fn _ -> "<function>"
        Err e -> "ERROR: \(e)"
        _ -> "Invalid ret value"

Env : {
    rc : U32,
    inner : Dict Str Value,
    outer : Result Nat [IsRoot],
}

newEnv : {} -> Env
newEnv = \{} -> { rc: 1, inner: Dict.empty {}, outer: Err IsRoot }

Evaluator : {
    envs : List Env,
    currentEnv : Nat,
}

setVar : Evaluator, Str, Value -> (Evaluator, Value)
setVar = \{ envs: envs0, currentEnv }, ident, val ->
    { list: envs1, value: { rc, inner, outer } } = List.replace envs0 currentEnv (newEnv {})
    nextInner = Dict.insert inner ident val
    ({ currentEnv, envs: List.set envs1 currentEnv { rc, inner: nextInner, outer } }, val)

getVar : List Env, Nat, Str -> Value
getVar = \envs, currentEnv, ident ->
    { inner, outer } = List.get envs currentEnv |> okOrUnreachable "bad env index"
    when Dict.get inner ident is
        Ok v -> v
        Err _ ->
            when outer is
                Ok envIndex ->
                    getVar envs envIndex ident

                Err _ ->
                    Err "identifier not found: \(ident)"

incEnv : Evaluator, Nat -> Evaluator
incEnv = \{ envs, currentEnv }, i ->
    { rc, inner, outer } = List.get envs i |> okOrUnreachable "bad env index"
    when outer is
        Ok nextI ->
            nextRc = Num.addSaturated rc 1
            nextEnvs = List.set envs (Num.toNat i) { rc: nextRc, inner, outer: Ok nextI }
            { envs: nextEnvs, currentEnv }
            |> incEnv nextI

        _ ->
            nextRc = Num.addSaturated rc 1
            nextEnvs = List.set envs (Num.toNat i) { rc: nextRc, inner, outer: Err IsRoot }
            { envs: nextEnvs, currentEnv }

decEnv : Evaluator, Nat -> Evaluator
decEnv = \{ envs, currentEnv }, i ->
    { rc, inner, outer } = List.get envs i |> okOrUnreachable "bad env index"
    when outer is
        Ok nextI ->
            nextRc = Num.subSaturated rc 1
            nextEnvs = List.set envs (Num.toNat i) { rc: nextRc, inner, outer: Ok nextI }
            { envs: nextEnvs, currentEnv }
            |> decEnv nextI
            |> maybeFreeEnv i

        _ ->
            nextRc = Num.subSaturated rc 1
            nextEnvs = List.set envs (Num.toNat i) { rc: nextRc, inner, outer: Err IsRoot }
            { envs: nextEnvs, currentEnv }
            |> maybeFreeEnv i

maybeFreeEnv = \{ envs, currentEnv }, i ->
    when List.get envs (Num.toNat i) is
        Ok { rc: 0, inner } ->
            e0, _, val <- Dict.walk inner { envs, currentEnv }
            when val is
                Fn { envIndex } ->
                    e0
                    |> decEnv envIndex

                _ ->
                    e0

        _ ->
            { envs, currentEnv }

wrapAndSetEnv : Evaluator, Nat -> Evaluator
wrapAndSetEnv = \{ envs }, i ->
    # First pick unused envs
    when List.findLastIndex envs (\{ rc } -> rc == 0) is
        Ok newIndex ->
            nextEnvs = List.set envs newIndex { rc: 1, inner: Dict.empty {}, outer: Ok i }
            { envs: nextEnvs, currentEnv: newIndex }
            |> incEnv i

        Err _ ->
            newIndex = List.len envs
            nextEnvs = List.append envs { rc: 1, inner: Dict.empty {}, outer: Ok i }
            { envs: nextEnvs, currentEnv: newIndex }
            |> incEnv i

eval : Program -> (List Env, Value)
eval = \program ->
    evalWithEnvs program [newEnv {}]

evalWithEnvs : Program, List Env -> (List Env, Value)
evalWithEnvs = \program, envs ->
    e0 = { envs, currentEnv: 0 }
    ({ envs: outEnvs }, outVal) = evalProgram e0 program
    (outEnvs, outVal)

evalProgram : Evaluator, List Node -> (Evaluator, Value)
evalProgram = \e0, statements ->
    List.walkUntil statements (e0, Null) \(e1, _), node ->
        (e2, val) = evalNode e1 node
        when val is
            RetInt int -> Break (e2, Int int)
            RetTrue -> Break (e2, True)
            RetFalse -> Break (e2, False)
            RetNull -> Break (e2, Null)
            Err e -> Break (e2, Err e)
            _ -> Continue (e2, val)

evalBlock : Evaluator, List Node -> (Evaluator, Value)
evalBlock = \e0, statements ->
    List.walkUntil statements (e0, Null) \(e1, _), node ->
        (e2, val) = evalNode e1 node
        when val is
            RetInt int -> Break (e2, RetInt int)
            RetTrue -> Break (e2, RetTrue)
            RetFalse -> Break (e2, RetFalse)
            RetNull -> Break (e2, RetNull)
            Err e -> Break (e2, Err e)
            _ -> Continue (e2, val)

evalNode : Evaluator, Node -> (Evaluator, Value)
evalNode = \e0, node ->
    when node is
        Int int -> (e0, Int int)
        True -> (e0, True)
        False -> (e0, False)
        Not expr ->
            (e1, val) = evalNode e0 expr
            when val is
                True -> (e1, False)
                False -> (e1, True)
                Null -> (e1, True)
                Int _ -> (e1, False)
                Err e -> (e1, Err e)
                _ ->
                    type = valueToType val
                    (e1, Err "unknown operator: !\(type)")

        Negate expr ->
            (e1, val) = evalNode e0 expr
            when val is
                Int int -> (e1, Int -int)
                Err e -> (e1, Err e)
                _ ->
                    type = valueToType val
                    (e1, Err "unknown operator: -\(type)")

        Plus lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, Int (lhsInt + rhsInt))

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr "+" lhsVal rhsVal)

        Minus lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, Int (lhsInt - rhsInt))

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr "-" lhsVal rhsVal)

        Product lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, Int (lhsInt * rhsInt))

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr "*" lhsVal rhsVal)

        Div lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, Int (lhsInt // rhsInt))

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr "/" lhsVal rhsVal)

        Lt lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, (lhsInt < rhsInt) |> boolToValue)

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr "<" lhsVal rhsVal)

        Gt lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            when (lhsVal, rhsVal) is
                (Int lhsInt, Int rhsInt) ->
                    (e2, (lhsInt > rhsInt) |> boolToValue)

                (_, Err e) | (Err e, _) ->
                    (e2, Err e)

                _ ->
                    (e2, infixErr ">" lhsVal rhsVal)

        Eq lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            (e2, (lhsVal == rhsVal) |> boolToValue)

        NotEq lhs rhs ->
            (e1, lhsVal) = evalNode e0 lhs
            (e2, rhsVal) = evalNode e1 rhs
            (e2, (lhsVal != rhsVal) |> boolToValue)

        If { cond, consequence } ->
            (e1, condVal) = evalNode e0 cond
            when condVal is
                Err e -> (e1, Err e)
                _ ->
                    if isTruthy condVal then
                        evalNode e1 consequence
                    else
                        (e1, Null)

        IfElse { cond, consequence, alternative } ->
            (e1, condVal) = evalNode e0 cond
            when condVal is
                Err e -> (e1, Err e)
                _ ->
                    if isTruthy condVal then
                        evalNode e1 consequence
                    else
                        evalNode e1 alternative

        Block statements ->
            evalBlock e0 statements

        Return expr ->
            (e1, exprVal) = evalNode e0 expr
            when exprVal is
                Int int -> (e1, RetInt int)
                True -> (e1, RetTrue)
                False -> (e1, RetFalse)
                Null -> (e1, RetNull)
                Err e -> (e1, Err e)
                _ -> (e1, Null)

        Let { ident, expr } ->
            (e1, exprVal) = evalNode e0 expr
            when exprVal is
                Err e -> (e1, Err e)
                _ -> setVar e1 ident exprVal

        Ident ident ->
            (e0, getVar e0.envs e0.currentEnv ident)

        Fn { params, body } ->
            e1 = incEnv e0 e0.currentEnv
            (e1, Fn { params, body, envIndex: e1.currentEnv })

        Call { fn, args } ->
            (e1, fnVal) = evalNode e0 fn
            (e2, argVals) = evalArgs e1 args
            when (argVals, fnVal) is
                ([Err e], _) -> (e2, Err e)
                (_, Err e) -> (e2, Err e)
                (_, Fn { params, body: Block body, envIndex }) ->
                    if List.len params == List.len argVals then
                        oldIndex = e2.currentEnv
                        e3 = wrapAndSetEnv e2 envIndex
                        newIndex = e3.currentEnv

                        (e6, _) =
                            List.walk params (e3, 0) \(e4, i), param ->
                                (e5, _) =
                                    List.get argVals i
                                    |> okOrUnreachable "size checked"
                                    |> \argVal -> setVar e4 param argVal
                                (e5, i + 1)

                        (e7, val) = evalProgram e6 body
                        # reset environment back to before the function was run.
                        e8 = decEnv e7 newIndex
                        ({ e8 & currentEnv: oldIndex }, val)
                    else
                        (e2, Err "FUNCTION applied with wrong number of args")

                _ ->
                    type = valueToType fnVal
                    (e2, Err "expected FUNCTION instead got \(type)")

evalArgs = \e0, args ->
    List.walkUntil args (e0, List.withCapacity (List.len args)) \(e1, exprs), arg ->
        (e2, argVal) = evalNode e1 arg
        when argVal is
            Err e -> Break (e2, [Err e])
            _ -> Continue (e2, List.append exprs argVal)

infixErr : Str, Value, Value -> Value
infixErr = \opStr, lhs, rhs ->
    lhsType = valueToType lhs
    rhsType = valueToType rhs
    if lhsType != rhsType then
        Err "type mismatch: \(lhsType) \(opStr) \(rhsType)"
    else
        Err "unknown operator: \(lhsType) \(opStr) \(rhsType)"

isTruthy : Value -> Bool
isTruthy = \val ->
    when val is
        True -> Bool.true
        False -> Bool.false
        Null -> Bool.false
        Int _ -> Bool.true
        _ -> crash "ret values are not truthy"

okOrUnreachable = \res, str ->
    when res is
        Ok v -> v
        Err _ -> crash "unreachable: \(str)"

runFromSource = \input ->
    input
    |> Str.toUtf8
    |> Lexer.lex
    |> Parser.parse
    |> okOrUnreachable "parse unexpectedly failed"
    |> eval
    |> .1

expect
    inputs = ["5", "10", "-5", "-10", "true", "false"]
    out = List.map inputs runFromSource

    expected = [Int 5, Int 10, Int -5, Int -10, True, False]
    out == expected

expect
    inputs = [
        "!true",
        "!false",
        "!5",
        "!!true",
        "!!false",
        "!!5",
    ]
    out = List.map inputs runFromSource

    expected = [
        False,
        True,
        False,
        True,
        False,
        True,
    ]
    out == expected

expect
    inputs = [
        "5 + 5 + 5 + 5 - 10",
        "2 * 2 * 2 * 2 * 2",
        "-50 + 100 + -50",
        "5 * 2 + 10",
        "5 + 2 * 10",
        "20 + 2 * -10",
        "50 / 2 * 2 + 10",
        "2 * (5 + 10)",
        "3 * 3 * 3 + 10",
        "3 * (3 * 3) + 10",
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
    ]
    out = List.map inputs runFromSource

    expected = [
        Int 10,
        Int 32,
        Int 0,
        Int 20,
        Int 25,
        Int 0,
        Int 60,
        Int 30,
        Int 37,
        Int 37,
        Int 50,
    ]
    out == expected

expect
    inputs = [
        "true == true",
        "false == false",
        "true == false",
        "true != false",
        "false != true",
        "(1 < 2) == true",
        "(1 < 2) == false",
        "(1 > 2) == true",
        "(1 > 2) == false",
    ]
    out = List.map inputs runFromSource

    expected = [
        True,
        True,
        False,
        True,
        True,
        True,
        False,
        False,
        True,
    ]
    out == expected

expect
    inputs = [
        "if (true) { 10 }",
        "if (false) { 10 }",
        "if (1) { 10 }",
        "if (1 < 2) { 10 }",
        "if (1 > 2) { 10 }",
        "if (1 < 2) { 10 } else { 20 }",
        "if (1 > 2) { 10 } else { 20 }",
    ]
    out = List.map inputs runFromSource

    expected = [
        Int 10,
        Null,
        Int 10,
        Int 10,
        Null,
        Int 10,
        Int 20,
    ]
    out == expected

expect
    inputs = [
        "return 10;",
        "return 10; 9",
        "return 2 * 5; 9",
        "9; return 10; 9",
        "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
    ]
    out = List.map inputs runFromSource

    expected = [
        Int 10,
        Int 10,
        Int 10,
        Int 10,
        Int 10,
    ]
    out == expected

expect
    inputs = [
        "let a = 5; a;",
        "let a = 5 * 5; a;",
        "let a = 5; let b = a; b;",
        "let a = 5; let b = a; let c = a + b + 5; c;",
    ]
    out = List.map inputs runFromSource

    expected = [
        Int 5,
        Int 25,
        Int 5,
        Int 15,
    ]
    out == expected

expect
    inputs = [
        "let identity = fn(x) { x; }; identity(5);",
        "let identity = fn(x) { return x; 12; }; identity(5);",
        "let double = fn(x) { 2 * x;  }; double(5);",
        "fn(x) {x} (5)",
        "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
    ]
    out = List.map inputs runFromSource

    expected = [
        Int 5,
        Int 5,
        Int 10,
        Int 5,
        Int 20,
    ]
    out == expected

expect
    input =
        """
        let newAdder = fn(x) {
          fn(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);
        """
    out = runFromSource input

    expected = Int 4
    out == expected

