import pytest

import logging

from deez_py import (
    Lexer,
    Parser,
    deez_ast as ast,
)


@pytest.mark.parametrize(
    "input_str, name, value",
    [
        ("let x = 5;", "x", 5),
        ("let y = true;", "y", True),
        ("let foobar = y;", "foobar", "y"),
    ],
)
def test_let_statements(input_str, name, value):
    lex = Lexer(input_str)
    parser = Parser(lex)

    program = parser.parse_program()
    check_parser_errors(parser)
    assert program is not None, "parse_program returned None"
    assert (
        len(program.statements) == 1
    ), "program.statements does not contain 1 statement."

    stmt = program.statements[0]
    assert isinstance(stmt, ast.LetStatement)
    let_statement_test(stmt, name)
    val = stmt.value
    literal_expression_test(val, value)


def let_statement_test(statement, name):
    if statement.token_literal() != "let":
        logging.error(
            f"Token literal is not let. {statement.token_literal()=}"
        )
        return False

    if not isinstance(statement, ast.LetStatement):
        logging.error(
            f"statement is not instace of LetStatement. {type(statement)=}"
        )
        return False

    if statement.name.value != name:
        logging.error(
            f"statement.name.value not {name}. {statement.name.value=}"
        )
        return False
    if statement.name.token_literal() != name:
        logging.error(
            f"statement name not {name}. {statement.name.token_literal()=}"
        )
        return False
    return True


def test_return_statements():
    input_str = """
    return 5;
    return 10;
    return 993322;
    """
    lex = Lexer(input_str)
    parser = Parser(lex)

    program = parser.parse_program()
    check_parser_errors(parser)
    assert program is not None, "parse_program returned None"
    assert (
        len(program.statements) == 3
    ), "program.statements does not contain 3 statements."
    for stmt in program.statements:
        assert isinstance(stmt, ast.ReturnStatement)
        assert stmt.token_literal() == "return"


def test_identifier_expression():
    input_str = "foobar;"

    lexer = Lexer(input_str)
    parser = Parser(lexer)
    program = parser.parse_program()
    check_parser_errors(parser)
    assert len(program.statements) == 1, "Program has not enough statements."

    stmt = program.statements[0]
    assert isinstance(stmt, ast.ExpressionStatement)

    ident = stmt.expression
    assert isinstance(ident, ast.Identifier)
    assert ident.value == "foobar"
    assert ident.token_literal() == "foobar"


def check_parser_errors(parser):
    if not parser.errors:
        return
    logging.error("Parser has errors.")
    for err in parser.errors:
        logging.error(f"Parser error: {err}")

    pytest.fail()


def literal_expression_test(exp, expected):
    if isinstance(expected, bool):
        boolean_literal_test(exp, expected)
    elif isinstance(expected, int):
        integer_literal_test(exp, expected)
    elif isinstance(expected, str):
        identifier_test(exp, expected)
    else:
        pytest.fail(f"Type of exp not handled. {expected=}")


def integer_literal_test(exp, value):
    assert isinstance(exp, ast.IntegerLiteral)
    assert exp.value == value
    assert exp.token_literal() == str(value)


def test_int_literal_expression():
    input_str = "5;"
    lexer = Lexer(input_str)
    parser = Parser(lexer)
    program = parser.parse_program()
    check_parser_errors(parser)
    assert (
        len(program.statements) == 1
    ), f"Program has not enough statements. {len(program.statements)=}"

    stmt = program.statements[0]
    assert isinstance(stmt, ast.ExpressionStatement)
    literal = stmt.expression
    assert isinstance(literal, ast.IntegerLiteral)
    assert literal.value == 5
    assert literal.token_literal() == "5"


def test_parsing_prefix_expressions():
    prefix_tests = [
        ("!5;", "!", 5),
        ("-15;", "-", 15),
    ]
    for test in prefix_tests:
        lexer = Lexer(test[0])
        parser = Parser(lexer)
        program = parser.parse_program()
        check_parser_errors(parser)
        assert (
            len(program.statements) == 1
        ), f"Program has not enough statements. {len(program.statements)=}"

        stmt = program.statements[0]
        assert isinstance(stmt, ast.ExpressionStatement)
        expression = stmt.expression
        assert isinstance(expression, ast.PrefixExpression)
        assert expression.operator == test[1]

        integer_literal_test(expression.right, test[2])


def test_parsing_infix_expression():
    infix_tests = [
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ]

    for test in infix_tests:
        lexer = Lexer(test[0])
        parser = Parser(lexer)
        program = parser.parse_program()
        check_parser_errors(parser)
        assert (
            len(program.statements) == 1
        ), f"Program has not enough statements. {len(program.statements)=}"

        stmt = program.statements[0]
        assert isinstance(stmt, ast.ExpressionStatement)
        expression = stmt.expression
        assert isinstance(expression, ast.InfixExpression)

        integer_literal_test(expression.left, test[1])
        assert test[2] == expression.operator
        integer_literal_test(expression.right, test[3])


def test_parsing_infix_expression_2():
    infix_tests = [
        ("true == true", True, "==", True),
        ("true != false", True, "!=", False),
        ("false == false", False, "==", False),
    ]

    for test in infix_tests:
        lexer = Lexer(test[0])
        parser = Parser(lexer)
        program = parser.parse_program()
        check_parser_errors(parser)
        assert (
            len(program.statements) == 1
        ), f"Program has not enough statements. {len(program.statements)=}"

        stmt = program.statements[0]
        assert isinstance(stmt, ast.ExpressionStatement)
        expression = stmt.expression
        assert isinstance(expression, ast.InfixExpression)

        literal_expression_test(expression.left, test[1])
        assert test[2] == expression.operator
        literal_expression_test(expression.right, test[3])


def test_if_expression():
    input_str = "if (x < y) {x}"
    lexer = Lexer(input_str)
    parser = Parser(lexer)
    program = parser.parse_program()
    check_parser_errors(parser)
    assert (
        len(program.statements) == 1
    ), f"Program has not enough statements. {len(program.statements)=}"

    stmt = program.statements[0]
    assert isinstance(stmt, ast.ExpressionStatement)
    expression = stmt.expression
    assert isinstance(expression, ast.IfExpression)

    infix_expression_test(expression.condition, "x", "<", "y")

    assert len(expression.consequence.statements) == 1
    consequence = expression.consequence.statements[0]
    assert isinstance(consequence, ast.ExpressionStatement)

    identifier_test(consequence.expression, "x")
    assert expression.Alternative is None


def test_function_literal():
    input_str = "fn(x, y) { x + y; }"

    lexer = Lexer(input_str)
    parser = Parser(lexer)
    program = parser.parse_program()
    check_parser_errors(parser)
    assert (
        len(program.statements) == 1
    ), f"Program has not enough statements. {len(program.statements)=}"

    stmt = program.statements[0]
    assert isinstance(stmt, ast.ExpressionStatement)
    function = stmt.expression
    assert isinstance(function, ast.FunctionLiteral)

    assert len(function.parameters) == 2
    literal_expression_test(function.parameters[0], "x")
    literal_expression_test(function.parameters[1], "y")

    assert len(function.body.statements) == 1

    body_stmt = function.body.statements[0]
    assert isinstance(body_stmt, ast.ExpressionStatement)

    infix_expression_test(body_stmt.expression, "x", "+", "y")


def test_call_expression():
    input = "add(1, 2 * 3, 4 + 5);"

    lexer = Lexer(input)
    parser = Parser(lexer)
    program = parser.parse_program()
    check_parser_errors(parser)
    assert (
        len(program.statements) == 1
    ), f"Program has not enough statements. {len(program.statements)=}"
    stmt = program.statements[0]
    assert isinstance(stmt, ast.ExpressionStatement)
    call = stmt.expression
    assert isinstance(call, ast.CallExpression)

    identifier_test(call.function, "add")
    assert len(call.arguments) == 3

    literal_expression_test(call.arguments[0], 1)
    infix_expression_test(call.arguments[1], 2, "*", 3)
    infix_expression_test(call.arguments[2], 4, "+", 5)


def identifier_test(exp, value):
    assert isinstance(exp, ast.Identifier)
    assert exp.value == value
    assert exp.token_literal() == value


def boolean_literal_test(exp, value):
    assert isinstance(exp, ast.Boolean)
    assert exp.value == value
    assert exp.token_literal() == str(value).lower()


def infix_expression_test(exp, left, operator, right):
    assert isinstance(exp, ast.InfixExpression)
    literal_expression_test(exp.left, left)
    assert exp.operator == operator
    literal_expression_test(exp.right, right)
