import unittest
import logging
from typing import cast
import monkey.ast as ast

from monkey.lexer import Lexer
from monkey.parser import Parser


class ParserTest(unittest.TestCase):
    def test_let_statements(self):
        tests = [
            ("let x = 5;", "x", 5),
            ("let y = true;", "y", True),
            ("let foobar = y;", "foobar", "y"),
        ]

        for test in tests:
            lex = Lexer(test[0])
            parser = Parser(lex)

            program = parser.parse_program()
            self.check_parser_errors(parser)
            self.assertIsNotNone(program, "parse_program returned None")
            self.assertEqual(
                len(program.statements),
                1,
                msg=("program.statements does not contain 1 statements."),
            )

            stmt: ast.LetStatement = program.statements[0]
            self.assertIsInstance(stmt, ast.LetStatement)
            self.let_statement_test(stmt, test[1])
            val = stmt.value
            self.literal_expression_test(val, test[2])

    def let_statement_test(self, statement: ast.Statement, name: str):
        if statement.token_literal() != "let":
            logging.error(f"Token literal is not let. {statement.token_literal()=}")
            return False

        if not isinstance(statement, ast.LetStatement):
            logging.error(f"statement is not instace of LetStatement. {type(statement)=}")
            return False

        if statement.name.value != name:
            logging.error(f"statement.name.value not {name}. {statement.name.value=}")
            return False
        if statement.name.token_literal() != name:
            logging.error(f"statement name not {name}. {statement.name.token_literal()=}")
            return False
        return True

    def test_return_statements(self):
        input = """
        return 5;
        return 10;
        return 993322;
        """
        lex = Lexer(input)
        parser = Parser(lex)

        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertIsNotNone(program, "parse_program returned None")
        self.assertEqual(
            len(program.statements),
            3,
            msg=("program.statements does not contain 3 statements."),
        )
        for stmt in program.statements:
            self.assertIsInstance(stmt, ast.ReturnStatement)
            self.assertEqual(stmt.token_literal(), "return")

    def test_identifier_expression(self):
        input = "foobar;"

        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, "Program has not enough statements.")

        stmt: ast.ExpressionStatement = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)

        ident: ast.Identifier = stmt.expression
        self.assertIsInstance(ident, ast.Identifier)
        self.assertEqual(ident.value, "foobar")
        self.assertEqual(ident.token_literal(), "foobar")

    def test_int_literal_expression(self):
        input = "5;"
        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}")

        stmt: ast.ExpressionStatement = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        literal: ast.IntegerLiteral = stmt.expression
        self.assertIsInstance(literal, ast.IntegerLiteral)
        self.assertEqual(literal.value, 5)
        self.assertEqual(literal.token_literal(), "5")

    def test_parsing_prefix_expressions(self):
        prefix_tests = [
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ]
        for test in prefix_tests:
            lexer = Lexer(test[0])
            parser = Parser(lexer)
            program = parser.parse_program()
            self.check_parser_errors(parser)
            self.assertEqual(
                len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}"
            )

            stmt: ast.ExpressionStatement = program.statements[0]
            self.assertIsInstance(stmt, ast.ExpressionStatement)
            expression: ast.PrefixExpression = stmt.expression
            self.assertIsInstance(expression, ast.PrefixExpression)
            self.assertEqual(expression.operator, test[1])

            self.integer_literal_test(expression.right, test[2])

    def test_parsing_infix_expression(self):
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
            self.check_parser_errors(parser)
            self.assertEqual(
                len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}"
            )

            stmt: ast.ExpressionStatement = program.statements[0]
            self.assertIsInstance(stmt, ast.ExpressionStatement)
            expression: ast.InfixExpression = stmt.expression
            self.assertIsInstance(expression, ast.InfixExpression)

            self.integer_literal_test(expression.left, test[1])
            self.assertEqual(test[2], expression.operator)
            self.integer_literal_test(expression.right, test[3])

    def test_parsing_infix_expression_2(self):
        infix_tests = [
            ("true == true", True, "==", True),
            ("true != false", True, "!=", False),
            ("false == false", False, "==", False),
        ]

        for test in infix_tests:
            lexer = Lexer(test[0])
            parser = Parser(lexer)
            program = parser.parse_program()
            self.check_parser_errors(parser)
            self.assertEqual(
                len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}"
            )

            stmt: ast.ExpressionStatement = program.statements[0]
            self.assertIsInstance(stmt, ast.ExpressionStatement)
            expression: ast.InfixExpression = stmt.expression
            self.assertIsInstance(expression, ast.InfixExpression)

            self.literal_expression_test(expression.left, test[1])
            self.assertEqual(test[2], expression.operator)
            self.literal_expression_test(expression.right, test[3])

    def test_if_expression(self):
        input = "if (x < y) {x}"
        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}")
        stmt: ast.ExpressionStatement = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        expression: ast.IfExpression = stmt.expression
        self.assertIsInstance(expression, ast.IfExpression)

        self.infix_expression_test(expression.condition, "x", "<", "y")

        self.assertEqual(len(expression.consequence.statements), 1)
        consequence: ast.ExpressionStatement = expression.consequence.statements[0]
        self.assertIsInstance(consequence, ast.ExpressionStatement)

        self.identifier_test(consequence.expression, "x")
        self.assertIsNone(expression.Alternative)

    def test_function_literal(self):
        input = "fn(x, y) { x + y; }"

        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}")
        stmt: ast.ExpressionStatement = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        function: ast.FunctionLiteral = stmt.expression
        self.assertIsInstance(function, ast.FunctionLiteral)

        self.assertEqual(len(function.parameters), 2)
        self.literal_expression_test(function.parameters[0], "x")
        self.literal_expression_test(function.parameters[1], "y")

        self.assertEqual(len(function.body.statements), 1)

        body_stmt: ast.ExpressionStatement = function.body.statements[0]
        self.assertIsInstance(body_stmt, ast.ExpressionStatement)

        self.infix_expression_test(body_stmt.expression, "x", "+", "y")

    def test_call_expression(self):
        input = "add(1, 2 * 3, 4 + 5);"

        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}")
        stmt: ast.ExpressionStatement = program.statements[0]
        self.assertIsInstance(stmt, ast.ExpressionStatement)
        call: ast.CallExpression = stmt.expression
        self.assertIsInstance(call, ast.CallExpression)

        self.identifier_test(call.function, "add")
        self.assertEqual(len(call.arguments), 3)

        self.literal_expression_test(call.arguments[0], 1)
        self.infix_expression_test(call.arguments[1], 2, "*", 3)
        self.infix_expression_test(call.arguments[2], 4, "+", 5)

    def integer_literal_test(self, exp: ast.Expression, value: int):
        self.assertIsInstance(exp, ast.IntegerLiteral)
        self.assertEqual(exp.value, value)
        self.assertEqual(exp.token_literal(), str(value))

    def identifier_test(self, exp: ast.Expression, value: str):
        self.assertIsInstance(exp, ast.Identifier)
        self.assertEqual(exp.value, value)
        self.assertEqual(exp.token_literal(), value)

    def boolean_literal_test(self, exp: ast.Expression, value: bool):
        self.assertIsInstance(exp, ast.Boolean)
        self.assertEqual(exp.value, value)
        self.assertEqual(exp.token_literal(), str(value).lower())

    def literal_expression_test(self, exp: ast.Expression, expected):
        match expected:
            case bool():
                self.boolean_literal_test(exp, expected)
            case int():
                self.integer_literal_test(exp, expected)
            case str():
                self.identifier_test(exp, expected)
            case _:
                self.fail(f"Type of exp not handled. {expected=}")

    def infix_expression_test(self, exp: ast.Expression, left, operator: str, right):
        self.assertIsInstance(exp, ast.InfixExpression)
        self.literal_expression_test(exp.left, left)
        self.assertEqual(exp.operator, operator)
        self.literal_expression_test(exp.right, right)

    def check_parser_errors(self, parser: Parser):
        if not parser.errors:
            return
        logging.error("Parser has errors.")
        for err in parser.errors:
            logging.error(f"Parser error: {err}")

        self.fail()
