import unittest
import logging
import monkey.ast as ast

from monkey.lexer import Lexer
from monkey.parser import Parser


class ParserTest(unittest.TestCase):
    def test_let_statements(self):
        input = """\
let x = 5;
let y = 10;
let foobar = 838383;
"""

        lex = Lexer(input)
        parser = Parser(lex)

        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertIsNotNone(program, "parse_program returned None")
        self.assertEqual(
            len(program.statements),
            3,
            msg=("program.statements does not contain 3 statements. Got = %d", len(program.statements)),
        )

        tests = ["x", "y", "foobar"]

        for i, expected in enumerate(tests):
            statement = program.statements[i]
            self.assertTrue(self.let_statement_test(statement, expected))

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

    def test_identifier_expression(self):
        input = "foobar;"

        lexer = Lexer(input)
        parser = Parser(lexer)
        program = parser.parse_program()
        self.check_parser_errors(parser)
        self.assertEqual(len(program.statements), 1, f"Program has not enough statements. {len(program.statements)=}")

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

    def integer_literal_test(self, exp: ast.IntegerLiteral, value: int):
        self.assertIsInstance(exp, ast.IntegerLiteral)
        self.assertEqual(exp.value, value)
        self.assertEqual(exp.token_literal(), str(value))

    def check_parser_errors(self, parser: Parser):
        if not parser.errors:
            return
        logging.error("Parser has errors.")
        for err in parser.errors:
            logging.error(f"Parser error: {err}")

        self.fail()
