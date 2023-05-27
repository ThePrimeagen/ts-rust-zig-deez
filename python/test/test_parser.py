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

    def check_parser_errors(self, parser: Parser):
        if not parser.errors:
            return
        logging.error(f"Parser has errors.")
        for err in parser.errors:
            logging.error(f"Parser error: {err}")

        self.fail()
