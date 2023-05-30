import unittest
from parser.parser import Parser
from parser.my_ast import Identifier, ExpressionStatement, ReturnStatement, LetStatement, Statement
from lexer.lexer import Lexer


def test_let_statement(s: Statement, name: str) -> bool:
    assert s.TokenLiteral(
    ) != "let", f"s.TokenLiteral not 'let'. got={s.TokenLiteral()}"

    assert isinstance(
        s, LetStatement), f"s not *ast.LetStatement. got=%{s}"

    assert s.Name.Value == name, f"letStmt.Name.Value not '{name}'. got={s.Name.Value}"
    assert s.TokenLiteral(
    ) == name, f"s.Name not '{name}'. got={s.Name}"
    return True


def checkParserErrors(p: Parser):
    errors = p.errors
    if len(errors) == 0:
        return
    print(f"Parser has {len(errors)}")

    for msg in errors:
        print(f"Parser Error: {msg}")
    raise RuntimeError


input_test_valid = """let x = 5;
        let y = 10;
        let foobar = 838383;"""

input_test_invalid = """let x 5;
        let = 10;
        let 838383;"""


class ParserTesting(unittest.TestCase):
    def test_let_statements(self):
        input_test = input_test_valid
        l = Lexer(input_test)
        p = Parser(l)

        program = p.ParseProgram()
        checkParserErrors(p)
        if program is None:
            assert False, "ParseProgram() return None"
        if len(program.Statements) != 3:
            assert False, f"program.Statements does not contain 3 statements. got={len(program.Statements)}"

        tests = ["x", "y", "foobar"]

        for i, tt in enumerate(tests):
            stmt = program.Statements[i]
            # print(stmt)
            if not test_let_statement(stmt, tt):
                return

    # def test_let_statements_invalid(self):
    #     input_test = input_test_invalid
    #     l = Lexer(input_test)
    #     p = Parser(l)
    #
    #     program = p.ParseProgram()
    #     checkParserErrors(p)
    #     if program is None:
    #         assert False, "ParseProgram() return None"
    #     if len(program.Statements) != 3:
    #         assert False, f"program.Statements does not contain 3 statements. got={len(program.Statements)}"
    #
    #     tests = ["x", "y", "foobar"]
    #
    #     for i, tt in enumerate(tests):
    #         stmt = program.Statements[i]
    #         # print(stmt)
    #         if not test_let_statement(stmt, tt):
    #             return

    def test_return_statements(self):
        input_test = """return 5;
        return 10;
        return 993322;
        """
        l = Lexer(input_test)
        p = Parser(l)

        program = p.ParseProgram()
        checkParserErrors(p)
        assert len(
            program.Statements) == 3, f"program.Statements does not contain 3 statements. got={len(program.Statements)}"

        for stmt in program.Statements:
            returnStmt = stmt
            assert isinstance(
                returnStmt, ReturnStatement), f"stmt not *ast.returnStatement. got={returnStmt}"
            assert returnStmt.TokenLiteral(
            ) == 'return', f"returnStmt.TokenLiteral not 'return', got {returnStmt.TokenLiteral()}"

    def test_identifier_expression(self):

        input = "foobar;"
        l = Lexer(input)
        p = Parser(l)
        program = p.ParseProgram()
        checkParserErrors(p)
        print(program.Statements)
        assert len(
            program.Statements) == 1, f"program has not enough statements. got={len(program.Statements)}"
        stmt = program.Statements[0]
        assert isinstance(
            stmt, ExpressionStatement), f"program.Statements[0] is not ast.ExpressionStatement. got={program.Statements[0]}"
        ident = stmt.Expression
        assert isinstance(
            ident, Identifier), f"exp not *ast.Identifier. got={stmt.Expression}"
        assert ident.Value == "foobar", f"ident.Value not foobar. got={ident.Value}"
        assert ident.TokenLiteral(
        ) == "foobar", f"ident.TokenLiteral not foobar. got={ident.TokenLiteral()}"
