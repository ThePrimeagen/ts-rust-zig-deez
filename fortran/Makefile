REPL_SRC=list.f90 token.f90 lexer.f90 ast.f90 parser.f90 repl.f90
TEST_LEXER_SRC=list.f90 token.f90 lexer.f90 test_lexer.f90
TEST_AST_SRC=list.f90 token.f90 lexer.f90 ast.f90 test_ast.f90
TEST_PARSER_SRC=list.f90 token.f90 lexer.f90 ast.f90 parser.f90 test_parser.f90

repl: $(REPL_SRC)
	gfortran $(REPL_SRC) -o repl

test: test-lexer test-ast test-parser

test-lexer: $(TEST_LEXER_SRC)
	gfortran $(TEST_LEXER_SRC) -o test-lexer
	./test-lexer

test-ast: $(TEST_AST_SRC)
	gfortran $(TEST_AST_SRC) -o test-ast
	./test-ast

test-parser: $(TEST_PARSER_SRC)
	gfortran $(TEST_PARSER_SRC) -o test-parser
	./test-parser

clean:
	rm -f *.o *.mod repl test-lexer test-ast test-parser
