##
# Static part, don't change these:
##

# Inspired by the Fortran Makefile. Thanks @greuben92!
REPL_SRC=src/token.groovy src/lexer.groovy src/repl.groovy
TEST_LEXER_SRC=src/token.groovy src/lexer.groovy src/test_lexer.groovy
TEST_AST_SRC=src/list.groovy src/token.groovy src/lexer.groovy src/ast.groovy src/test_ast.groovy
TEST_PARSER_SRC=src/list.groovy src/token.groovy src/lexer.groovy src/ast.groovy src/parser.groovy src/test_parser.groovy

.PHONY: help ready docker-build docker-ready lint test test-lexer test-ast test-parser

help:
	@cat Makefile | grep -E "^\w+$:"

ready: test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update those:
##

repl: $(REPL_SRC)
	groovyc -d target $(REPL_SRC)
	cd target && java -cp ".:/${GROOVY_HOME}/lib/*" repl

clean:
	rm -rf target

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"

test-lexer:
	@echo "===> Testing LEXER"
	groovyc -d target $(TEST_LEXER_SRC)
	cd target && java -cp ".:/${GROOVY_HOME}/lib/*" test_lexer

test-ast: $(TEST_AST_SRC)
	groovyc $(TEST_AST_SRC) -d target
	cd target && java -cp ".:/${GROOVY_HOME}/lib/*" test_ast

test-parser: $(TEST_PARSER_SRC)
	groovyc $(TEST_PARSER_SRC) -d target
	cd target && java -cp ".:/${GROOVY_HOME}/lib/*" test_parser
