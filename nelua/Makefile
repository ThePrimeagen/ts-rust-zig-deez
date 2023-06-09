##
# Static part, don't change these:
##

.PHONY: help ready docker-build docker-ready fmt lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update those:
##

# compile = nelua -r -M -s repl.nelua -o bin/monkey-repl
src = repl.nelua lexerTest.nelua lexer.nelua

clean:
	@echo "===> Cleaning"
	rm -r bin

fmt:
	@echo "===> Formatting"
	# TODO: replace this line with your formatting commands

lint:
	@echo "===> Linting"
	nelua -a $(src)

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	# TODO: replace this line with your testing commands

test-lexer:
	@echo "===> Testing lexer"
	bash -c "diff <(nelua lexerTest.nelua) <(cat expect.txt)" || \
		echo "Tests Failed! look at the differences in expected output and the output of the lexer ^" && \
		return 1 # Grants that the shell will receive error after the echo

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands

