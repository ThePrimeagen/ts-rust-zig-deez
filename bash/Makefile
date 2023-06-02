##
# Static part, don't change these:
##

.PHONY: help ready docker-build docker-ready clean fmt lint test

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

clean:
	@echo "===> Cleaning"
	# TODO: replace this line with your cleaning commands

fmt:
	@echo "===> Formatting"
	# TODO: replace this line with your formatting commands

lint:
	@echo "===> Linting"
	shellcheck --shell=bash --source-path=src/ src/**

test: test-lexer test-ast test-parser
	@echo "===> Tested EVERYTHING"

test-lexer:
	@echo "===> Testing lexer"
	bash src/lexer_test.sh

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands

