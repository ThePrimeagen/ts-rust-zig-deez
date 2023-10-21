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

clean:
	@echo "===> Cleaning"
	# TODO: replace this line with your cleaning commands

fmt:
	@echo "===> Formatting"
	# TODO: replace this line with your formatting commands

lint:
	@echo "===> Linting"
	# TODO: replace this line with your linting commands

test: test-lexer test-ast test-parser test-eval
	@echo "===> Testing EVERYTHING"
	bb test

test-lexer:
	@echo "===> Testing lexer"
	bb test --nses monkey-lang.lexer-test

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	bb test --nses monkey-lang.parser-test

test-eval:
	@echo "===> Testing eval"
	bb test --nses monkey-lang.eval-test
