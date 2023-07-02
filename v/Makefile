.PHONY: help ready docker-build docker-ready fmt lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

clean:
	@echo "===> Cleaning"

fmt:
	@echo "===> Formatting"
	v fmt -w src/

lint:
	@echo "===> Linting"
	v vet src/

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	v test src/

test-lexer:
	@echo "===> Testing lexer"
	v -stats test src/modules/lexer/lexer_test.v

test-ast:
	@echo "===> Testing AST"

test-parser:
	@echo "===> Testing parser"
