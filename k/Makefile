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
	# DC wdym formatting we're writing array langs here

lint:
	@echo "===> Linting"
	# DC wdym linting...

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"

test-lexer:
	@echo "===> Testing lexer"
	k/k test.k

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands

