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
	cabal clean

fmt:
	@echo "===> Formatting"
	fourmolu --mode inplace .

lint:
	@echo "===> Linting"
	hlint .

test:
	@echo "===> Testing EVERYTHING"
	cabal test

test-lexer:
	@echo "===> Testing lexer"
	# TODO: replace this line with your lexer testing commands

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands
