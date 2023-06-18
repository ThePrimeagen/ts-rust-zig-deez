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
	mvn -f ./monkey/pom.xml clean

fmt:
	@echo "===> Formatting"
	@echo "No formatting configured"

lint:
	@echo "===> Linting"
	@echo "No linting configured"

test:
	@echo "===> Testing EVERYTHING"
	mvn -f ./monkey/pom.xml test

test-lexer:
	@echo "===> Testing lexer"
	mvn -f ./monkey/pom.xml test -Dtest=LexerTest#* test

test-ast:
	@echo "===> Testing AST"
	mvn -f ./monkey/pom.xml test -Dtest=ParserTest#testString test

test-parser:
	@echo "===> Testing parser"
	mvn -f ./monkey/pom.xml test -Dtest=ParserTest#* test
