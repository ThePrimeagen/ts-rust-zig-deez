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

bench:
	@echo "==> Benchmarking"
	@dotnet run -c release --project MonkeyBench

clean:
	@echo "===> Cleaning"
	@dotnet clean Monkey -v m

fmt:
	@echo "===> Formatting"
	@dotnet format -v m

lint:
	@echo "===> Linting"

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"

test-lexer:
	@echo "===> Testing lexer"
	@dotnet test MonkeyTests --filter LexerTests

test-ast:
	@echo "===> Testing AST"
	@dotnet test MonkeyTests --filter AstTests

test-parser:
	@echo "===> Testing parser"
	@dotnet test MonkeyTests --filter ParserTests
