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
	@dotnet format Monkey -v n

lint:
	@echo "===> Linting"

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	# TODO: replace this line with your testing commands

test-lexer:
	@echo "===> Testing lexer"
	@dotnet test MonkeyTests -l:"console;verbosity=normal" --filter LexerTests

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands
