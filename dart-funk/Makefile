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
	dart pub get

fmt:
	@echo "===> Formatting"
	dart format .

lint:
	@echo "===> Linting"
	dart format .

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	dart pub global run coverage:format_coverage --packages=.dart_tool/package_config.json --report-on=lib --lcov -o ./coverage/lcov.info -i ./coverage --check-ignore && dart run test --coverage=./coverage && genhtml -o ./coverage/report ./coverage/lcov.info && open ./coverage/report/index.html

test-lexer:
	@echo "===> Testing lexer"
	dart test ./test/src/lexer_test.dart

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands

