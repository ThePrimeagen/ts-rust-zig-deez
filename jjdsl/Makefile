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
	ant -Djava8=%JAVA_HOME% -Dgithub=https://github.com/Crain-32/ts-rust-zig-deez -Dbranch=jjdsl -d -v

fmt:
	@echo "===> Formatting"
	# TODO: replace this line with your formatting commands

lint:
	@echo "===> Linting"
	# TODO: replace this line with your linting commands

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	# TODO: replace this line with your testing commands

test-lexer:
	@echo "===> Testing lexer"
	# TODO: replace this line with your lexer testing commands

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands
