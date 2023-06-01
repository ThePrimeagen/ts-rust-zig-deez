.PHONY: help ready docker-build docker-ready fmt lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/app -t deez_$(notdir $(shell pwd))

clean:
	@echo "===> Cleaning"
	./gradlew clean

fmt:
	@echo "===> Formatting"
	./gradlew ktlintFormat

lint:
	@echo "===> Linting"
	./gradlew ktlintCheck

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"

test-lexer:
	@echo "===> Testing lexer"
	./gradlew test --tests dev.hermannm.monkeylang.LexerTest

test-ast:
	@echo "===> Testing AST"
	@echo "[Not yet implemented]"

test-parser:
	@echo "===> Testing parser"
	@echo "[Not yet implemented]"

repl:
	@echo "===> Running REPL"
	./gradlew run --quiet --console=plain
