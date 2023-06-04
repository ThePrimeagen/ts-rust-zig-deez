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
	./gradlew formatKotlin

lint:
	@echo "===> Linting"
	./gradlew lintKotlin

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
	@echo "===> Running REPL (JVM)"
	./gradlew :jvm:run --quiet --console=plain

repl-browser:
	@echo "===> Running REPL (Kotlin-JS in browser)"
	./gradlew :web:jsBrowserRun

repl-browser-dev:
	@echo "===> Running REPL (Kotlin-JS in browser) [Development mode with hot reload]"
	./gradlew :web:jsBrowserDevelopmentRun --continuous
