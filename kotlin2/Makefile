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

test:
	@echo "===> Testing"
	./gradlew :monkey-common:te st --tests dev.hermannm.monkeylang.LexerTest

repl:
	@echo "===> Running REPL (JVM)"
	./gradlew :monkey-jvm:run --quiet --console=plain

repl-browser:
	@echo "===> Running REPL (JS in browser)"
	./gradlew :monkey-js:jsBrowserRun

repl-browser-dev:
	@echo "===> Running REPL (JS in browser) [Development mode with hot reload]"
	./gradlew :monkey-js:jsBrowserDevelopmentRun --continuous
