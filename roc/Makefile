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
	rm -f src/🐵🤘🏼

fmt:
	@echo "===> Formatting"
	roc format src/*

lint:
	@echo "===> Linting"
	roc check src/main.roc

test:
	@echo "===> Testing EVERYTHING"
	roc test src/main.roc

test-lexer:
	@echo "===> Testing lexer"
	roc test src/Lexer.roc

test-parser:
	@echo "===> Testing parser"
	roc test src/Parser.roc

test-eval:
	@echo "===> Testing parser"
	roc test src/Eval.roc
