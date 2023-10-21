.PHONY: help ready lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: lint test

lint:
	shellcheck src/*.sh

test: test-lexer

test-lexer:
	bats test/
