.PHONY: help ready docker-build docker-ready test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))


test:
	@echo "===> Testing EVERYTHING"
	godot --no-window --script src/main.gd --test=lexer,parser,ast

test-lexer:
	@echo "===> Testing lexer"
	godot --no-window --script src/main.gd --test=lexer

test-ast:
	@echo "===> Testing AST"
	godot --no-window --script src/main.gd --test=ast

test-parser:
	@echo "===> Testing parser"
	godot --no-window --script src/main.gd --test=parser

