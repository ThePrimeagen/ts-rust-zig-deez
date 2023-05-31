##
# Static part, don't change these:
##

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

fmt:
	@echo "===> Formatting"

lint:
	@echo "===> Linting"

test: lexer
	@echo "===> Testing"
	# Yes, this is scuffed, but also, try writing tests in assembly...
	./lexer test/example.monkey  | diff - expected/example.expected
	./lexer test/example2.monkey | diff - expected/example2.expected

lexer: src/main.s
	nasm -felf64 src/main.s -o main.o
	cc main.o -o lexer
