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

NASM_FLAGS = -felf64 -isrc -isys
ASM_FILES = $(wildcard src/*.asm)

# The runtime of this is so minuscule that we can recompile it every time.
.PHONY: lexer
lexer: $(ASM_FILES) sys/sys.asm
	nasm src/main.asm -o main.o $(NASM_FLAGS)
	cc main.o -o lexer

sys/sys.asm: sys/gen_header.c
	@echo "===> Generating sys.asm"
	cc sys/gen_header.c -o gen_header -std=gnu11 -Wall -Wextra
	./gen_header > sys/sys.asm
