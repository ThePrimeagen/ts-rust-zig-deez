# config
AS        = nasm    # use nasm or yasm
ASFLAGS   = -felf64
LD        = gcc

# the -no-pie flag is required because of some linkage bs, otherwise we need stupid syntax in the code
# https://www.mourtada.se/calling-printf-from-the-c-standard-library-in-assembly/
LDFLAGS   = -no-pie
BUILD_DIR = bin

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

OBJ=$(patsubst src/%.asm, $(BUILD_DIR)/%.o, $(wildcard src/*.asm))

clean:
	@echo "===> Cleaning"
	# TODO: replace this line with your cleaning commands

fmt:
	@echo "you don't format assembly code"
	# TODO: replace this line with your formatting commands

lint:
	@echo "rly?"
	# TODO: replace this line with your linting commands

test: test-lexer test-ast test-parser
	@echo "===> Testing EVERYTHING"
	# TODO: replace this line with your testing commands

test-lexer: $(BUILD_DIR)/relexer
	@echo "===> Testing lexer"
	@./$(BUILD_DIR)/relexer

test-ast:
	@echo "===> Testing AST"
	# TODO: replace this line with your AST testing commands

test-parser:
	@echo "===> Testing parser"
	# TODO: replace this line with your parser testing commands

$(BUILD_DIR)/relexer: $(OBJ)
	@$(LD) $(LDFLAGS) $(OBJ) -o $(BUILD_DIR)/relexer

# compile asm to object
$(BUILD_DIR)/%.o: src/%.asm
	@mkdir -p $(BUILD_DIR)
	@$(AS) $(ASFLAGS) $< -o $@
