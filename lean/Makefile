TESTS := $(shell find test -name '*.lean')

.PHONY: ready build run clean fmt lint test

ready: fmt lint test

build:
	lake build

run: build
	./build/bin/lean

clean:
	@echo "===> Cleaning..."
	lake clean

fmt:
	@echo "===> Not Formatting"

lint:
	@echo "===> Not Linting"

test: $(addsuffix .run, $(TESTS))

test/%.run: build
	@echo "===> Testing..."
	lake env lean test/$*
