##
# Static part, don't change these:
##

help:
	@sed -r -e "s#^(\w+):.*#\1#p;d" < Makefile | sort

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update those:
##

BUILD := build

.PHONY: $(BUILD)

fmt:
	@echo "===> Formatting"
	# TODO: add yours

lint:
	@echo "===> Linting"
	# TODO: add yours

build:
	@echo "===> Building"
	@mkdir -p build
	@cmake -S . -B $(BUILD) -G Ninja
	@cmake --build $(BUILD)

test: build
	@echo "===> Testing"
	$(BUILD)/tests/tests

clean:
	@echo "===> Clean"
	@rm -fr $(BUILD)/