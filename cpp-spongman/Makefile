##
# Static part, don't change these:
##

help:
	@sed -r -e "s#^((\w|-)+):.*#\1#p;d" < Makefile | sort

ready: fmt lint test

docker-build:
	docker build . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

docker-repl: docker-build
	docker run -v $(shell pwd):/deez -i -t deez_$(notdir $(shell pwd)) rlwrap ./bin/TsRustZigDeez -f prelude

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
	@cmake -S . -B $(BUILD) -G Ninja -DCMAKE_INSTALL_PREFIX=./dist
	@cmake --build $(BUILD)

install: build
	@cmake --install $(BUILD)

test: build
	@echo "===> Testing"
	$(BUILD)/tests/tests

repl: build
	@echo "===> Testing"
	$(BUILD)/TsRustZigDeez -f prelude

clean:
	@echo "===> Clean"
	@rm -fr $(BUILD)/