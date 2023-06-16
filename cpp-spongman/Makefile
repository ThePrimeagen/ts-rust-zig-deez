##
# Static part, don't change these:
##

help:
	@sed -r -e "s#^((\w|-)+):.*#\1#p;d" < Makefile | sort

ready: fmt lint test

docker-build:
	docker build . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -t -v $(shell pwd):/deez deez_$(notdir $(shell pwd))

docker-repl: docker-build
	docker run -it -v $(shell pwd):/deez deez_$(notdir $(shell pwd)) rlwrap ./bin/TsRustZigDeez -f prelude

docker-time: docker-build
	docker run -i -v $(shell pwd):/deez deez_$(notdir $(shell pwd)) time ./bin/TsRustZigDeez

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
	@cmake -S . -B $(BUILD) -G Ninja -DCMAKE_INSTALL_PREFIX=./dist -DCMAKE_BUILD_TYPE=Release # -DCMAKE_VERBOSE_MAKEFILE=ON
	@cmake --build $(BUILD)

install: build
	@cmake --install $(BUILD)

test: build
	@echo "===> Testing"
	@$(BUILD)/tests/tests

repl: build
	@echo "===> Repl"
	@rlwrap $(BUILD)/TsRustZigDeez -f prelude

time: build
	@echo "===> Timing"
	@time $(BUILD)/TsRustZigDeez -f prelude

valgrind: build
	@echo "===> Valgrind"
	@valgrind --tool=callgrind $(BUILD)/TsRustZigDeez -f prelude

clean:
	@echo "===> Clean"
	@rm -fr $(BUILD)/