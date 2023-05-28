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

DC:=dmd
DFILES:=$(shell find src/ -name "*.d" -type f)

fmt:
	@echo "===> Formatting"
	dfmt -i $(DFILES)

lint:
	@echo "===> Linting"
	dscanner -s $(DFILES)

test:
	@echo "===> Testing"
	$(DC) -main -unittest -Isrc/ $(DFILES)

build:
	@echo "===> Building"
	$(DC) -lib -of=deez $(DFILES)
