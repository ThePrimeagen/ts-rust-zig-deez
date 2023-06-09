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

clean:
	@echo "===> Cleaning"
	rm -rf bin/

fmt:
	@echo "===> Formatting"
	crystal tool format

lint:
	@echo "===> Linting"
	bin/ameba

test:
	@echo "===> Testing EVERYTHING"
	crystal spec
