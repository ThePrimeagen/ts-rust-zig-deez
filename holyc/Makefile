.PHONY: help docker-build docker-ready ready test

threedays ?= $(shell pwd)/3Days

help:
	@cat Makefile | grep -E "^\w+$:"

ready: test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd)/lexer:/deez/lexer -t deez_$(notdir $(shell pwd))

test:
	cd $(threedays) && ./3d_loader -t $(threedays)/T -c Run.HC
