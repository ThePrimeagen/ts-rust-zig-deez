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
	dfmt -i source/*

lint:
	@echo "===> Linting"
	dscanner --syntaxCheck source/*
	dscanner --styleCheck source/*

test:
	@echo "===> Testing"
	dub test

