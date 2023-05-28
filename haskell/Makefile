.PHONY: help ready docker-build docker-ready fmt lint test clean

##
# Static part, don't change these:
##

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test clean

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update those:
##

fmt:
	@echo "===> Formatting"
	fourmolu --mode inplace .

lint:
	@echo "===> Linting"
	hlint .

test:
	@echo "===> Testing"
	cabal test

clean:
	@echo "===> Cleaning"
	cabal clean
