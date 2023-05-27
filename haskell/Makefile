.PHONY: help ready docker-build docker-ready fmt lint test

##
# Static part, don't change these:
##

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -t deez_$(notdir $(shell pwd))

##
# Update those:
##

fmt:
	@echo "===> Formatting"
	# fourmolu --mode check src test app

lint:
	@echo "===> Linting"
	hlint src test app

test:
	@echo "===> Testing"
	cabal test
