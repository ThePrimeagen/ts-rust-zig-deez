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
	# TODO: I don't know what I'm doing

lint:
	@echo "===> Linting"
	# TODO: I've never done this before

test:
	@echo "===> Testing"
	# TODO: What the hell is this
