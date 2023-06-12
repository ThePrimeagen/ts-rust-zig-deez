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
	@echo "The formatting is whatever your heart desires"

lint:
	@echo "===> Linting"
	@echo "Linting deez"

test:
	@echo "===> Testing"
	swipl -g run_tests -t halt src/lexer.pl


