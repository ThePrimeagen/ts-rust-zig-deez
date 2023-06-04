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

build:
	sbcl --non-interactive --load build.lisp

fmt:
	@echo "===> Formatting"
	# TODO: add yours

lint:
	@echo "===> Linting"
	# TODO: add yours

test:
	@echo "===> Testing"
	sbcl --non-interactive --load load.lisp --eval '(uiop:quit (if (asdf:test-system "deez") 0 1))'
