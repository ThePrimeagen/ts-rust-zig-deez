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
	dfmt --brace_style=knr -i source/*

lint:
	@echo "===> Linting"
	dscanner --syntaxCheck --config dscanner.ini source/*
	# skipping due to broken unused parameter check for mixins
	#dscanner --styleCheck --config dscanner.ini source/*

test:
	@echo "===> Testing"
	DFLAGS="-L-L/usr/local/lib/" dub -v test

repl:
	@echo "===> Repl"
	DFLAGS="-L-L/usr/local/lib/" dub -b release

