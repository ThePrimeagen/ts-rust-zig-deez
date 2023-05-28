help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

fmt:
	@echo "===> Formatting"
	scala-cli fmt .

lint:
	@echo "===> Linting"
	# Just compile and see what warnings and errors the compiler prints for now
	scala-cli compile .

test:
	@echo "===> Testing"
	scala-cli test .

